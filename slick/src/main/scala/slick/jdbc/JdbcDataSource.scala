package slick.jdbc

import java.io.Closeable
import java.lang.reflect.{InvocationHandler, InvocationTargetException, Method, Proxy}
import java.sql.{Connection, Driver}

import slick.util.*
import slick.util.ConfigExtensionMethods.*
import slick.SlickException

import com.typesafe.config.Config
import javax.sql.DataSource

/** A `JdbcDataSource` provides a way to create a `Connection` object for a database. It is
  * similar to a `javax.sql.DataSource` but simpler. Unlike [[JdbcBackend.JdbcDatabaseDef]] it is not a
  * part of the backend cake. This trait defines the SPI for 3rd-party connection pool support. */
trait JdbcDataSource extends Closeable {

  /** Create a new Connection or get one from the pool */
  def createConnection(): Connection

  /** If this object represents a connection pool managed directly by Slick, close it.
    * Otherwise no action is taken. */
  def close(): Unit

  /** If this object represents a connection pool with a limited size, return the maximum pool size.
    * Otherwise return None. This is required to prevent deadlocks when scheduling database actions.
    */
  val maxConnections: Option[Int]

  /** Whether a transaction that is already open on a connection from this source (because the
    * connection arrives with auto-commit disabled) belongs to whoever supplied the connection.
    *
    * If true, Slick runs actions inside that transaction and never commits it, rolls it back or
    * changes its auto-commit mode or isolation level: `transactionally` scopes nest inside it. If
    * false, Slick treats an open transaction as its own, and the outermost `transactionally` scope
    * commits or rolls it back. A connection that arrives with auto-commit enabled has no open
    * transaction, so this setting does not affect it. */
  def openTransactionsOwnedExternally: Boolean = false
}

object JdbcDataSource extends Logging {
  /** Create a JdbcDataSource from a `Config`. See `slick.jdbc.DatabaseConfig`
    * for documentation of the supported configuration parameters. */
  def forConfig(c: Config, driver: Driver, name: String, classLoader: ClassLoader): JdbcDataSource = {
    def loadFactory(name: String): JdbcDataSourceFactory = {
      val clazz = classLoader.loadClass(name)
      clazz.getField("MODULE$").get(clazz).asInstanceOf[JdbcDataSourceFactory]
    }
    val pf: JdbcDataSourceFactory = c.getStringOr("connectionPool", "HikariCP") match {
      case "disabled" => DataSourceJdbcDataSource
      case "HikariCP" => loadFactory("slick.jdbc.hikaricp.HikariCPJdbcDataSource$")
      case "slick.jdbc.HikariCPJdbcDataSource" =>
        logger.warn("connectionPool class 'slick.jdbc.HikariCPJdbcDataSource$' has been renamed to 'slick.jdbc.hikaricp.HikariCPJdbcDataSource$'")
        loadFactory("slick.jdbc.hikaricp.HikariCPJdbcDataSource$")
      case name => loadFactory(name)
    }
    pf.forConfig(c, driver, name, classLoader)
  }
}

/** Create a [[JdbcDataSource]] from a `Config` object and an optional JDBC `Driver`.
  * This is used with the "connectionPool" configuration option in
  * `slick.jdbc.DatabaseConfig`. */
trait JdbcDataSourceFactory {
  def forConfig(c: Config, driver: Driver, name: String, classLoader: ClassLoader): JdbcDataSource
}

/** A JdbcDataSource for a `DataSource` */
class DataSourceJdbcDataSource(val ds: DataSource, val keepAliveConnection: Boolean,
                               val maxConnections: Option[Int],
                               val connectionPreparer: ConnectionPreparer = null) extends JdbcDataSource {
  private[this] var openedKeepAliveConnection: Connection = null

  /** Open the keepalive connection eagerly, if `keepAliveConnection` is true and it has not been
    * opened yet. For named in-memory databases (e.g. H2 named databases) the database is only
    * kept alive as long as at least one connection is open. Calling this method in the
    * `Resource.make` acquire (before any DBIO actions run) ensures the database survives the
    * period between pool construction and the first actual query. */
  def openKeepAlive(): Unit = if (keepAliveConnection) {
    synchronized {
      if (openedKeepAliveConnection eq null)
        openedKeepAliveConnection = ds.getConnection
    }
  }

  def createConnection(): Connection = {
    val c = ds.getConnection
    if(connectionPreparer ne null) connectionPreparer(c)
    c
  }

  def close(): Unit = synchronized {
    try if(keepAliveConnection && (openedKeepAliveConnection ne null)) {
      val c = openedKeepAliveConnection
      openedKeepAliveConnection = null
      c.close()
    }
    finally ds match {
      case ds: Closeable => ds.close()
      case _ =>
    }
  }
}

object DataSourceJdbcDataSource extends JdbcDataSourceFactory {
  /** The configured DataSource class name together with the key it was set under
    * (`dataSourceClassName`, falling back to `dataSourceClass`), if any. */
  private[jdbc] def dataSourceClassSetting(c: Config): Option[(String, String)] =
    c.getStringOpt("dataSourceClassName").map(("dataSourceClassName", _))
      .orElse(c.getStringOpt("dataSourceClass").map(("dataSourceClass", _)))

  /** Fail with a clear message if the class configured under `key` is not a `javax.sql.DataSource`,
    * for example when a Slick profile or a `java.sql.Driver` was configured there by mistake. */
  private[jdbc] def requireDataSourceClass(key: String, cls: Class[?]): Unit =
    if (!classOf[DataSource].isAssignableFrom(cls))
      throw new SlickException(
        s"The `$key` setting must name a javax.sql.DataSource class provided by the JDBC driver, but ${cls.getName} " +
          "is not a DataSource. Configure the Slick profile with `profile` and a java.sql.Driver class with `driver`.")

  def forConfig(c: Config, driver: Driver, name: String, classLoader: ClassLoader): DataSourceJdbcDataSource = {
    val (ds, maxConnections) =
      dataSourceClassSetting(c) match {

        case Some((key, dsClass)) =>
          val propsO = c.getPropertiesOpt("properties")
          def error(ex: Exception) = new SlickException("Error configuring DataSource " + dsClass, ex)
          val cl = if (classLoader eq null) getClass.getClassLoader else classLoader
          val cls = try Class.forName(dsClass, true, cl) catch { case ex: Exception => throw error(ex) }
          requireDataSourceClass(key, cls)
          try {
            val ds = cls.getConstructor().newInstance().asInstanceOf[DataSource]
            propsO.foreach(BeanConfigurator.configure(ds, _))
            val maxConnections = c.getIntOpt("maxConnections")
            (ds, maxConnections)
          } catch { case ex: Exception => throw error(ex) }

        case None =>
          val ds = new DriverDataSource
          ds.classLoader = classLoader
          ds.driverObject = driver
          BeanConfigurator.configure(ds, c.toProperties, Set("url", "user", "password", "properties", "driver", "driverClassName"))
          (ds, None)
      }
    new DataSourceJdbcDataSource(ds, c.getBooleanOr("keepAliveConnection"), maxConnections, new ConnectionPreparer(c))
  }
}

/** A [[JdbcDataSource]] for a single connection that its caller opened and will close, such as the
  * connection a migration tool hands to a migration.
  *
  * Slick runs actions on that connection one at a time and never closes it. If the connection has
  * auto-commit disabled, its open transaction belongs to the caller (see
  * [[JdbcDataSource.openTransactionsOwnedExternally]]), so everything Slick does on it commits or rolls
  * back with that transaction. The caller must not use the connection while Slick is using it. */
class CallerOwnedConnectionJdbcDataSource(conn: Connection) extends JdbcDataSource {
  // Like a pooled connection, the handed-out connection's close() releases it rather than closing
  // the physical connection; here releasing it does nothing, because the caller closes `conn`.
  private[this] val unclosableConn: Connection =
    Proxy.newProxyInstance(classOf[Connection].getClassLoader, Array(classOf[Connection]), new InvocationHandler {
      def invoke(proxy: AnyRef, method: Method, args: Array[AnyRef]): AnyRef =
        if (method.getName == "close" && method.getParameterCount == 0) null
        else
          try method.invoke(conn, (if (args eq null) Array.empty[AnyRef] else args)*)
          catch { case e: InvocationTargetException => throw e.getCause }
    }).asInstanceOf[Connection]

  def createConnection(): Connection = unclosableConn
  def close(): Unit = ()
  val maxConnections: Option[Int] = Some(1)
  override def openTransactionsOwnedExternally: Boolean = true
}

/** Set parameters on a new Connection. This is used by [[DataSourceJdbcDataSource]]. */
class ConnectionPreparer(c: Config) extends (Connection => Unit) {
  val isolation = c.getStringOpt("isolation").map {
    case "NONE" => Connection.TRANSACTION_NONE
    case "READ_COMMITTED" => Connection.TRANSACTION_READ_COMMITTED
    case "READ_UNCOMMITTED" => Connection.TRANSACTION_READ_UNCOMMITTED
    case "REPEATABLE_READ" => Connection.TRANSACTION_REPEATABLE_READ
    case "SERIALIZABLE" => Connection.TRANSACTION_SERIALIZABLE
    case unknown => throw new SlickException(s"Unknown transaction isolation level [$unknown]")
  }
  val catalog = c.getStringOpt("catalog").orElse(c.getStringOpt("defaultCatalog"))
  val readOnly = c.getBooleanOpt("readOnly")

  val isLive = isolation.isDefined || catalog.isDefined || readOnly.isDefined

  def apply(c: Connection): Unit = if(isLive) {
    isolation.foreach(c.setTransactionIsolation)
    readOnly.foreach(c.setReadOnly)
    catalog.foreach(c.setCatalog)
  }
}
