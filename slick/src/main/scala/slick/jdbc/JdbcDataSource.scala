package slick.jdbc

import java.io.Closeable
import java.util.Properties
import java.sql.{Connection, Driver, DriverManager, SQLException}
import javax.sql.DataSource

import com.typesafe.config.Config
import slick.util._
import slick.util.ConfigExtensionMethods._
import slick.SlickException

/** A `JdbcDataSource` provides a way to create a `Connection` object for a database. It is
  * similar to a `javax.sql.DataSource` but simpler. Unlike [[JdbcBackend.DatabaseDef]] it is not a
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
}

object JdbcDataSource extends Logging {
  /** Create a JdbcDataSource from a `Config`. See [[JdbcBackend.DatabaseFactoryDef.forConfig]]
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
  * [[JdbcBackend.DatabaseFactoryDef.forConfig]]. */
trait JdbcDataSourceFactory {
  def forConfig(c: Config, driver: Driver, name: String, classLoader: ClassLoader): JdbcDataSource
}

/** A JdbcDataSource for a `DataSource` */
class DataSourceJdbcDataSource(val ds: DataSource, val keepAliveConnection: Boolean,
                               val maxConnections: Option[Int],
                               val connectionPreparer: ConnectionPreparer = null) extends JdbcDataSource {
  private[this] var openedKeepAliveConnection: Connection = null

  def createConnection(): Connection = {
    if(keepAliveConnection) {
      synchronized {
        if(openedKeepAliveConnection eq null)
          openedKeepAliveConnection = ds.getConnection
      }
    }
    val c = ds.getConnection
    if(connectionPreparer ne null) connectionPreparer(c)
    c
  }

  def close(): Unit = {
    try if(keepAliveConnection && (openedKeepAliveConnection ne null)) openedKeepAliveConnection.close()
    finally ds match {
      case ds: Closeable => ds.close()
      case _ =>
    }
  }
}

object DataSourceJdbcDataSource extends JdbcDataSourceFactory {
  def forConfig(c: Config, driver: Driver, name: String, classLoader: ClassLoader): DataSourceJdbcDataSource = {
    val (ds, maxConnections) =
      c.getStringOpt("dataSourceClassName").orElse(c.getStringOpt("dataSourceClass")) match {

        case Some(dsClass) =>
          val propsO = c.getPropertiesOpt("properties")
          try {
            val ds = Class.forName(dsClass).newInstance.asInstanceOf[DataSource]
            propsO.foreach(BeanConfigurator.configure(ds, _))
            val maxConnections = c.getIntOpt("maxConnections")
            (ds, maxConnections)
          } catch { case ex: Exception => throw new SlickException("Error configuring DataSource " + dsClass, ex) }

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

/** A JdbcDataSource which can load a JDBC `Driver` from a class name */
@deprecated("Use DataSourceJdbcDataSource with DriverDataSource instead", "3.1")
trait DriverBasedJdbcDataSource extends JdbcDataSource {
  private[this] var registeredDriver: Driver = null

  protected[this] def registerDriver(driverName: String, url: String): Unit = if(driverName ne null) {
    val oldDriver = try DriverManager.getDriver(url) catch { case ex: SQLException if "08001" == ex.getSQLState => null }
    if(oldDriver eq null) {
      Class.forName(driverName)
      registeredDriver = DriverManager.getDriver(url)
    }
  }

  /** Deregister the JDBC driver if it was registered by this JdbcDataSource.
    * Returns true if an attempt was made to deregister a driver. */
  def deregisterDriver(): Boolean =
    if(registeredDriver ne null) { DriverManager.deregisterDriver(registeredDriver); true }
    else false

  val maxConnections: Option[Int] = None
}

/** A JdbcDataSource for lookup via a `Driver` or the `DriverManager` */
@deprecated("Use DataSourceJdbcDataSource with DriverDataSource instead", "3.1")
class DriverJdbcDataSource(url: String, user: String, password: String, prop: Properties,
                           driverName: String = null, driver: Driver = null,
                           connectionPreparer: ConnectionPreparer = null,
                           keepAliveConnection: Boolean = false) extends DriverBasedJdbcDataSource {
  private[this] var openedKeepAliveConnection: Connection = null

  if(driver eq null) registerDriver(driverName, url)

  val connectionProps = if(prop.ne(null) && user.eq(null) && password.eq(null)) prop else {
    val p = new Properties(prop)
    if(user ne null) p.setProperty("user", user)
    if(password ne null) p.setProperty("password", password)
    p
  }

  def createConnection(): Connection = {
    if(keepAliveConnection) {
      synchronized {
        if(openedKeepAliveConnection eq null)
          openedKeepAliveConnection = internalCreateConnection()
      }
    }
    internalCreateConnection()
  }

  protected[this] def internalCreateConnection(): Connection = {
    val conn = (if(driver eq null) DriverManager.getConnection(url, connectionProps)
    else {
      val conn = driver.connect(url, connectionProps)
      if(conn eq null)
        throw new SQLException("Driver " + driver + " does not know how to handle URL " + url, "08001")
      conn
    })
    if(connectionPreparer ne null) connectionPreparer(conn)
    conn
  }

  def close(): Unit = if(keepAliveConnection) {
    if(openedKeepAliveConnection ne null) openedKeepAliveConnection.close()
  }
}

@deprecated("Use DataSourceJdbcDataSource with DriverDataSource instead", "3.1")
object DriverJdbcDataSource extends JdbcDataSourceFactory {
  def forConfig(c: Config, driver: Driver, name: String, classLoader: ClassLoader): DriverJdbcDataSource = {
    val cp = new ConnectionPreparer(c)
    new DriverJdbcDataSource(
      c.getStringOr("url"),
      c.getStringOr("user"),
      c.getStringOr("password"),
      c.getPropertiesOr("properties"),
      c.getStringOr("driver", c.getStringOr("driverClassName")),
      driver,
      if(cp.isLive) cp else null,
      c.getBooleanOr("keepAliveConnection"))
  }
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
