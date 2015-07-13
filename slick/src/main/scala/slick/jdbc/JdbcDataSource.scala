package slick.jdbc

import java.io.Closeable
import java.util.Properties
import java.util.concurrent.TimeUnit
import java.sql.{SQLException, DriverManager, Driver, Connection}
import javax.sql.DataSource
import com.typesafe.config.Config
import slick.util.ClassLoaderUtil
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
}

object JdbcDataSource {
  /** Create a JdbcDataSource from a `Config`. See [[JdbcBackend.DatabaseFactoryDef.forConfig]]
    * for documentation of the supported configuration parameters. */
  def forConfig(c: Config, driver: Driver, name: String): JdbcDataSource = forConfig(c, driver, name, ClassLoaderUtil.defaultClassLoader)

  /** Create a JdbcDataSource from a `Config`. See [[JdbcBackend.DatabaseFactoryDef.forConfig]]
    * for documentation of the supported configuration parameters. */
  def forConfig(c: Config, driver: Driver, name: String, classLoader: ClassLoader): JdbcDataSource = {
    val pf: JdbcDataSourceFactory = c.getStringOr("connectionPool", "HikariCP") match {
      case "disabled" => DriverJdbcDataSource
      case "HikariCP" => HikariCPJdbcDataSource
      case name =>
        val clazz = classLoader.loadClass(name)
        clazz.getField("MODULE$").get(clazz).asInstanceOf[JdbcDataSourceFactory]
    }
    pf.forConfig(c, driver, name)
  }
}

/** Create a [[JdbcDataSource]] from a `Config` object and an optional JDBC `Driver`.
  * This is used with the "connectionPool" configuration option in
  * [[JdbcBackend.DatabaseFactoryDef.forConfig]]. */
trait JdbcDataSourceFactory {
  def forConfig(c: Config, driver: Driver, name: String): JdbcDataSource
}

/** A JdbcDataSource for a `DataSource` */
class DataSourceJdbcDataSource(val ds: DataSource) extends JdbcDataSource {
  def createConnection(): Connection = ds.getConnection
  def close(): Unit = ()
}

/** A JdbcDataSource which can load a JDBC `Driver` from a class name */
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
}

/** A JdbcDataSource for lookup via a `Driver` or the `DriverManager` */
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

object DriverJdbcDataSource extends JdbcDataSourceFactory {
  def forConfig(c: Config, driver: Driver, name: String): DriverJdbcDataSource = {
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

/** A JdbcDataSource for a HikariCP connection pool */
class HikariCPJdbcDataSource(val ds: com.zaxxer.hikari.HikariDataSource, val hconf: com.zaxxer.hikari.HikariConfig) extends JdbcDataSource {
  def createConnection(): Connection = ds.getConnection()
  def close(): Unit = ds.close()
}

object HikariCPJdbcDataSource extends JdbcDataSourceFactory {
  import com.zaxxer.hikari._

  def forConfig(c: Config, driver: Driver, name: String): HikariCPJdbcDataSource = {
    if(driver ne null)
      throw new SlickException("An explicit Driver object is not supported by HikariCPJdbcDataSource")
    val hconf = new HikariConfig()

    // Connection settings
    hconf.setDataSourceClassName(c.getStringOr("dataSourceClass", null))
    Option(c.getStringOr("driverClassName", c.getStringOr("driver"))).map(hconf.setDriverClassName _)
    hconf.setJdbcUrl(c.getStringOr("url", null))
    c.getStringOpt("user").foreach(hconf.setUsername)
    c.getStringOpt("password").foreach(hconf.setPassword)
    c.getPropertiesOpt("properties").foreach(hconf.setDataSourceProperties)

    // Pool configuration
    hconf.setConnectionTimeout(c.getMillisecondsOr("connectionTimeout", 1000))
    hconf.setIdleTimeout(c.getMillisecondsOr("idleTimeout", 600000))
    hconf.setMaxLifetime(c.getMillisecondsOr("maxLifetime", 1800000))
    hconf.setLeakDetectionThreshold(c.getMillisecondsOr("leakDetectionThreshold", 0))
    hconf.setInitializationFailFast(c.getBooleanOr("initializationFailFast", false))
    c.getStringOpt("connectionTestQuery").foreach { s =>
      hconf.setJdbc4ConnectionTest(false)
      hconf.setConnectionTestQuery(s)
    }
    c.getStringOpt("connectionInitSql").foreach(hconf.setConnectionInitSql)
    val numThreads = c.getIntOr("numThreads", 20)
    hconf.setMaximumPoolSize(c.getIntOr("maxConnections", numThreads * 5))
    hconf.setMinimumIdle(c.getIntOr("minConnections", numThreads))
    hconf.setPoolName(name)
    hconf.setRegisterMbeans(c.getBooleanOr("registerMbeans", false))

    // Equivalent of ConnectionPreparer
    hconf.setReadOnly(c.getBooleanOr("readOnly", false))
    c.getStringOpt("isolation").map("TRANSACTION_" + _).foreach(hconf.setTransactionIsolation)
    hconf.setCatalog(c.getStringOr("catalog", null))

    val ds = new HikariDataSource(hconf)
    new HikariCPJdbcDataSource(ds, hconf)
  }
}

/** Set parameters on a new Connection. This is used by [[DriverJdbcDataSource]]. */
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
