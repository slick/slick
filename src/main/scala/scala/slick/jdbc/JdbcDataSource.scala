package scala.slick.jdbc

import java.io.Closeable
import java.util.Properties
import java.util.concurrent.TimeUnit
import java.sql.{SQLException, DriverManager, Driver, Connection}
import javax.sql.DataSource
import com.typesafe.config.Config
import scala.slick.util.ConfigExtensionMethods._
import scala.slick.SlickException

/** A `JdbcDataSource` provides a way to create a `Connection` object for a database. It is
  * similar to a `javax.sql.DataSource` but simpler. Unlike [[JdbcBackend.DatabaseDef]] it is not a
  * part of the backend cake. This trait defines the SPI for 3rd-party connection pool support. */
trait JdbcDataSource extends Closeable {
  /** Create a new Connection or get one from the pool */
  def createConnection(): Connection

  /** If this object represents a connection pool managed directly by Slick, close it.
    * Otherwise no action is taken. */
  def close(): Unit

  /** Return the maximum number of concurrent connections that can be used with this data source,
    * or -1 if the number is unknown. */
  def maxConnections: Int
}

object JdbcDataSource {
  /** Create a JdbcDataSource from a `Config`. See [[JdbcBackend.DatabaseFactoryDef.forConfig]]
    * for documentation of the supported configuration parameters. */
  def forConfig(c: Config, driver: Driver, name: String): JdbcDataSource = {
    val pf: JdbcDataSourceFactory = c.getStringOr("pool") match {
      case null => DriverJdbcDataSource
      case "HikariCP" => HikariCPJdbcDataSource
      case "BoneCP" => BoneCPJdbcDataSource
      case name =>
        val clazz = Class.forName(name)
        clazz.getField("MODULE$").get(clazz).asInstanceOf[JdbcDataSourceFactory]
    }
    pf.forConfig(c, driver, name)
  }
}

/** Create a [[JdbcDataSource]] from a `Config` object and an optional JDBC `Driver`.
  * This is used with the "pool" configuration option in [[JdbcBackend.DatabaseFactoryDef.forConfig]]. */
trait JdbcDataSourceFactory {
  def forConfig(c: Config, driver: Driver, name: String): JdbcDataSource
}

/** A JdbcDataSource for a `DataSource` */
class DataSourceJdbcDataSource(val ds: DataSource) extends JdbcDataSource {
  def createConnection(): Connection = ds.getConnection
  def close(): Unit = ()
  def maxConnections = -1
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
                           connectionPreparer: ConnectionPreparer = null) extends DriverBasedJdbcDataSource {
  registerDriver(driverName, url)

  val connectionProps = if(prop.ne(null) && user.eq(null) && password.eq(null)) prop else {
    val p = new Properties(prop)
    if(user ne null) p.setProperty("user", user)
    if(password ne null) p.setProperty("password", password)
    p
  }

  def createConnection(): Connection = {
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

  def close(): Unit = ()

  def maxConnections = -1
}

object DriverJdbcDataSource extends JdbcDataSourceFactory {
  def forConfig(c: Config, driver: Driver, name: String): DriverJdbcDataSource = {
    val cp = new ConnectionPreparer(c)
    new DriverJdbcDataSource(c.getString("url"), c.getStringOr("user"), c.getStringOr("password"),
      c.getPropertiesOr("properties"), c.getStringOr("driver"), driver, if(cp.isLive) cp else null)
  }
}

/** A JdbcDataSource for a HikariCP connection pool */
class HikariCPJdbcDataSource(val ds: com.zaxxer.hikari.HikariDataSource, val hconf: com.zaxxer.hikari.HikariConfig) extends JdbcDataSource {
  def createConnection(): Connection = ds.getConnection()
  def close(): Unit = ds.close()
  def maxConnections = hconf.getMaximumPoolSize
}

object HikariCPJdbcDataSource extends JdbcDataSourceFactory {
  import com.zaxxer.hikari._

  def forConfig(c: Config, driver: Driver, name: String): HikariCPJdbcDataSource = {
    if(driver ne null)
      throw new SlickException("An explicit Driver object is not supported by HikariCPJdbcDataSource")
    val hconf = new HikariConfig()

    // Connection settings
    hconf.setDataSourceClassName(c.getStringOr("dataSourceClassName", null))
    hconf.setDriverClassName(c.getStringOr("driverClassName", c.getStringOr("driver")))
    hconf.setJdbcUrl(c.getStringOr("jdbcUrl", c.getStringOr("url", null)))
    c.getStringOpt("username").orElse(c.getStringOpt("user")).foreach(hconf.setUsername)
    c.getStringOpt("password").foreach(hconf.setPassword)
    c.getPropertiesOpt("dataSource").orElse(c.getPropertiesOpt("properties")).foreach(hconf.setDataSourceProperties)

    // Pool configuration
    hconf.setConnectionTimeout(c.getMillisecondsOr("connectionTimeout", 30000))
    hconf.setIdleTimeout(c.getMillisecondsOr("idleTimeout", c.getMillisecondsOr("idleMaxAge", 600000)))
    hconf.setMaxLifetime(c.getMillisecondsOr("maxLifetime", c.getMillisecondsOr("maxConnectionAge", 1800000)))
    hconf.setLeakDetectionThreshold(c.getMillisecondsOr("leakDetectionThreshold", 0))
    hconf.setInitializationFailFast(c.getBooleanOr("initializationFailFast", false))
    hconf.setJdbc4ConnectionTest(c.getBooleanOr("jdbc4ConnectionTest", true))
    c.getStringOpt("connectionTestQuery").orElse(c.getStringOpt("connectionTestStatement")).foreach(hconf.setConnectionTestQuery)
    c.getStringOpt("connectionInitSql").orElse(c.getStringOpt("initSQL")).foreach(hconf.setConnectionInitSql)
    val compatMaxPoolSize = c.getIntOpt("maxConnectionsPerPartition").map(_ * c.getIntOr("partitionCount", 1))
    hconf.setMaximumPoolSize(c.getIntOr("maximumPoolSize", compatMaxPoolSize.getOrElse(10)))
    val compatMinPoolSize = c.getIntOpt("minConnectionsPerPartition").map(_ * c.getIntOr("partitionCount", 1))
    hconf.setMinimumIdle(c.getIntOr("minimumIdle", compatMinPoolSize.getOrElse(hconf.getMaximumPoolSize)))
    c.getStringOpt("poolName").orElse(Option(name)).foreach(hconf.setPoolName)
    hconf.setRegisterMbeans(c.getBooleanOr("registerMbeans", !c.getBooleanOr("disableJMX", true)))
    hconf.setIsolateInternalQueries(c.getBooleanOr("isolateInternalQueries", false))

    // Equivalent of ConnectionPreparer
    hconf.setAutoCommit(c.getBooleanOr("autoCommit", c.getBooleanOr("autocommit", true)))
    hconf.setReadOnly(c.getBooleanOr("readOnly", false))
    c.getStringOpt("transactionIsolation").orElse(c.getStringOpt("isolation")).map {
      case "READ_COMMITTED" => "TRANSACTION_READ_COMMITTED"
      case "READ_UNCOMMITTED" => "TRANSACTION_READ_UNCOMMITTED"
      case "REPEATABLE_READ" => "TRANSACTION_REPEATABLE_READ"
      case "SERIALIZABLE" => "TRANSACTION_SERIALIZABLE"
      case s => s
    }.foreach(hconf.setTransactionIsolation)
    hconf.setCatalog(c.getStringOr("catalog", c.getStringOr("defaultCatalog", null)))

    val ds = new HikariDataSource(hconf)
    new HikariCPJdbcDataSource(ds, hconf)
  }
}

/** A JdbcDataSource for a BoneCP connection pool */
class BoneCPJdbcDataSource(val ds: com.jolbox.bonecp.BoneCPDataSource, driverName: String) extends DriverBasedJdbcDataSource {
  registerDriver(driverName, ds.getJdbcUrl)

  def createConnection(): Connection = ds.getConnection()
  def close(): Unit = ds.close()
  def maxConnections = ds.getPartitionCount * ds.getMaxConnectionsPerPartition
}

object BoneCPJdbcDataSource extends JdbcDataSourceFactory {
  import com.jolbox.bonecp._
  import com.jolbox.bonecp.hooks._

  def forConfig(c: Config, driver: Driver, name: String): BoneCPJdbcDataSource = {
    if(driver ne null)
      throw new SlickException("An explicit Driver object is not supported by BoneCPJdbcDataSource")
    val ds = new BoneCPDataSource

    // Connection settings
    ds.setJdbcUrl(c.getString("url"))
    c.getStringOpt("user").foreach(ds.setUsername)
    c.getStringOpt("password").foreach(ds.setPassword)
    c.getPropertiesOpt("properties").foreach(ds.setDriverProperties)

    // Pool configuration
    ds.setPartitionCount(c.getIntOr("partitionCount", 1))
    ds.setMaxConnectionsPerPartition(c.getIntOr("maxConnectionsPerPartition", 30))
    ds.setMinConnectionsPerPartition(c.getIntOr("minConnectionsPerPartition", 5))
    ds.setAcquireIncrement(c.getIntOr("acquireIncrement", 1))
    ds.setAcquireRetryAttempts(c.getIntOr("acquireRetryAttempts", 10))
    ds.setAcquireRetryDelayInMs(c.getMillisecondsOr("acquireRetryDelay", 1000))
    ds.setConnectionTimeoutInMs(c.getMillisecondsOr("connectionTimeout", 1000))
    ds.setIdleMaxAge(c.getMillisecondsOr("idleMaxAge", 1000 * 60 * 10), TimeUnit.MILLISECONDS)
    ds.setMaxConnectionAge(c.getMillisecondsOr("maxConnectionAge", 1000 * 60 * 60), TimeUnit.MILLISECONDS)
    ds.setDisableJMX(c.getBooleanOr("disableJMX", true))
    ds.setStatisticsEnabled(c.getBooleanOr("statisticsEnabled"))
    ds.setIdleConnectionTestPeriod(c.getMillisecondsOr("idleConnectionTestPeriod", 1000 * 60), TimeUnit.MILLISECONDS)
    ds.setDisableConnectionTracking(c.getBooleanOr("disableConnectionTracking", true))
    ds.setQueryExecuteTimeLimitInMs(c.getMillisecondsOr("queryExecuteTimeLimit"))
    c.getStringOpt("initSQL").foreach(ds.setInitSQL)
    ds.setLogStatementsEnabled(c.getBooleanOr("logStatements"))
    c.getStringOpt("connectionTestStatement").foreach(ds.setConnectionTestStatement)

    // Connection preparer
    val cp = new ConnectionPreparer(c)
    if(cp.isLive) ds.setConnectionHook(new AbstractConnectionHook {
      override def onCheckOut(conn: ConnectionHandle) = cp(conn)
    })

    new BoneCPJdbcDataSource(ds, c.getStringOr("driver"))
  }
}

/** Set parameters on a new Connection. This is used by [[DriverJdbcDataSource]]
  * and [[BoneCPJdbcDataSource]]. */
class ConnectionPreparer(c: Config) extends (Connection => Unit) {
  val autocommit = c.getBooleanOpt("autocommit")
  val isolation = c.getStringOpt("isolation").map {
    case "NONE" => Connection.TRANSACTION_NONE
    case "READ_COMMITTED" => Connection.TRANSACTION_READ_COMMITTED
    case "READ_UNCOMMITTED" => Connection.TRANSACTION_READ_UNCOMMITTED
    case "REPEATABLE_READ" => Connection.TRANSACTION_REPEATABLE_READ
    case "SERIALIZABLE" => Connection.TRANSACTION_SERIALIZABLE
    case unknown => throw new SlickException(s"Unknown isolation level [$unknown]")
  }
  val catalog = c.getStringOpt("defaultCatalog")
  val readOnly = c.getBooleanOpt("readOnly")

  val isLive = autocommit.isDefined || isolation.isDefined || catalog.isDefined || readOnly.isDefined

  def apply(c: Connection): Unit = if(isLive) {
    autocommit.foreach(c.setAutoCommit)
    isolation.foreach(c.setTransactionIsolation)
    readOnly.foreach(c.setReadOnly)
    catalog.foreach(c.setCatalog)
  }
}
