package slick.jdbc.hikaricp

import java.sql.{Connection, Driver}

import com.typesafe.config.Config
import slick.jdbc.{DataSourceJdbcDataSource, JdbcDataSource, JdbcDataSourceFactory}
import slick.util.SlickLogger
import slick.util.ConfigExtensionMethods._

/** A JdbcDataSource for a HikariCP connection pool.
  * See the "Connection pools" section of the Slick manual for documentation on the config parameters.
  *
  * `ds` is the running HikariCP pool and `hconf` its configuration. To register metrics or health checks
  * (e.g. `setMetricRegistry`, `setMetricsTrackerFactory` or `setHealthCheckRegistry`), write your own
  * [[slick.jdbc.JdbcDataSourceFactory]] object that gets the configuration from
  * [[HikariCPJdbcDataSource.hikariConfig]], sets them on it and then creates the pool, and name that object in
  * the `connectionPool` setting. HikariCP does not allow a health check registry to be set on a running pool. */
class HikariCPJdbcDataSource(val ds: com.zaxxer.hikari.HikariDataSource, val hconf: com.zaxxer.hikari.HikariConfig)
  extends JdbcDataSource {

  def createConnection(): Connection = ds.getConnection()

  def close(): Unit = ds.close()

  override val maxConnections: Option[Int] = Some(ds.getMaximumPoolSize)
}

object HikariCPJdbcDataSource extends JdbcDataSourceFactory {
  import com.zaxxer.hikari._

  private lazy val logger = SlickLogger[HikariCPJdbcDataSource]

  /** Warnings about the pool settings in `c` that conflict with each other. */
  private[hikaricp] def configWarnings(c: Config): Seq[String] =
    (for {
      maximumPoolSize <- c.getIntOpt("maximumPoolSize")
      maxConnections <- c.getIntOpt("maxConnections")
      if maximumPoolSize != maxConnections
    } yield
      s"Both `maximumPoolSize` ($maximumPoolSize) and `maxConnections` ($maxConnections) are set. The pool size is " +
        s"taken from `maximumPoolSize` and `maxConnections` is ignored for it. Set only one of them; a " +
        "`maximumPoolSize` can also come from a `reference.conf` file of a library.").toSeq

  def forConfig(c: Config, driver: Driver, name: String, classLoader: ClassLoader): HikariCPJdbcDataSource = {
    val hconf = hikariConfig(c, name, classLoader)
    new HikariCPJdbcDataSource(new HikariDataSource(hconf), hconf)
  }

  /** The HikariCP configuration for the pool settings in `c`, as used by [[forConfig]]. Use this in your own
    * [[slick.jdbc.JdbcDataSourceFactory]] to change the configuration before the pool is created. */
  def hikariConfig(c: Config, name: String, classLoader: ClassLoader): HikariConfig = {
    configWarnings(c).foreach(logger.warn(_))

    val hconf = new HikariConfig()

    // Essential settings

    // Use HikariCP `dataSourceClassName` as the main configuration and fallback to
    // `dataSourceClass`, `driverClassName` and finally `driver`.
    DataSourceJdbcDataSource.dataSourceClassSetting(c) match {
      case Some((key, className)) =>
        // HikariCP fails with an opaque error if the class is not a DataSource, so check it here.
        // If the class cannot be loaded, leave it to HikariCP to report that.
        val cl = if (classLoader eq null) getClass.getClassLoader else classLoader
        try DataSourceJdbcDataSource.requireDataSourceClass(key, cl.loadClass(className))
        catch { case _: ClassNotFoundException => }
        hconf.setDataSourceClassName(className)
      case None => c.getStringOpt("driverClassName").orElse(c.getStringOpt("driver")).foreach(hconf.setDriverClassName)
    }

    // Use `jdbcUrl` and then `url` to configure the pool. According to HikariCP docs, when
    // using this property with "old" drivers, you may also need to set the driverClassName
    // property.
    c.getStringOpt("jdbcUrl").orElse(c.getStringOpt("url")).foreach(hconf.setJdbcUrl)
    c.getStringOpt("username").orElse(c.getStringOpt("user")).foreach(hconf.setUsername)
    c.getStringOpt("password").foreach(hconf.setPassword)

    c.getPropertiesOpt("properties").foreach(hconf.setDataSourceProperties)

    // Frequently used pool configuration
    c.getBooleanOpt("autoCommit").foreach(hconf.setAutoCommit)
    c.getStringOpt("exceptionOverrideClassName").foreach(hconf.setExceptionOverrideClassName)

    val defaultPoolSize = c.getIntOpt("maxConnections").orElse(c.getIntOpt("numThreads")).getOrElse(20)

    hconf.setConnectionTimeout(c.getMillisecondsOr("connectionTimeout", 30000))
    hconf.setIdleTimeout(c.getMillisecondsOr("idleTimeout", 600000))
    hconf.setMaxLifetime(c.getMillisecondsOr("maxLifetime", 1800000))
    c.getStringOpt("connectionTestQuery").foreach(hconf.setConnectionTestQuery)
    c.getStringOpt("connectionInitSql").foreach(hconf.setConnectionInitSql)
    hconf.setMaximumPoolSize(c.getIntOpt("maximumPoolSize").orElse(c.getIntOpt("maxConnections")).getOrElse(defaultPoolSize))
    hconf.setMinimumIdle(c.getIntOpt("minimumIdle").orElse(c.getIntOpt("minConnections")).getOrElse(defaultPoolSize))
    hconf.setPoolName(c.getStringOr("poolName", name))
    c.getLongOpt("keepAliveTime").foreach(hconf.setKeepaliveTime)

    // Infrequently used

    // Slick's default is -1 instead of HikariCP's 1, so the pool starts even if it cannot
    // connect to the database.
    hconf.setInitializationFailTimeout(c.getMillisecondsOr("initializationFailTimeout", -1L))

    c.getBooleanOpt("isolateInternalQueries").foreach(hconf.setIsolateInternalQueries)
    c.getBooleanOpt("allowPoolSuspension").foreach(hconf.setAllowPoolSuspension)
    c.getBooleanOpt("readOnly").foreach(hconf.setReadOnly)
    c.getBooleanOpt("registerMbeans").foreach(hconf.setRegisterMbeans)
    c.getStringOpt("catalog").foreach(hconf.setCatalog)
    c.getStringOpt("connectionInitSql").foreach(hconf.setConnectionInitSql)
    c.getStringOpt("transactionIsolation")
      .orElse(c.getStringOpt("isolation"))
      .map("TRANSACTION_" + _)
      .foreach(hconf.setTransactionIsolation)

    hconf.setValidationTimeout(c.getMillisecondsOr("validationTimeout", 5000))
    hconf.setLeakDetectionThreshold(c.getMillisecondsOr("leakDetectionThreshold", 0))

    c.getStringOpt("schema").foreach(hconf.setSchema)

    hconf
  }
}
