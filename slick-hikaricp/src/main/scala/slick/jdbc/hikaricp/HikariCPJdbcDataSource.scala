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
  * (e.g. `ds.setMetricRegistry`, `ds.setMetricsTrackerFactory` or `ds.setHealthCheckRegistry`), wrap
  * [[HikariCPJdbcDataSource.forConfig]] in your own [[slick.jdbc.JdbcDataSourceFactory]] object and
  * name that object in the `connectionPool` setting. */
class HikariCPJdbcDataSource(val ds: com.zaxxer.hikari.HikariDataSource, val hconf: com.zaxxer.hikari.HikariConfig)
  extends JdbcDataSource {

  def createConnection(): Connection = ds.getConnection()

  def close(): Unit = ds.close()

  override val maxConnections: Option[Int] = Some(ds.getMaximumPoolSize)
}

object HikariCPJdbcDataSource extends JdbcDataSourceFactory {
  import com.zaxxer.hikari._

  private lazy val logger = SlickLogger[HikariCPJdbcDataSource]

  /** Warnings about the pool settings in `c` that are deprecated or that conflict with each other. */
  private[hikaricp] def configWarnings(c: Config): Seq[String] = {
    val failFast = c.getBooleanOpt("initializationFailFast").map { failFast =>
      "The `initializationFailFast` setting is deprecated because HikariCP no longer supports it. Use " +
        "`initializationFailTimeout = 1` to fail fast, or `initializationFailTimeout = 0` or `-1` otherwise." +
        (if (c.hasPath("initializationFailTimeout")) " It is ignored because `initializationFailTimeout` is also set."
         else s" `initializationFailFast = $failFast` is treated as `initializationFailTimeout = ${if (failFast) 1 else 0}`.")
    }
    val poolSize = for {
      maximumPoolSize <- c.getIntOpt("maximumPoolSize")
      maxConnections <- c.getIntOpt("maxConnections")
    } yield
      s"Both `maximumPoolSize` ($maximumPoolSize) and `maxConnections` ($maxConnections) are set. The pool size is " +
        s"taken from `maximumPoolSize` and `maxConnections` is ignored for it. Set only one of them; a " +
        "`maximumPoolSize` can also come from a `reference.conf` file of a library."
    failFast.toSeq ++ poolSize
  }

  def forConfig(c: Config, driver: Driver, name: String, classLoader: ClassLoader): HikariCPJdbcDataSource = {
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

    // The default value for `initializationFailFast` was false, which means the pool
    // will not fail to start if there is a problem when connecting to the database.
    // To keep this behavior, we need to set `initializationFailTimeout` to -1 as
    // documented by HikariCP.
    hconf.setInitializationFailTimeout(c.getMillisecondsOr("initializationFailTimeout", default = {
      // `initializationFailFast` is deprecated and should be replaced by
      // `initializationFailTimeout`. See HikariCP docs for more information:
      // https://github.com/brettwooldridge/HikariCP#infrequently-used
      // But for backwards compatibility we check for it if initializationFailTimeout is missing
      c.getBooleanOpt("initializationFailFast").map { failFast =>
        if (failFast) 1L
        else 0L
      }.getOrElse(-1L)
    }))

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

    val ds = new HikariDataSource(hconf)
    new HikariCPJdbcDataSource(ds, hconf)
  }
}
