package slick.jdbc.hikaricp

import java.sql.{Connection, Driver}

import com.typesafe.config.Config
import slick.jdbc.{JdbcDataSource, JdbcDataSourceFactory}
import slick.util.ConfigExtensionMethods._

/** A JdbcDataSource for a HikariCP connection pool.
  * See `slick.jdbc.JdbcBackend#Database.forConfig` for documentation on the config parameters. */
class HikariCPJdbcDataSource(val ds: com.zaxxer.hikari.HikariDataSource, val hconf: com.zaxxer.hikari.HikariConfig)
  extends JdbcDataSource {

  def createConnection(): Connection = ds.getConnection()

  def close(): Unit = ds.close()

  override val maxConnections: Option[Int] = Some(ds.getMaximumPoolSize)
}

object HikariCPJdbcDataSource extends JdbcDataSourceFactory {
  import com.zaxxer.hikari._

  def forConfig(c: Config, driver: Driver, name: String, classLoader: ClassLoader): HikariCPJdbcDataSource = {
    val hconf = new HikariConfig()

    // Essential settings

    // Use HikariCP `dataSourceClassName` as the main configuration and fallback to
    // `dataSourceClass`, `driverClassName` and finally `drive`.
    c.getStringOpt("dataSourceClassName").orElse(c.getStringOpt("dataSourceClass")) match {
      case Some(className) => hconf.setDataSourceClassName(className)
      case None => c.getStringOpt("driverClassName").orElse(c.getStringOpt("driver")).foreach(hconf.setDriverClassName)
    }

    // Use `jdbcUrl` an then `url` to configure the pool. According to HikariCP docs, when
    // using this property with "old" drivers, you may also need to set the driverClassName
    // property.
    c.getStringOpt("jdbcUrl").orElse(c.getStringOpt("url")).foreach(hconf.setJdbcUrl)
    c.getStringOpt("username").orElse(c.getStringOpt("user")).foreach(hconf.setUsername)
    c.getStringOpt("password").foreach(hconf.setPassword)

    c.getPropertiesOpt("properties").foreach(hconf.setDataSourceProperties)

    // Frequently used pool configuration
    c.getBooleanOpt("autoCommit").foreach(hconf.setAutoCommit)

    val numThreads = c.getIntOr("numThreads", 20)

    hconf.setConnectionTimeout(c.getMillisecondsOr("connectionTimeout", 1000))
    hconf.setIdleTimeout(c.getMillisecondsOr("idleTimeout", 600000))
    hconf.setMaxLifetime(c.getMillisecondsOr("maxLifetime", 1800000))
    c.getStringOpt("connectionTestQuery").foreach(hconf.setConnectionTestQuery)
    c.getStringOpt("connectionInitSql").foreach(hconf.setConnectionInitSql)
    hconf.setMaximumPoolSize(c.getIntOpt("maximumPoolSize").orElse(c.getIntOpt("maxConnections")).getOrElse(numThreads))
    hconf.setMinimumIdle(c.getIntOpt("minimumIdle").orElse(c.getIntOpt("minConnections")).getOrElse(numThreads))
    hconf.setPoolName(c.getStringOr("poolName", name))

    // Infrequently used

    // `initializationFailFast` is deprecated and should be replaced by
    // `initializationFailTimeout`. See HikariCP docs for more information:
    // https://github.com/brettwooldridge/HikariCP#infrequently-used
    // But we still respect the value if it configured.
    c.getBooleanOpt("initializationFailFast").foreach(hconf.setInitializationFailFast)

    // The default value for `initializationFailFast` was false, which means the pool
    // will not fail to start if there is a problem when connecting to the database.
    // To keep this behavior, we need to set `initializationFailTimeout` to -1 as
    // documented by HikariCP.
    hconf.setInitializationFailTimeout(c.getMillisecondsOr("initializationFailTimeout", -1))

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

    hconf.setValidationTimeout(c.getMillisecondsOr("validationTimeout", 1000))
    hconf.setLeakDetectionThreshold(c.getMillisecondsOr("leakDetectionThreshold", 0))

    c.getStringOpt("schema").foreach(hconf.setSchema)

    val ds = new HikariDataSource(hconf)
    new HikariCPJdbcDataSource(ds, hconf)
  }
}
