package slick.test.jdbc.hikaricp

import java.sql.Driver
import java.util.concurrent.atomic.AtomicInteger

import scala.jdk.CollectionConverters.*

import cats.effect.unsafe.implicits.global

import ch.qos.logback.classic.{Level, Logger}
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.read.ListAppender
import com.codahale.metrics.MetricRegistry
import com.codahale.metrics.health.HealthCheckRegistry
import com.typesafe.config.{Config, ConfigFactory}
import com.zaxxer.hikari.HikariDataSource
import com.zaxxer.hikari.metrics.{IMetricsTracker, MetricsTrackerFactory, PoolStats}
import org.junit.Assert.*
import org.junit.Test
import org.slf4j.LoggerFactory
import slick.SlickException
import slick.cats.Database
import slick.jdbc.{DatabaseConfig, H2Profile, JdbcDataSource, JdbcDataSourceFactory}
import slick.jdbc.hikaricp.HikariCPJdbcDataSource
import slick.util.ClassLoaderUtil

class HikariCPConfigTest {
  private def config(s: String): Config = ConfigFactory.parseString(s)

  private val h2 = """url = "jdbc:h2:mem:hikariConfigTest", driver = org.h2.Driver"""

  /** Run `f` and return the warnings HikariCPJdbcDataSource logged meanwhile. */
  private def warningsDuring(f: => Unit): Seq[String] = {
    val logger = LoggerFactory.getLogger(classOf[HikariCPJdbcDataSource]).asInstanceOf[Logger]
    val appender = new ListAppender[ILoggingEvent]
    appender.start()
    val oldLevel = logger.getLevel
    logger.setLevel(Level.WARN)
    logger.addAppender(appender)
    try f
    finally {
      logger.detachAppender(appender)
      logger.setLevel(oldLevel)
    }
    appender.list.asScala.toSeq.filter(_.getLevel == Level.WARN).map(_.getFormattedMessage)
  }

  private def withSource[A](c: Config)(f: HikariCPJdbcDataSource => A): A = {
    val source = HikariCPJdbcDataSource.forConfig(c, null, "hikariConfigTest", ClassLoaderUtil.defaultClassLoader)
    try f(source) finally source.close()
  }

  @Test def testNoWarningsForPlainConfig(): Unit = {
    val warnings = warningsDuring {
      withSource(config(s"$h2, maxConnections = 3")) { source =>
        assertEquals(3, source.ds.getMaximumPoolSize)
        assertEquals(-1L, source.hconf.getInitializationFailTimeout)
      }
    }
    assertEquals(Nil, warnings)
  }

  @Test def testInitializationFailTimeout(): Unit = {
    withSource(config(s"$h2, initializationFailTimeout = 1")) { source =>
      assertEquals(1L, source.hconf.getInitializationFailTimeout)
    }
    // The removed `initializationFailFast` key is no longer read
    withSource(config(s"$h2, initializationFailFast = true")) { source =>
      assertEquals(-1L, source.hconf.getInitializationFailTimeout)
    }
  }

  @Test def testMaximumPoolSizeAndMaxConnections(): Unit = {
    val warnings = warningsDuring {
      withSource(config(s"$h2, maxConnections = 3, maximumPoolSize = 5")) { source =>
        assertEquals(5, source.ds.getMaximumPoolSize)
        assertEquals(Some(5), source.maxConnections)
      }
    }
    assertEquals(1, warnings.size)
    assertTrue(warnings.head, warnings.head.contains("Both `maximumPoolSize` (5) and `maxConnections` (3) are set"))
  }

  @Test def testEqualMaximumPoolSizeAndMaxConnections(): Unit = {
    val warnings = warningsDuring {
      withSource(config(s"$h2, maxConnections = 4, maximumPoolSize = 4")) { source =>
        assertEquals(4, source.ds.getMaximumPoolSize)
      }
    }
    assertEquals(Nil, warnings)
  }

  @Test def testDataSourceClass(): Unit = {
    val c = config("""dataSourceClass = "org.h2.jdbcx.JdbcDataSource", properties.url = "jdbc:h2:mem:hikariConfigTest"""")
    withSource(c) { source =>
      val conn = source.createConnection()
      try assertTrue(conn.isValid(1)) finally conn.close()
    }
  }

  private def assertNotADataSource(key: String, connectionPool: String): Unit = {
    val c = config(s"""connectionPool = $connectionPool, $key = "slick.jdbc.MySQLProfile$$"""")
    try {
      JdbcDataSource.forConfig(c, null, "hikariConfigTest", ClassLoaderUtil.defaultClassLoader).close()
      fail("Expected a SlickException")
    } catch {
      case ex: SlickException =>
        assertEquals(
          s"The `$key` setting must name a javax.sql.DataSource class provided by the JDBC driver, but " +
            "slick.jdbc.MySQLProfile$ is not a DataSource. Configure the Slick profile with `profile` and a " +
            "java.sql.Driver class with `driver`.",
          ex.getMessage)
    }
  }

  @Test def testNotADataSourceClass(): Unit = {
    assertNotADataSource("dataSourceClass", "HikariCP")
    assertNotADataSource("dataSourceClassName", "HikariCP")
  }

  @Test def testNotADataSourceClassWithoutPool(): Unit = {
    assertNotADataSource("dataSourceClass", "disabled")
    assertNotADataSource("dataSourceClassName", "disabled")
  }

  /** Run a query on a database that uses `connectionPool`, then call `check` before the database is closed. */
  private def runSelect(connectionPool: String)(check: => Unit = ()): Unit = {
    val c = config(s"""$h2, connectionPool = "$connectionPool", maxConnections = 2, poolName = testPool""")
    val dc = DatabaseConfig.forProfileConfig(H2Profile, "", c)
    import H2Profile.api.*
    Database.resource(dc).use(_.run(sql"select 1".as[Int].head).map(_ => check)).unsafeRunSync()
  }

  @Test def testMetricsTrackerFactoryFromCustomConnectionPool(): Unit = {
    val created = TrackedHikariCP.trackerFactory.created.get
    runSelect(TrackedHikariCP.getClass.getName)()
    assertEquals(created + 1, TrackedHikariCP.trackerFactory.created.get)
    assertTrue(TrackedHikariCP.trackerFactory.acquired.get > 0)
  }

  @Test def testMetricAndHealthCheckRegistriesFromCustomConnectionPool(): Unit = {
    // HikariCP removes its metrics and health checks from the registries when the pool is closed
    runSelect(InstrumentedHikariCP.getClass.getName) {
      val timers = InstrumentedHikariCP.metricRegistry.getTimers.asScala
      assertTrue(timers.keys.toString, timers.get("testPool.pool.Wait").exists(_.getCount > 0))
      val healthChecks = InstrumentedHikariCP.healthCheckRegistry.getNames.asScala
      assertTrue(healthChecks.toString, healthChecks.contains("testPool.pool.ConnectivityCheck"))
    }
  }
}

class CountingMetricsTrackerFactory extends MetricsTrackerFactory {
  val created = new AtomicInteger
  val acquired = new AtomicInteger
  def create(poolName: String, poolStats: PoolStats): IMetricsTracker = {
    created.incrementAndGet()
    new IMetricsTracker {
      override def recordConnectionAcquiredNanos(elapsedAcquiredNanos: Long): Unit = { acquired.incrementAndGet(); () }
    }
  }
}

/** A `connectionPool` factory that registers a metrics tracker factory, as shown in the manual. */
object TrackedHikariCP extends JdbcDataSourceFactory {
  val trackerFactory = new CountingMetricsTrackerFactory
  def forConfig(c: Config, driver: Driver, name: String, classLoader: ClassLoader): HikariCPJdbcDataSource = {
    val hconf = HikariCPJdbcDataSource.hikariConfig(c, name, classLoader)
    hconf.setMetricsTrackerFactory(trackerFactory)
    new HikariCPJdbcDataSource(new HikariDataSource(hconf), hconf)
  }
}

/** A `connectionPool` factory that registers metric and health check registries, as shown in the manual. */
object InstrumentedHikariCP extends JdbcDataSourceFactory {
  val metricRegistry = new MetricRegistry
  val healthCheckRegistry = new HealthCheckRegistry
  def forConfig(c: Config, driver: Driver, name: String, classLoader: ClassLoader): HikariCPJdbcDataSource = {
    val hconf = HikariCPJdbcDataSource.hikariConfig(c, name, classLoader)
    hconf.setMetricRegistry(metricRegistry)
    hconf.setHealthCheckRegistry(healthCheckRegistry)
    new HikariCPJdbcDataSource(new HikariDataSource(hconf), hconf)
  }
}
