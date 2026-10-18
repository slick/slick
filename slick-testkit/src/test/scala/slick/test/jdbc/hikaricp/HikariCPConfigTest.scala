package slick.test.jdbc.hikaricp

import java.sql.Driver
import java.util.concurrent.atomic.AtomicInteger

import scala.jdk.CollectionConverters.*

import cats.effect.unsafe.implicits.global

import ch.qos.logback.classic.{Level, Logger}
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.read.ListAppender
import com.typesafe.config.{Config, ConfigFactory}
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

  @Test def testInitializationFailFastIsDeprecated(): Unit = {
    val warnings = warningsDuring {
      withSource(config(s"$h2, initializationFailFast = true")) { source =>
        assertEquals(1L, source.hconf.getInitializationFailTimeout)
      }
    }
    assertEquals(1, warnings.size)
    assertTrue(warnings.head, warnings.head.contains("`initializationFailFast` setting is deprecated"))
    assertTrue(warnings.head, warnings.head.contains("`initializationFailTimeout = 1` to fail fast"))
    assertTrue(warnings.head, warnings.head.contains("treated as `initializationFailTimeout = 1`"))
  }

  @Test def testInitializationFailFastIgnoredWithInitializationFailTimeout(): Unit = {
    val warnings = warningsDuring {
      withSource(config(s"$h2, initializationFailFast = true, initializationFailTimeout = -1")) { source =>
        assertEquals(-1L, source.hconf.getInitializationFailTimeout)
      }
    }
    assertEquals(1, warnings.size)
    assertTrue(warnings.head, warnings.head.contains("ignored because `initializationFailTimeout` is also set"))
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

  @Test def testMetricsTrackerFactoryFromCustomConnectionPool(): Unit = {
    val c = config(s"""$h2, connectionPool = "${MetricsHikariCP.getClass.getName}", maxConnections = 2""")
    val dc = DatabaseConfig.forProfileConfig(H2Profile, "", c)
    val created = MetricsHikariCP.trackerFactory.created.get
    import H2Profile.api.*
    Database.resource(dc).use(_.run(sql"select 1".as[Int].head)).unsafeRunSync()
    assertEquals(created + 1, MetricsHikariCP.trackerFactory.created.get)
    assertTrue(MetricsHikariCP.trackerFactory.acquired.get > 0)
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

/** A `connectionPool` factory that registers metrics on the HikariCP pool, as shown in the manual. */
object MetricsHikariCP extends JdbcDataSourceFactory {
  val trackerFactory = new CountingMetricsTrackerFactory
  def forConfig(c: Config, driver: Driver, name: String, classLoader: ClassLoader): HikariCPJdbcDataSource = {
    val source = HikariCPJdbcDataSource.forConfig(c, driver, name, classLoader)
    source.ds.setMetricsTrackerFactory(trackerFactory)
    source
  }
}
