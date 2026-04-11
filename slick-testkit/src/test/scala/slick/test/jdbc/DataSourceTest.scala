package slick.test.jdbc

import java.sql.{Connection, Driver, DriverPropertyInfo, SQLException}
import java.util.Properties
import java.util.logging.Logger

import cats.effect.IO
import cats.effect.unsafe.implicits.global

import com.typesafe.config.ConfigFactory
import org.junit.Test
import org.junit.Assert._
import slick.basic.DatabaseConfig
import slick.jdbc.{JdbcBackend, JdbcProfile}

class DataSourceTest {
  @Test def testDataSourceJdbcDataSource: Unit = {
    val dc = DatabaseConfig.forConfig[JdbcProfile]("ds1")
    import dc.profile.api._
    dc.profile.backend.Database.forConfig[IO]("ds1.db").use { db =>
      db.run(sql"select setting_value from information_schema.settings where setting_name = 'MODE'".as[String].head)
        .map(r => assertEquals("MySQL", r))
    }.unsafeRunSync()
  }

  @Test def testDirectDataSource: Unit = {
    val dc = DatabaseConfig.forConfig[JdbcProfile]("ds2")
    import dc.profile.api._
    dc.profile.backend.Database.forConfig[IO]("ds2.db").use { db =>
      db.run(sql"select setting_value from information_schema.settings where setting_name = 'MODE'".as[String].head)
        .map(r => assertEquals("PostgreSQL", r))
    }.unsafeRunSync()
  }

  @Test def testDatabaseUrlDataSource: Unit = {
    import slick.jdbc.H2Profile.api.actionBasedSQLInterpolation
    MockDriver.reset
    val (db, close) = JdbcBackend.Database.forConfig[IO]("databaseUrl").allocated.unsafeRunSync()
    try {
      assertEquals(Some(20), db.source.maxConnections)
      try db.run(sqlu"dummy").unsafeRunSync() catch { case ex: SQLException => }
      val (url, info) = MockDriver.getLast.getOrElse(fail("No connection data recorded").asInstanceOf[Nothing])
      assertEquals("jdbc:postgresql://host/dbname", url)
      assertEquals("user", info.getProperty("user"))
      assertEquals("pass", info.getProperty("password"))
      assertEquals("bar", info.getProperty("foo"))
    } finally close.unsafeRunSync()
  }

  @Test def testDatabaseUrlDataSourceNoPassword: Unit = {
    import slick.jdbc.H2Profile.api.actionBasedSQLInterpolation
    MockDriver.reset
    val (db, close) = JdbcBackend.Database.forConfig[IO]("databaseUrlNoPassword").allocated.unsafeRunSync()
    try {
      assertEquals(Some(20), db.source.maxConnections)
      try db.run(sqlu"dummy").unsafeRunSync() catch { case ex: SQLException => }
      val (url, info) = MockDriver.getLast.getOrElse(fail("No connection data recorded").asInstanceOf[Nothing])
      assertEquals("jdbc:postgresql://host/dbname", url)
      assertEquals("user", info.getProperty("user"))
      assertEquals(null, info.getProperty("password"))
      assertEquals("bar", info.getProperty("foo"))
    } finally close.unsafeRunSync()
  }

  @Test def testDatabaseUrlDataSourceMySQL: Unit = {
    import slick.jdbc.H2Profile.api.actionBasedSQLInterpolation
    MockDriver.reset
    val (db, close) = JdbcBackend.Database.forConfig[IO]("databaseUrlMySQL").allocated.unsafeRunSync()
    try {
      assertEquals(Some(20), db.source.maxConnections)
      try db.run(sqlu"dummy").unsafeRunSync() catch { case ex: SQLException => }
      val (url, info) = MockDriver.getLast.getOrElse(fail("No connection data recorded").asInstanceOf[Nothing])
      assertEquals("jdbc:mysql://host/dbname?useUnicode=yes&characterEncoding=UTF-8&connectionCollation=utf8_general_ci", url)
      assertEquals("user", info.getProperty("user"))
      assertEquals("pass", info.getProperty("password"))
      assertEquals("bar", info.getProperty("foo"))
    } finally close.unsafeRunSync()
  }

  @Test def testDatabaseUrlDataSourceMySQLNoPassword: Unit = {
    import slick.jdbc.H2Profile.api.actionBasedSQLInterpolation
    MockDriver.reset
    val (db, close) = JdbcBackend.Database.forConfig[IO]("databaseUrlMySQLNoPassword").allocated.unsafeRunSync()
    try {
      assertEquals(Some(20), db.source.maxConnections)
      try db.run(sqlu"dummy").unsafeRunSync() catch { case ex: SQLException => }
      val (url, info) = MockDriver.getLast.getOrElse(fail("No connection data recorded").asInstanceOf[Nothing])
      assertEquals("jdbc:mysql://host/dbname?useUnicode=yes&characterEncoding=UTF-8&connectionCollation=utf8_general_ci", url)
      assertEquals("user", info.getProperty("user"))
      assertEquals(null, info.getProperty("password"))
      assertEquals("bar", info.getProperty("foo"))
    } finally close.unsafeRunSync()
  }

  @Test def testAltDatabaseUrlDataSourceScheme: Unit = {
    import slick.jdbc.H2Profile.api.actionBasedSQLInterpolation
    MockDriver.reset
    val (db, close) = JdbcBackend.Database.forConfig[IO]("altDatabaseUrl").allocated.unsafeRunSync()
    try {
      assertEquals(Some(20), db.source.maxConnections)
      try db.run(sqlu"dummy").unsafeRunSync() catch { case ex: SQLException => }
      val (url, info) = MockDriver.getLast.getOrElse(fail("No connection data recorded").asInstanceOf[Nothing])
      assertEquals("jdbc:postgresql://host/dbname", url)
      assertEquals("user", info.getProperty("user"))
      assertEquals("pass", info.getProperty("password"))
      assertEquals("bar", info.getProperty("foo"))
    } finally close.unsafeRunSync()
  }

  @Test def testMaxConnections: Unit = {
    MockDriver.reset
    val (db, close) = JdbcBackend.Database.forConfig[IO]("databaseUrl", ConfigFactory.parseString(
      """
         |databaseUrl {
         |  dataSourceClass = "slick.jdbc.DatabaseUrlDataSource"
         |  maxConnections = 20
         |  url = "postgres://user:pass@host/dbname"
         |}
         |""".stripMargin)).allocated.unsafeRunSync()
    try {
      assertEquals("maxConnections should be respected", Some(20), db.source.maxConnections)
    } finally close.unsafeRunSync()
  }

  @Test def testMaxConnectionsFallbackToNumThreads: Unit = {
    MockDriver.reset
    val (db, close) = JdbcBackend.Database.forConfig[IO]("databaseUrl", ConfigFactory.parseString(
      """
        |databaseUrl {
        |  dataSourceClass = "slick.jdbc.DatabaseUrlDataSource"
        |  numThreads = 10
        |  url = "postgres://user:pass@host/dbname"
        |}
        |""".stripMargin
    )).allocated.unsafeRunSync()
    try {
      assertEquals("maxConnections should be numThreads", Some(10), db.source.maxConnections)
      val q = db.availableAdmissionQueueSlots.unsafeRunSync()
      val i = db.availableInflightSlots.unsafeRunSync()
      assertEquals("default queueSize should be 1000", 1000L, q)
      assertEquals("default maxInflightActions should be 2 * maxConnections", 20L, i)
    } finally close.unsafeRunSync()
  }

  @Test def testMaxConnectionsPreferredOverNumThreads: Unit = {
    MockDriver.reset
    val (db, close) = JdbcBackend.Database.forConfig[IO]("databaseUrl", ConfigFactory.parseString(
      """
        |databaseUrl {
        |  dataSourceClass = "slick.jdbc.DatabaseUrlDataSource"
        |  maxConnections = 12
        |  numThreads = 10
        |  url = "postgres://user:pass@host/dbname"
        |}
        |""".stripMargin
    )).allocated.unsafeRunSync()
    try {
      val q = db.availableAdmissionQueueSlots.unsafeRunSync()
      val i = db.availableInflightSlots.unsafeRunSync()
      assertEquals("maxConnections should take precedence over numThreads", Some(12), db.source.maxConnections)
      assertEquals("default queueSize should be 1000", 1000L, q)
      assertEquals("default maxInflightActions should be 2 * maxConnections", 24L, i)
    } finally close.unsafeRunSync()
  }

  @Test def testInflightAndQueueConfig: Unit = {
    MockDriver.reset
    val (db, close) = JdbcBackend.Database.forConfig[IO]("databaseUrl", ConfigFactory.parseString(
      """
        |databaseUrl {
        |  dataSourceClass = "slick.jdbc.DatabaseUrlDataSource"
        |  maxConnections = 10
        |  maxInflightActions = 5
        |  queueSize = 42
        |  url = "postgres://user:pass@host/dbname"
        |}
        |""".stripMargin
    )).allocated.unsafeRunSync()
    try {
      val q = db.availableAdmissionQueueSlots.unsafeRunSync()
      val i = db.availableInflightSlots.unsafeRunSync()
      assertEquals("maxConnections should be respected", Some(10), db.source.maxConnections)
      assertEquals("queueSize should be configurable", 42L, q)
      assertEquals("maxInflightActions must be at least maxConnections", 10L, i)
    } finally close.unsafeRunSync()
  }

  @Test def testAdmissionAndConnectionTimeoutConfig: Unit = {
    MockDriver.reset
    val (db, close) = JdbcBackend.Database.forConfig[IO]("databaseUrl", ConfigFactory.parseString(
      """
        |databaseUrl {
        |  dataSourceClass = "slick.jdbc.DatabaseUrlDataSource"
        |  maxConnections = 10
        |  maxInflightActions = 20
        |  queueSize = 42
        |  inflightAdmissionTimeout = 250ms
        |  connectionAcquireTimeout = 500ms
        |  url = "postgres://user:pass@host/dbname"
        |}
        |""".stripMargin
    )).allocated.unsafeRunSync()
    try {
      assertEquals("queueSize should be configurable", 42L, db.availableAdmissionQueueSlots.unsafeRunSync())
    } finally close.unsafeRunSync()
  }

  @Test def testTimeoutsMustBePositive: Unit = {
    MockDriver.reset
    val inflightEx = try {
      JdbcBackend.Database.forConfig[IO]("databaseUrl", ConfigFactory.parseString(
        """
          |databaseUrl {
          |  dataSourceClass = "slick.jdbc.DatabaseUrlDataSource"
          |  maxConnections = 10
          |  inflightAdmissionTimeout = 0ms
          |  url = "postgres://user:pass@host/dbname"
          |}
          |""".stripMargin
      )).allocated.unsafeRunSync()
      null
    } catch {
      case t: Throwable => t
    }

    assertTrue("zero inflight timeout should be rejected", inflightEx != null)
    assertTrue("must mention positive inflight timeout", inflightEx.getMessage.contains("inflightAdmissionTimeout must be > 0"))

    val connectionEx = try {
      JdbcBackend.Database.forConfig[IO]("databaseUrl", ConfigFactory.parseString(
        """
          |databaseUrl {
          |  dataSourceClass = "slick.jdbc.DatabaseUrlDataSource"
          |  maxConnections = 10
          |  connectionAcquireTimeout = 0ms
          |  url = "postgres://user:pass@host/dbname"
          |}
          |""".stripMargin
      )).allocated.unsafeRunSync()
      null
    } catch {
      case t: Throwable => t
    }

    assertTrue("zero connection timeout should be rejected", connectionEx != null)
    assertTrue("must mention positive connection timeout", connectionEx.getMessage.contains("connectionAcquireTimeout must be > 0"))
  }

  @Test def testConnectionPoolDisabled: Unit = {
    MockDriver.reset
    val (db, close) = JdbcBackend.Database.forConfig[IO]("databaseUrl", ConfigFactory.parseString(
      """
        |databaseUrl {
        |  dataSourceClass = "slick.jdbc.DatabaseUrlDataSource"
        |  connectionPool = "disabled"
        |  url = "postgres://user:pass@host/dbname"
        |}
        |
      """.stripMargin)).allocated.unsafeRunSync()
    try {
      assertEquals("maxConnections should be None when not using a pool", None, db.source.maxConnections)
    } finally close.unsafeRunSync()
  }

}

object MockDriver {
  @volatile private var last: Option[(String, Properties)] = None
  def getLast = last
  def reset: Unit = last = None
}

class MockDriver extends Driver {
  def acceptsURL(url: String): Boolean = true
  def jdbcCompliant(): Boolean = false
  def getPropertyInfo(url: String, info: Properties): Array[DriverPropertyInfo] = Array()
  def getMinorVersion: Int = 0
  def getParentLogger: Logger = throw new SQLException("feature not implemented")
  def connect(url: String, info: Properties): Connection = {
    MockDriver.last = Some((url, info))
    throw new SQLException("Connection data has been recorded")
  }
  def getMajorVersion: Int = 0
}
