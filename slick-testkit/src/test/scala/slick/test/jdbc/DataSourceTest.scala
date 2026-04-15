package slick.test.jdbc

import java.sql.{Connection, Driver, DriverPropertyInfo, SQLException}
import java.util.Properties
import java.util.logging.Logger

import cats.effect.IO
import cats.effect.unsafe.implicits.global

import com.typesafe.config.ConfigFactory
import org.junit.Assert.*
import org.junit.Test
import slick.basic.BasicDatabaseConfig
import slick.cats
import slick.jdbc.DatabaseConfig
import slick.jdbc.{H2Profile, JdbcProfile}

class DataSourceTest {
  private def lastConnectionData: (String, Properties) =
    MockDriver.getLast.getOrElse(throw new AssertionError("No connection data recorded"))

  private def withDb[P <: JdbcProfile, A](dc: BasicDatabaseConfig[P])
                                   (f: (dc.profile.backend.Database[IO], cats.Database) => IO[A]): A = {
    val rawDb = dc.profile.backend.makeDatabase[IO](dc).unsafeRunSync()
    val db = cats.Database.fromCore(rawDb)
    try f(rawDb, db).unsafeRunSync()
    finally rawDb.close()
  }

  @Test def testDataSourceJdbcDataSource: Unit = {
    val dc = DatabaseConfig.forConfig[JdbcProfile]("ds1")
    import dc.profile.api.*
    withDb(dc) { (_, db) =>
      db.run(sql"select setting_value from information_schema.settings where setting_name = 'MODE'".as[String].head)
        .map(r => assertEquals("MySQL", r))
    }
  }

  @Test def testDirectDataSource: Unit = {
    val dc = DatabaseConfig.forConfig[JdbcProfile]("ds2")
    import dc.profile.api.*
    withDb(dc) { (_, db) =>
      db.run(sql"select setting_value from information_schema.settings where setting_name = 'MODE'".as[String].head)
        .map(r => assertEquals("PostgreSQL", r))
    }
  }

  @Test def testDatabaseUrlDataSource: Unit = {
    import slick.jdbc.H2Profile.api.actionBasedSQLInterpolation
    MockDriver.reset
    val dc = DatabaseConfig.forProfileConfig(H2Profile, "databaseUrl")
    withDb(dc) { (rawDb, db) =>
      assertEquals(Some(20), rawDb.source.maxConnections)
      try db.run(sqlu"dummy").unsafeRunSync() catch { case _: SQLException => }
      val (url, info) = lastConnectionData
      assertEquals("jdbc:postgresql://host/dbname", url)
      assertEquals("user", info.getProperty("user"))
      assertEquals("pass", info.getProperty("password"))
      assertEquals("bar", info.getProperty("foo"))
      IO.unit
    }
  }

  @Test def testDatabaseUrlDataSourceNoPassword: Unit = {
    import slick.jdbc.H2Profile.api.actionBasedSQLInterpolation
    MockDriver.reset
    val dc = DatabaseConfig.forProfileConfig(H2Profile, "databaseUrlNoPassword")
    withDb(dc) { (rawDb, db) =>
      assertEquals(Some(20), rawDb.source.maxConnections)
      try db.run(sqlu"dummy").unsafeRunSync() catch { case _: SQLException => }
      val (url, info) = lastConnectionData
      assertEquals("jdbc:postgresql://host/dbname", url)
      assertEquals("user", info.getProperty("user"))
      assertEquals(null, info.getProperty("password"))
      assertEquals("bar", info.getProperty("foo"))
      IO.unit
    }
  }

  @Test def testDatabaseUrlDataSourceMySQL: Unit = {
    import slick.jdbc.H2Profile.api.actionBasedSQLInterpolation
    MockDriver.reset
    val dc = DatabaseConfig.forProfileConfig(H2Profile, "databaseUrlMySQL")
    withDb(dc) { (rawDb, db) =>
      assertEquals(Some(20), rawDb.source.maxConnections)
      try db.run(sqlu"dummy").unsafeRunSync() catch { case _: SQLException => }
      val (url, info) = lastConnectionData
      assertEquals("jdbc:mysql://host/dbname?useUnicode=yes&characterEncoding=UTF-8&connectionCollation=utf8_general_ci", url)
      assertEquals("user", info.getProperty("user"))
      assertEquals("pass", info.getProperty("password"))
      assertEquals("bar", info.getProperty("foo"))
      IO.unit
    }
  }

  @Test def testDatabaseUrlDataSourceMySQLNoPassword: Unit = {
    import slick.jdbc.H2Profile.api.actionBasedSQLInterpolation
    MockDriver.reset
    val dc = DatabaseConfig.forProfileConfig(H2Profile, "databaseUrlMySQLNoPassword")
    withDb(dc) { (rawDb, db) =>
      assertEquals(Some(20), rawDb.source.maxConnections)
      try db.run(sqlu"dummy").unsafeRunSync() catch { case _: SQLException => }
      val (url, info) = lastConnectionData
      assertEquals("jdbc:mysql://host/dbname?useUnicode=yes&characterEncoding=UTF-8&connectionCollation=utf8_general_ci", url)
      assertEquals("user", info.getProperty("user"))
      assertEquals(null, info.getProperty("password"))
      assertEquals("bar", info.getProperty("foo"))
      IO.unit
    }
  }

  @Test def testAltDatabaseUrlDataSourceScheme: Unit = {
    import slick.jdbc.H2Profile.api.actionBasedSQLInterpolation
    MockDriver.reset
    val dc = DatabaseConfig.forProfileConfig(H2Profile, "altDatabaseUrl")
    withDb(dc) { (rawDb, db) =>
      assertEquals(Some(20), rawDb.source.maxConnections)
      try db.run(sqlu"dummy").unsafeRunSync() catch { case _: SQLException => }
      val (url, info) = lastConnectionData
      assertEquals("jdbc:postgresql://host/dbname", url)
      assertEquals("user", info.getProperty("user"))
      assertEquals("pass", info.getProperty("password"))
      assertEquals("bar", info.getProperty("foo"))
      IO.unit
    }
  }

  @Test def testMaxConnections: Unit = {
    MockDriver.reset
    val dc = DatabaseConfig.forProfileConfig(H2Profile, "databaseUrl", ConfigFactory.parseString(
      """
        |databaseUrl {
        |  dataSourceClass = "slick.jdbc.DatabaseUrlDataSource"
        |  maxConnections = 20
        |  url = "postgres://user:pass@host/dbname"
        |}
        |""".stripMargin
    ))
    withDb(dc) { (rawDb, _) =>
      assertEquals("maxConnections should be respected", Some(20), rawDb.source.maxConnections)
      IO.unit
    }
  }

  @Test def testMaxConnectionsFallbackToNumThreads: Unit = {
    MockDriver.reset
    val dc = DatabaseConfig.forProfileConfig(H2Profile, "databaseUrl", ConfigFactory.parseString(
      """
        |databaseUrl {
        |  dataSourceClass = "slick.jdbc.DatabaseUrlDataSource"
        |  numThreads = 10
        |  url = "postgres://user:pass@host/dbname"
        |}
        |""".stripMargin
    ))
    withDb(dc) { (rawDb, db) =>
      assertEquals("maxConnections should be numThreads", Some(10), rawDb.source.maxConnections)
      val status = db.controlStatus.unsafeRunSync()
      assertEquals("default queueSize should be 1000", 1000L, status.availableAdmissionQueueSlots)
      assertEquals("default maxInflightActions should be 2 * maxConnections", 20L, status.availableInflightSlots)
      IO.unit
    }
  }

  @Test def testMaxConnectionsPreferredOverNumThreads: Unit = {
    MockDriver.reset
    val dc = DatabaseConfig.forProfileConfig(H2Profile, "databaseUrl", ConfigFactory.parseString(
      """
        |databaseUrl {
        |  dataSourceClass = "slick.jdbc.DatabaseUrlDataSource"
        |  maxConnections = 12
        |  numThreads = 10
        |  url = "postgres://user:pass@host/dbname"
        |}
        |""".stripMargin
    ))
    withDb(dc) { (rawDb, db) =>
      assertEquals("maxConnections should take precedence over numThreads", Some(12), rawDb.source.maxConnections)
      val status = db.controlStatus.unsafeRunSync()
      assertEquals("default queueSize should be 1000", 1000L, status.availableAdmissionQueueSlots)
      assertEquals("default maxInflightActions should be 2 * maxConnections", 24L, status.availableInflightSlots)
      IO.unit
    }
  }

  @Test def testInflightAndQueueConfig: Unit = {
    MockDriver.reset
    val dc = DatabaseConfig.forProfileConfig(H2Profile, "databaseUrl", ConfigFactory.parseString(
      """
        |databaseUrl {
        |  dataSourceClass = "slick.jdbc.DatabaseUrlDataSource"
        |  maxConnections = 10
        |  maxInflightActions = 5
        |  queueSize = 42
        |  url = "postgres://user:pass@host/dbname"
        |}
        |""".stripMargin
    ))
    withDb(dc) { (rawDb, db) =>
      assertEquals("maxConnections should be respected", Some(10), rawDb.source.maxConnections)
      val status = db.controlStatus.unsafeRunSync()
      assertEquals("queueSize should be configurable", 42L, status.availableAdmissionQueueSlots)
      assertEquals("maxInflightActions must be at least maxConnections", 10L, status.availableInflightSlots)
      IO.unit
    }
  }

  @Test def testAdmissionAndConnectionTimeoutConfig: Unit = {
    MockDriver.reset
    val dc = DatabaseConfig.forProfileConfig(H2Profile, "databaseUrl", ConfigFactory.parseString(
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
    ))
    withDb(dc) { (_, db) =>
      val status = db.controlStatus.unsafeRunSync()
      assertEquals("queueSize should be configurable", 42L, status.availableAdmissionQueueSlots)
      IO.unit
    }
  }

  @Test def testTimeoutsMustBePositive: Unit = {
    MockDriver.reset
    val inflightEx = try {
      val dc = DatabaseConfig.forProfileConfig(H2Profile, "databaseUrl", ConfigFactory.parseString(
        """
          |databaseUrl {
          |  dataSourceClass = "slick.jdbc.DatabaseUrlDataSource"
          |  maxConnections = 10
          |  inflightAdmissionTimeout = 0ms
          |  url = "postgres://user:pass@host/dbname"
          |}
          |""".stripMargin
      ))
      val rawDb = dc.profile.backend.makeDatabase[IO](dc).unsafeRunSync()
      rawDb.close()
      null
    } catch {
      case t: Throwable => t
    }

    assertTrue("zero inflight timeout should be rejected", inflightEx != null)
    assertTrue("must mention positive inflight timeout", inflightEx.getMessage.contains("inflightAdmissionTimeout must be > 0"))

    val connectionEx = try {
      val dc = DatabaseConfig.forProfileConfig(H2Profile, "databaseUrl", ConfigFactory.parseString(
        """
          |databaseUrl {
          |  dataSourceClass = "slick.jdbc.DatabaseUrlDataSource"
          |  maxConnections = 10
          |  connectionAcquireTimeout = 0ms
          |  url = "postgres://user:pass@host/dbname"
          |}
          |""".stripMargin
      ))
      val rawDb = dc.profile.backend.makeDatabase[IO](dc).unsafeRunSync()
      rawDb.close()
      null
    } catch {
      case t: Throwable => t
    }

    assertTrue("zero connection timeout should be rejected", connectionEx != null)
    assertTrue("must mention positive connection timeout", connectionEx.getMessage.contains("connectionAcquireTimeout must be > 0"))
  }

  @Test def testConnectionPoolDisabled: Unit = {
    MockDriver.reset
    val dc = DatabaseConfig.forProfileConfig(H2Profile, "databaseUrl", ConfigFactory.parseString(
      """
        |databaseUrl {
        |  dataSourceClass = "slick.jdbc.DatabaseUrlDataSource"
        |  connectionPool = "disabled"
        |  url = "postgres://user:pass@host/dbname"
        |}
        |
      """.stripMargin
    ))
    withDb(dc) { (rawDb, _) =>
      assertEquals("maxConnections should be None when not using a pool", None, rawDb.source.maxConnections)
      IO.unit
    }
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
