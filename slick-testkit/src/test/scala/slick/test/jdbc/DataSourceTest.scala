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

  @Test def testMaxConnectionsNumThreads: Unit = {
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
    } finally close.unsafeRunSync()
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
