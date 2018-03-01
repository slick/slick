package slick.test.jdbc

import java.sql.{Connection, Driver, DriverPropertyInfo, SQLException}
import java.util.Properties
import java.util.logging.Logger

import com.typesafe.config.ConfigFactory
import org.junit.Test
import org.junit.Assert._
import slick.basic.DatabaseConfig
import slick.jdbc.{JdbcBackend, JdbcProfile}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class DataSourceTest {
  @Test def testDataSourceJdbcDataSource: Unit = {
    val dc = DatabaseConfig.forConfig[JdbcProfile]("ds1")
    import dc.profile.api._
    try {
      assertEquals(1, Await.result(dc.db.run(sql"select lock_mode()".as[Int].head), Duration.Inf))
    } finally dc.db.close
  }

  @Test def testDirectDataSource: Unit = {
    val dc = DatabaseConfig.forConfig[JdbcProfile]("ds2")
    import dc.profile.api._
    try {
      assertEquals(2, Await.result(dc.db.run(sql"select lock_mode()".as[Int].head), Duration.Inf))
    } finally dc.db.close
  }

  @Test def testDatabaseUrlDataSource: Unit = {
    import slick.jdbc.H2Profile.api.actionBasedSQLInterpolation
    MockDriver.reset
    val db = JdbcBackend.Database.forConfig("databaseUrl")
    try {
      assertEquals(Some(20), db.source.maxConnections)
      try Await.result(db.run(sqlu"dummy"), Duration.Inf) catch { case ex: SQLException => }
      val (url, info) = MockDriver.getLast.getOrElse(fail("No connection data recorded").asInstanceOf[Nothing])
      assertEquals("jdbc:postgresql://host/dbname", url)
      assertEquals("user", info.getProperty("user"))
      assertEquals("pass", info.getProperty("password"))
      assertEquals("bar", info.getProperty("foo"))
    } finally db.close
  }

  @Test def testDatabaseUrlDataSourceNoPassword: Unit = {
    import slick.jdbc.H2Profile.api.actionBasedSQLInterpolation
    MockDriver.reset
    val db = JdbcBackend.Database.forConfig("databaseUrlNoPassword")
    try {
      assertEquals(Some(20), db.source.maxConnections)
      try Await.result(db.run(sqlu"dummy"), Duration.Inf) catch { case ex: SQLException => }
      val (url, info) = MockDriver.getLast.getOrElse(fail("No connection data recorded").asInstanceOf[Nothing])
      assertEquals("jdbc:postgresql://host/dbname", url)
      assertEquals("user", info.getProperty("user"))
      assertEquals(null, info.getProperty("password"))
      assertEquals("bar", info.getProperty("foo"))
    } finally db.close
  }

  @Test def testDatabaseUrlDataSourceMySQL: Unit = {
    import slick.jdbc.H2Profile.api.actionBasedSQLInterpolation
    MockDriver.reset
    val db = JdbcBackend.Database.forConfig("databaseUrlMySQL")
    try {
      assertEquals(Some(20), db.source.maxConnections)
      try Await.result(db.run(sqlu"dummy"), Duration.Inf) catch { case ex: SQLException => }
      val (url, info) = MockDriver.getLast.getOrElse(fail("No connection data recorded").asInstanceOf[Nothing])
      assertEquals("jdbc:mysql://host/dbname?useUnicode=yes&characterEncoding=UTF-8&connectionCollation=utf8_general_ci", url)
      assertEquals("user", info.getProperty("user"))
      assertEquals("pass", info.getProperty("password"))
      assertEquals("bar", info.getProperty("foo"))
    } finally db.close
  }

  @Test def testDatabaseUrlDataSourceMySQLNoPassword: Unit = {
    import slick.jdbc.H2Profile.api.actionBasedSQLInterpolation
    MockDriver.reset
    val db = JdbcBackend.Database.forConfig("databaseUrlMySQLNoPassword")
    try {
      assertEquals(Some(20), db.source.maxConnections)
      try Await.result(db.run(sqlu"dummy"), Duration.Inf) catch { case ex: SQLException => }
      val (url, info) = MockDriver.getLast.getOrElse(fail("No connection data recorded").asInstanceOf[Nothing])
      assertEquals("jdbc:mysql://host/dbname?useUnicode=yes&characterEncoding=UTF-8&connectionCollation=utf8_general_ci", url)
      assertEquals("user", info.getProperty("user"))
      assertEquals(null, info.getProperty("password"))
      assertEquals("bar", info.getProperty("foo"))
    } finally db.close
  }

  @Test def testAltDatabaseUrlDataSourceScheme: Unit = {
    import slick.jdbc.H2Profile.api.actionBasedSQLInterpolation
    MockDriver.reset
    val db = JdbcBackend.Database.forConfig("altDatabaseUrl")
    try {
      assertEquals(Some(20), db.source.maxConnections)
      try Await.result(db.run(sqlu"dummy"), Duration.Inf) catch { case ex: SQLException => }
      val (url, info) = MockDriver.getLast.getOrElse(fail("No connection data recorded").asInstanceOf[Nothing])
      assertEquals("jdbc:postgresql://host/dbname", url)
      assertEquals("user", info.getProperty("user"))
      assertEquals("pass", info.getProperty("password"))
      assertEquals("bar", info.getProperty("foo"))
    } finally db.close
  }

  @Test def testMaxConnections: Unit = {
    MockDriver.reset
    val db = JdbcBackend.Database.forConfig("databaseUrl", ConfigFactory.parseString(
      """
         |databaseUrl {
         |  dataSourceClass = "slick.jdbc.DatabaseUrlDataSource"
         |  maxConnections = 20
         |  url = "postgres://user:pass@host/dbname"
         |}
         |""".stripMargin))
    try {
      assertEquals("maxConnections should be respected", Some(20), db.source.maxConnections)
    } finally db.close
  }

  @Test def testMaxConnectionsNumThreads: Unit = {
    MockDriver.reset
    val db = JdbcBackend.Database.forConfig("databaseUrl", ConfigFactory.parseString(
      """
        |databaseUrl {
        |  dataSourceClass = "slick.jdbc.DatabaseUrlDataSource"
        |  numThreads = 10
        |  url = "postgres://user:pass@host/dbname"
        |}
        |""".stripMargin
    ))
    try {
      assertEquals("maxConnections should be numThreads", Some(10), db.source.maxConnections)
    } finally db.close
  }

  @Test def testConnectionPoolDisabled: Unit = {
    MockDriver.reset
    val db = JdbcBackend.Database.forConfig("databaseUrl", ConfigFactory.parseString(
      """
        |databaseUrl {
        |  dataSourceClass = "slick.jdbc.DatabaseUrlDataSource"
        |  connectionPool = "disabled"
        |  url = "postgres://user:pass@host/dbname"
        |}
        |
      """.stripMargin))
    try {
      assertEquals("maxConnections should be None when not using a pool", None, db.source.maxConnections)
    } finally db.close
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

