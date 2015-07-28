package slick.test.jdbc

import java.sql.{SQLException, DriverPropertyInfo, Connection, Driver}
import java.util.Properties
import java.util.logging.Logger

import org.junit.Test
import org.junit.Assert._
import slick.backend.DatabaseConfig
import slick.driver.JdbcProfile
import slick.jdbc.JdbcBackend

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class DataSourceTest {
  @Test def testDataSourceJdbcDataSource: Unit = {
    val dc = DatabaseConfig.forConfig[JdbcProfile]("ds1")
    import dc.driver.api._
    try {
      assertEquals(1, Await.result(dc.db.run(sql"select lock_mode()".as[Int].head), Duration.Inf))
    } finally dc.db.close
  }

  @Test def testDirectDataSource: Unit = {
    val dc = DatabaseConfig.forConfig[JdbcProfile]("ds2")
    import dc.driver.api._
    try {
      assertEquals(2, Await.result(dc.db.run(sql"select lock_mode()".as[Int].head), Duration.Inf))
    } finally dc.db.close
  }

  @Test def testDatabaseUrlDataSource: Unit = {
    import slick.driver.H2Driver.api.actionBasedSQLInterpolation
    MockDriver.reset
    val db = JdbcBackend.Database.forConfig("databaseUrl")
    try {
      try Await.result(db.run(sqlu"dummy"), Duration.Inf) catch { case ex: SQLException => }
      val (url, info) = MockDriver.getLast.getOrElse(fail("No connection data recorded").asInstanceOf[Nothing])
      assertEquals("jdbc:postgresql://host/dbname", url)
      assertEquals("user", info.getProperty("user"))
      assertEquals("pass", info.getProperty("password"))
      assertEquals("bar", info.getProperty("foo"))
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
