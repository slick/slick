package slick.test.jdbc

import org.junit.Test
import org.junit.Assert._
import slick.backend.DatabaseConfig
import slick.driver.JdbcProfile

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
}
