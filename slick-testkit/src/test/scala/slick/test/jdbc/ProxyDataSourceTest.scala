package slick.test.jdbc

import java.sql.SQLException

import org.junit.{Assert, Test}
import slick.backend.DatabaseConfig
import slick.driver.JdbcProfile
import slick.jdbc.ProxyDataSource

import scala.beans.BeanProperty
import scala.concurrent._
import scala.concurrent.duration.{FiniteDuration, Duration}

class ProxyDataSourceTest {

  @Test def testNoBinding: Unit = {
    val dc = DatabaseConfig.forConfig[JdbcProfile]("proxyds1")
    import dc.driver.api._
    try {
      try {
        Await.result(dc.db.run(sql"select 1".as[Int]), Duration.Inf)
      } finally dc.db.close()
      Assert.fail("Expected timeout")
    } catch {
      case ex: SQLException if ex.toString contains "Timeout" =>
    }
  }

  @Test def testInitialDelay: Unit = {
    val dc = DatabaseConfig.forConfig[JdbcProfile]("proxyds2")
    import dc.driver.api._
    try {
      val t0 = System.currentTimeMillis()
      println("DB name: " + Await.result(dc.db.run(Functions.database.result), Duration.Inf) + ", after: "+(System.currentTimeMillis()-t0)+"ms")
    } finally dc.db.close()
  }

  @Test def testLongInitialDelay: Unit = {
    val dc = DatabaseConfig.forConfig[JdbcProfile]("proxyds2b")
    import dc.driver.api._
    try {
      val t0 = System.currentTimeMillis()
      var count = 0
      var name: String = null
      while(count < 20 && (name eq null)) {
        try { name = Await.result(dc.db.run(Functions.database.result), Duration.Inf) }
        catch {
          case ex: SQLException if ex.toString contains "Timeout" =>
            println("Timeout waiting for result: "+ex)
        }
        count += 1
      }
      Assert.assertNotNull(name)
      println("DB name: " + name + ", after: "+(System.currentTimeMillis()-t0)+"ms")
    } finally dc.db.close()
  }
}

class NoBindingProxyDataSource extends ProxyDataSource {
  def lookup(serviceName: String): Future[Option[(String, Int)]] =
    Future.successful(None)
}

class InitialDelayProxyDataSource extends ProxyDataSource {
  @BeanProperty var delay: Long = 0 // delay in ms
  @BeanProperty var host: String = "localhost"
  @BeanProperty var port: Int = 0

  private[this] var f: Future[Option[(String, Int)]] = _

  def lookup(serviceName: String): Future[Option[(String, Int)]] = synchronized {
    if(f eq null)
      f = Future(blocking { Thread.sleep(delay); Some((host, port)) })(ExecutionContext.global)
    f
  }
}
