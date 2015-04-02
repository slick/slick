package slick.test.stream

import org.testng.annotations.{AfterClass, BeforeClass}

import slick.driver.{H2Driver, JdbcProfile}
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.control.NonFatal

class JdbcPublisherTest extends RelationalPublisherTest[JdbcProfile](H2Driver, 1000L) {
  import driver.api._

  def createDB = {
    val db = Database.forURL("jdbc:h2:mem:DatabasePublisherTest", driver = "org.h2.Driver", keepAliveConnection = true)
    // Wait until the database has been initialized and can process queries:
    try { Await.result(db.run(sql"select 1".as[Int]), Duration.Inf) } catch { case NonFatal(ex) => }
    db
  }
}
