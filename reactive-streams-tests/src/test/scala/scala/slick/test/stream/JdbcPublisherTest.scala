package scala.slick.test.stream

import org.testng.annotations.{AfterClass, BeforeClass}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.slick.driver.{H2Driver, JdbcProfile}
import scala.util.control.NonFatal

class JdbcPublisherTest extends RelationalPublisherTest[JdbcProfile](H2Driver, 500L) {
  import driver.api._

  @BeforeClass def setUpDB: Unit = {
    db = Database.forURL("jdbc:h2:mem:DatabasePublisherTest", driver = "org.h2.Driver")
    //db = Database.forURL("jdbc:derby:memory:JdbcPublisherTest;create=true", driver = "org.apache.derby.jdbc.EmbeddedDriver")
    // Wait until the database has been initialized and can process queries:
    try { Await.result(db.run(sql"select 1".as[Int]), Duration.Inf) } catch { case NonFatal(ex) => }
  }

  @AfterClass def tearDownDB: Unit =
    db.close()
}
