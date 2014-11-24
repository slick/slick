package scala.slick.test.stream

import org.testng.annotations.{AfterClass, BeforeClass}

import scala.slick.driver.{H2Driver, JdbcProfile}

class JdbcPublisherTest extends RelationalPublisherTest[JdbcProfile](H2Driver) {
  import driver.api._

  @BeforeClass def setUpDB: Unit =
    db = Database.forURL("jdbc:h2:mem:DatabasePublisherTest", driver = "org.h2.Driver")

  @AfterClass def tearDownDB: Unit =
    db.close()
}
