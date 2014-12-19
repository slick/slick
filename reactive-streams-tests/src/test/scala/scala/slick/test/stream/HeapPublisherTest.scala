package scala.slick.test.stream

import org.testng.annotations.{AfterClass, BeforeClass}

import scala.concurrent.ExecutionContext
import scala.slick.memory.MemoryDriver

class HeapPublisherTest extends RelationalPublisherTest[MemoryDriver](MemoryDriver) {
  import driver.api._

  @BeforeClass def setUpDB: Unit =
    db = Database(ExecutionContext.global)

  @AfterClass def tearDownDB: Unit =
    db.close()
}
