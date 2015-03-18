package slick.test.stream

import org.testng.annotations.{AfterClass, BeforeClass}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}
import slick.memory.MemoryDriver

class HeapPublisherTest extends RelationalPublisherTest[MemoryDriver](MemoryDriver, 300L) {
  import driver.api._

  @BeforeClass def setUpDB: Unit =
    db = Database(ExecutionContext.global)

  @AfterClass def tearDownDB: Unit =
    Await.ready(db.shutdown, Duration.Inf)
}
