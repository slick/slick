package slick.test.stream

import org.testng.annotations.{AfterClass, BeforeClass}

import scala.concurrent.ExecutionContext
import slick.memory.MemoryDriver

class HeapPublisherTest extends RelationalPublisherTest[MemoryDriver](MemoryDriver, 300L) {
  import driver.api._

  def createDB = Database(ExecutionContext.global)
}
