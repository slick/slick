package slick.test.stream

import org.testng.annotations.{AfterClass, BeforeClass}

import scala.concurrent.ExecutionContext
import slick.memory.MemoryProfile

class HeapPublisherTest extends RelationalPublisherTest[MemoryProfile](MemoryProfile, 300L) {
  import profile.api._

  def createDB = Database(ExecutionContext.global)
}
