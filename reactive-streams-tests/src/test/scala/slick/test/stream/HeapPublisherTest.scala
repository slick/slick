package slick.test.stream

import scala.concurrent.ExecutionContext

import slick.memory.MemoryProfile


class HeapPublisherTest extends RelationalPublisherTest[MemoryProfile](MemoryProfile, 300L) {

  import profile.api.*

  def createDB = Database(ExecutionContext.global)
}
