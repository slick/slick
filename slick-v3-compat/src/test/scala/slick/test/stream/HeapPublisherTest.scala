package slick.test.stream

import cats.effect.unsafe.implicits.global

import slick.memory.MemoryProfile

class HeapPublisherTest extends RelationalPublisherTest[MemoryProfile](MemoryProfile, 300L) {
  import profile.api.*

  def createDB: slick.compat.Database = slick.compat.Database.wrap(Database())
}
