package slick.test.stream

import cats.effect.unsafe.implicits.global

import slick.future.Database
import slick.memory.MemoryProfile


class HeapPublisherTest extends RelationalPublisherTest[MemoryProfile](MemoryProfile, 300L) {

  def createDB =
    Database.fromCore(MemoryProfile.backend.Database().allocated.unsafeRunSync()._1)
}
