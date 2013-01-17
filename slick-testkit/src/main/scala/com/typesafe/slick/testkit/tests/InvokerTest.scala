package com.typesafe.slick.testkit.tests

import scala.collection.mutable.ArrayBuffer
import org.junit.Assert._
import scala.slick.util.CloseableIterator
import com.typesafe.slick.testkit.util.{TestkitTest, TestDB}

class InvokerTest(val tdb: TestDB) extends TestkitTest {
  import tdb.profile.simple._

  override val reuseInstance = true

  def testCollections {
    object T extends Table[Int]("t") {
      def a = column[Int]("a")
      def * = a
    }

    T.ddl.create
    T.insertAll(2, 3, 1, 5, 4)

    val q = T.map(_.a).sorted

    val r1 = q.list
    val r1t: List[Int] = r1
    assertEquals(List(1, 2, 3, 4, 5), r1)

    val r2 = q.to[List]
    val r2t: List[Int] = r2
    assertEquals(List(1, 2, 3, 4, 5), r2)

    val r3 = q.to[Set]
    val r3t: Set[Int] = r3
    assertEquals(Set(3, 4, 2, 1, 5), r3)

    val r4 = q.to[IndexedSeq]
    val r4t: IndexedSeq[Int] = r4
    assertEquals(IndexedSeq(1, 2, 3, 4, 5), r4)

    val r5 = q.to[ArrayBuffer]
    val r5t: ArrayBuffer[Int] = r5
    assertEquals(ArrayBuffer(1, 2, 3, 4, 5), r5)

    val r6 = q.to[Array]
    val r6t: Array[Int] = r6
    assertEquals(Array(1, 2, 3, 4, 5).toList, r6.toList)

    val it = q.elements
    val sum = try {
      it.reduceLeft(_ + _)
    } finally it.close()
    assertEquals(15, sum)
  }

  def testMap {
    val T = new Table[(Int, String)]("t2") {
      def k = column[Int]("k")
      def v = column[String]("v")
      def * = k ~ v
    }

    T.ddl.create
    T.insertAll(2 -> "b", 3 -> "c", 1 -> "a")

    val q = Query(T)

    val r1 = q.toMap
    val r1t: Map[Int, String] = r1
    assertEquals(Map(1 -> "a", 2 -> "b", 3 -> "c"), r1)
  }

  def testLazy = if(tdb.isShared) {
    object T extends Table[Int]("t3") {
      def a = column[Int]("a")
      def * = a
    }

    val q = Query(T).sortBy(_.a)

    def setUp(session: Session) {
      T.ddl.create(session)
      for(g <- 1 to 1000 grouped 100)
        T.insertAll(g:_*)(session)
    }

    def f() = CloseableIterator close db.createSession after { session =>
      setUp(session)
      q.elements()(session)
    }

    def g() = CloseableIterator close db.createSession after { session =>
      setUp(session)
      throw new Exception("make sure it gets closed")
    }

    val it = f()
    it.use { assertEquals((1 to 1000).toList, it.toStream.toList) }
    assertFail(g())
    db.withSession { s: Session => T.ddl.drop(s) }
    val it2 = f()
    it2.use { assertEquals((1 to 1000).toList, it2.toStream.toList) }
  }
}
