package scala.slick.test.lifted

import scala.collection.mutable.ArrayBuffer
import org.junit.Test
import org.junit.Assert._
import scala.slick.lifted._
import scala.slick.testutil._
import scala.slick.testutil.TestDB._
import scala.slick.util.CloseableIterator
import scala.slick.session.Session

object InvokerTest extends DBTestObject(H2Mem)

class InvokerTest(val tdb: TestDB) extends DBTest {
  import tdb.profile.Table
  import tdb.profile.Implicit._

  @deprecated("Testing deprecated method Query.orderBy", "0.10.0-M2")
  @Test def testCollections() {
    import scala.slick.session.Database.threadLocalSession

    object T extends Table[Int]("t") {
      def a = column[Int]("a")
      def * = a
    }

    db withSession {
      T.ddl.create
      T.insertAll(2, 3, 1, 5, 4)

      val q = for {
        t <- T
        _ <- Query orderBy t.a
      } yield t.a

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
    }
  }

  @Test def testMap() {
    import scala.slick.session.Database.threadLocalSession

    val T = new Table[(Int, String)]("t") {
      def k = column[Int]("k")
      def v = column[String]("v")
      def * = k ~ v
    }

    db withSession {
      T.ddl.create
      T.insertAll(2 -> "b", 3 -> "c", 1 -> "a")

      val q = Query(T)

      val r1 = q.toMap
      val r1t: Map[Int, String] = r1
      assertEquals(Map(1 -> "a", 2 -> "b", 3 -> "c"), r1)
    }
  }

  @deprecated("Testing deprecated method Query.orderBy", "0.10.0-M2")
  @Test def testLazy() {

    object T extends Table[Int]("t") {
      def a = column[Int]("a")
      def * = a
    }

    val q = for {
      t <- T
      _ <- Query orderBy t.a
    } yield t

    def setUp(implicit session: Session) {
      T.ddl.create
      for(g <- 1 to 1000 grouped 100)
        T.insertAll(g:_*)
    }

    def f() = CloseableIterator close db.createSession after { implicit session =>
      setUp
      q.elements
    }

    def g() = CloseableIterator close db.createSession after { implicit session =>
      setUp
      throw new Exception("make sure it gets closed")
    }

    val it = f()
    it.use { assertEquals((1 to 1000).toList, it.toStream.toList) }
    assertFail(g())
    val it2 = f()
    it2.use { assertEquals((1 to 1000).toList, it2.toStream.toList) }
  }
}
