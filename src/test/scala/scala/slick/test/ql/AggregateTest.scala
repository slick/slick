package scala.slick.test.ql

import org.junit.Test
import org.junit.Assert._
import scala.slick.ql._
import scala.slick.session.Database.threadLocalSession
import scala.slick.testutil._
import scala.slick.testutil.TestDB._

object AggregateTest extends DBTestObject(H2Mem, SQLiteMem, Postgres, MySQL, DerbyMem, HsqldbMem, MSAccess, SQLServer)

class AggregateTest(val tdb: TestDB) extends DBTest {
  import tdb.profile.Table
  import tdb.profile.Implicit._

  @Test def testAggregates() = db withSession {
    object T extends Table[(Int, Option[Int])]("t") {
      def a = column[Int]("a")
      def b = column[Option[Int]]("b")
      def * = a ~ b
    }
    T.ddl.create
    T.insertAll((1, Some(1)), (1, Some(2)), (1, Some(3)))
    val q = for {
      i <- Parameters[Int]
      t <- T if t.a === i
    } yield (t.a.count, t.b.count, t.a.sum, t.b.sum, t.a.avg, t.b.avg)
    println("q: "+q.selectStatement)
    println(q.first(0))
    println(q.first(1))
    assertEquals((0, 0, None, None, None, None), q.first(0))
    assertEquals((3, 3, Some(3), Some(6), Some(1), Some(2)), q.first(1))
  }

  @Test def testNewAggregates() = db withSession {
    object T extends Table[(Int, Option[Int])]("t") {
      def a = column[Int]("a")
      def b = column[Option[Int]]("b")
      def * = a ~ b
    }
    T.ddl.create
    T.insertAll((1, Some(1)), (1, Some(2)), (1, Some(3)))
    def q1(i: Int) = for { t <- T if t.a === i } yield t
    def q2(i: Int) = (q1(i).length, q1(i).map(_.a).sum, q1(i).map(_.b).sum, q1(i).map(_.b).avg)
    val q2_0 = q2(0).toQueryExecutor
    val q2_1 = q2(1).toQueryExecutor
    println("q2_0: "+q2_0._selectStatement)
    println("q2_1: "+q2_1._selectStatement)
    println(q2_0.run)
    println(q2_1.run)
    assertEquals((0, None, None, None), q2_0.run)
    assertEquals((3, Some(3), Some(6), Some(2)), q2_1.run)
  }

  @Test def testGroupBy() = db withSession {
    object T extends Table[(Int, Option[Int])]("t") {
      def a = column[Int]("a")
      def b = column[Option[Int]]("b")
      def * = a ~ b
    }
    T.ddl.create
    T.insertAll((1, Some(1)), (1, Some(2)), (1, Some(3)))
    T.insertAll((2, Some(1)), (2, Some(2)), (2, Some(5)))
    T.insertAll((3, Some(1)), (3, Some(9)))

    println("=========================================================== q0")
    val q0 = T.groupBy(_.a)
    val q1 = q0.map(_._2.length)
    val r0 = q1.list
    (r0: List[Int])
    println(q1.selectStatement)
    assertEquals(List(3, 3, 2), q1.list)

    println("=========================================================== q")
    val q = for {
      (k, v) <- T.groupBy(t => t.a)
    } yield (k, v.length, v.map(_.a).sum, v.map(_.b).sum)
    println(q.selectStatement)
    val r = q.list
    val rt = r: List[(Int, Int, Option[Int], Option[Int])]
    println(r)
    assertEquals(List((1, 3, Some(3), Some(6)), (2, 3, Some(6), Some(8)), (3, 2, Some(6), Some(10))), rt)
  }
}
