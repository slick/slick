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
}
