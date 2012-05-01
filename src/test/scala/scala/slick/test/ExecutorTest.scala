package scala.slick.test

import org.junit.Test
import org.junit.Assert._
import scala.slick.ql._
import scala.slick.driver.{ExtendedTable => Table}
import scala.slick.test.util._
import scala.slick.test.util.TestDB._
import scala.slick.session.Database.threadLocalSession

object ExecutorTest extends DBTestObject(H2Mem)

class ExecutorTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit._

  @Test def test() {

    val T = new Table[Int]("t") {
      def a = column[Int]("a")
      def * = a
    }

    db withSession {
      T.ddl.create
      T.insertAll(2, 3, 1, 5, 4)

      val q = T.sortBy(_.a).map(_.a)

      val r1 = q.list
      val r1t: List[Int] = r1
      assertEquals(List(1, 2, 3, 4, 5), r1t)

      val r2 = q.run
      val r2t: Seq[Int] = r2
      assertEquals(List(1, 2, 3, 4, 5), r2t)

      val r3 = q.length.run
      val r3t: Int = r3
      assertEquals(5, r3t)
    }
  }
}
