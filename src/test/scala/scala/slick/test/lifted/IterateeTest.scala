package scala.slick.test.lifted

import org.junit.Test
import org.junit.Assert._
import scala.slick.lifted._
import scala.slick.session._
import scala.slick.session.Database.threadLocalSession
import scala.slick.util.iter._
import scala.slick.testutil._
import scala.slick.testutil.TestDB._

object IterateeTest extends DBTestObject(H2Mem)

class IterateeTest(val tdb: TestDB) extends DBTest {
  import tdb.profile.Table
  import tdb.profile.Implicit._

  object A extends Table[(String, Int)]("a") {
    def s = column[String]("s", O.PrimaryKey)
    def i = column[Int]("i")
    def * = s ~ i
  }

  @Test def test() = db withSession {
    A.ddl.create
    A.insertAll(("a", 1), ("b", 2), ("c", 3), ("d", 4))

    val q1 = Query(A).sortBy(_.s)

    /* Sum i values until > 5 with foldLeft().
     * There is no way to stop early when the limit has been reached */
    var seen1 = ""
    val r1 = q1.foldLeft(0) { case (z, (s, i)) =>
      seen1 += s
      if(z > 5) z else z + i
    }
    assertEquals(6, r1)
    assertEquals("abcd", seen1)

    /* Do the same with enumerate() and terminate when done */
    var seen2 = ""
    def step(z: Int): Input[(String, Int)] => IterV[(String, Int), Int] = {
      case El((s, i)) =>
        seen2 += s
        if(z+i > 5) Done(z+i, Empty) else Cont(step(z+i))
      case Empty =>
        seen2 += "_"
        Cont(step(z))
      case EOF =>
        seen2 += "."
        Done(z, EOF)
    }
    val r2 = q1.enumerate(Cont(step(0))).run
    assertEquals(6, r2)
    assertEquals("abc", seen2)

    /* Using a fold on the Input and some syntactic sugar */
    def step2(z: Int): Cont.K[(String, Int), Int] = _.fold({ case (s, i) =>
      if(z+i > 5) Done(z+i) else Cont(step2(z+i))
    }, Cont(step2(z)), Done(z, EOF))
    val r3 = q1.enumerate(Cont(step2(0))).run
    assertEquals(6, r3)
  }
}
