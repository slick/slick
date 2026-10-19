package slick.test.jdbc

import org.junit.Assert._
import org.junit.Test

import slick.jdbc.DerbyProfile.api._

/** Derby rejects an untyped NULL literal outside of the SELECT clause, so it must be cast. */
class DerbyNullLiteralTest {
  class T(tag: Tag) extends Table[(Int, Option[Int], Option[String])](tag, "t") {
    def a = column[Int]("a")
    def b = column[Option[Int]]("b")
    def s = column[Option[String]]("s")
    def * = (a, b, s)
  }
  val ts = TableQuery[T]
  val none: Option[Int] = None
  val noneS: Option[String] = None

  def sql(q: Query[?, ?, Seq]): String = q.result.statements.mkString.replaceAll("\\s+", " ")

  @Test def testNullLiteralIsCast(): Unit = {
    assertEquals("""select "a" from "t" where "b" = cast(null as INTEGER)""",
      sql(ts.filter(_.b === none).map(_.a)))
    assertEquals("""select "a" from "t" where not ("b" = cast(null as INTEGER))""",
      sql(ts.filter(_.b =!= none).map(_.a)))
    assertEquals("""select "a" from "t" where cast(null as INTEGER) = "b"""",
      sql(ts.filter(t => LiteralColumn(none) === t.b).map(_.a)))
    assertEquals("""select "a" from "t" where "s" = cast(null as VARCHAR(254))""",
      sql(ts.filter(_.s === noneS).map(_.a)))
  }

  @Test def testBoundNullStaysBindVariable(): Unit =
    assertEquals("""select "a" from "t" where "b" = ?""", sql(ts.filter(_.b === none.bind).map(_.a)))
}
