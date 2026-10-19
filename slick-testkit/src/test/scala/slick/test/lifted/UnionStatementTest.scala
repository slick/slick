package slick.test.lifted

import org.junit.Assert.*
import org.junit.Test

import slick.jdbc.H2Profile.api.*

/** Literals and computed values in a table's `*` projection must be computed by each side of a union (#1559). */
class UnionStatementTest {
  abstract class AB(tag: Tag, name: String) extends Table[(Int, Boolean)](tag, name) {
    def a = column[Int]("a")
  }
  class TF(tag: Tag) extends AB(tag, "tf") {
    def * = (a, LiteralColumn(false))
  }
  class TT(tag: Tag) extends AB(tag, "tt") {
    def * = (a, LiteralColumn(true))
  }
  class Plain(tag: Tag, name: String) extends Table[(Int, Int)](tag, name) {
    def a = column[Int]("a")
    def b = column[Int]("b")
    def * = (a, b)
  }
  class Opt(tag: Tag, name: String) extends Table[(Option[Int], Int)](tag, name) {
    def a = column[Int]("a")
    def b = column[Int]("b")
    def * = (a.?, b)
  }
  val tfs = TableQuery[TF]
  val tts = TableQuery[TT]
  val p1 = TableQuery(new Plain(_, "p1"))
  val p2 = TableQuery(new Plain(_, "p2"))
  val o1 = TableQuery(new Opt(_, "o1"))
  val o2 = TableQuery(new Opt(_, "o2"))

  private def sql(q: Query[?, ?, Seq]) = q.result.statements.head.replaceAll("\\s+", " ")

  @Test def testLiteralsPerSide(): Unit = {
    assertEquals(
      """( select "a" as x2, false as x3 from "tf" ) union ( select "a" as x2, true as x3 from "tt" )""",
      sql(tfs.union(tts)))
    assertEquals(
      """( select "a" as x2, false as x3 from "tf" where "a" > 1 ) union all ( select "a" as x2, true as x3 from "tt" where "a" > 1 )""",
      sql((tfs ++ tts).filter(_.a > 1)))
    // Each table's literal is selected from that table on both sides of a union of joins
    val joins = sql(tfs.join(tts).on(_.a < _.a).union(tts.join(tfs).on(_.a < _.a)))
    for (side <- joins.split(" union ")) {
      assertTrue(joins, side.matches(""".*false as x\d+ from "tf".*""") && side.matches(""".*true as x\d+ from "tt".*"""))
    }
  }

  // Unions without non-column elements in the star projections of their rows are not rewritten
  @Test def testUnchangedStatements(): Unit = {
    assertEquals(
      """( select "a" as x2, "b" as x3 from "p1" ) union ( select "a" as x2, "b" as x3 from "p2" )""",
      sql(p1.union(p2)))
    assertEquals(
      """( select "a" as x2, "b" as x3 from "o1" ) union ( select "a" as x2, "b" as x3 from "o2" )""",
      sql(o1.union(o2)))
    assertEquals(
      """( select x2."a" as x3, x2."b" as x4, x5."a" as x6, x5."b" as x7 from "p1" x2, "p2" x5 where x2."a" < x5."a" ) """ +
        """union ( select x8."a" as x3, x8."b" as x4, x9."a" as x6, x9."b" as x7 from "p2" x8, "p1" x9 where x8."a" < x9."a" )""",
      sql(p1.join(p2).on(_.a < _.a).union(p2.join(p1).on(_.a < _.a))))
    assertEquals(
      """( select "a" as x2 from "tf" ) union ( select "a" as x2 from "tt" )""",
      sql(tfs.map(_.a).union(tts.map(_.a))))
  }
}
