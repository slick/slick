package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{RelationalTestDB, TestkitTest}

class WithClauseTest extends TestkitTest[RelationalTestDB] {
  // for now(20140601), h2, derby, hsql, sqlite3.7 don't support the With Clause.
  // so skip sql execution.

  import tdb.profile.simple._

  override val reuseInstance = true

  case class FibonacciModel(var fibA: Int, var fibB: Int, var seed: Int, var num: Int)

  class FibonacciTable(t: Tag, n: String) extends Table[FibonacciModel](t, n) {
    def fibA = column[Int]("fiba")

    def fibB = column[Int]("fibb")

    def seed = column[Int]("seed")

    def num = column[Int]("num")

    override def * = (fibA, fibB, seed, num) <>(FibonacciModel.tupled, FibonacciModel.unapply)
  }

  def testBasic {
    val table = TableQuery(new FibonacciTable(_, "fibtest_basic"))
    table.ddl.create
    table.+=(FibonacciModel(0, 0, 1, 1))

    val withDeclarationFib = table.rename("with_fib")
    val query = withDeclarationFib.`with`(withDeclarationFib, table)
//    val actual = query.run
    table.ddl.drop
//    assertEquals(FibonacciModel(0, 0, 1, 1), actual.head)
  }

  def testMultiNested {
    // sqlite3.8, postgres support nested the With Clause.
    // but oracle, sqlserver don't support it.
    val table = TableQuery(new FibonacciTable(_, "fibtest_multinested"))
    table.ddl.create
    table.+=(FibonacciModel(0, 1, 2, 3))
    table.+=(FibonacciModel(1, 2, 3, 4))
    table.+=(FibonacciModel(2, 3, 4, 5))

    val withDeclaration3 = table.rename("with3")
    val nestedWith = withDeclaration3.`with`(withDeclaration3, table.filter(_.fibA === 1))

    val withDeclaration1 = table.rename("with1")
    val withDeclaration2 = table.rename("with2")
    val query = withDeclaration1.union(withDeclaration2).sortBy(t => t.fibA).
      `with`(withDeclaration1, table.filter(_.fibA === 0)).
      `with`(withDeclaration2, nestedWith)
//    val actual = query.run
    table.ddl.drop

//    val expected = List(FibonacciModel(0, 1, 2, 3), FibonacciModel(1, 2, 3, 4))
//    assertEquals(expected, actual)
  }

  def testSubUnion {
    // test for Union's childOfWithClause flag.
    val table = TableQuery(new FibonacciTable(_, "fibtest_subunion"))
    table.ddl.create
    table.+=(FibonacciModel(0, 0, 1, 1))

    val first = table.map(t => (t.fibA, t.fibB, t.seed, t.num))
    def fibonacciQuery(first: Query[(Column[Int], Column[Int], Column[Int], Column[Int]), (Int, Int, Int, Int), Seq])(n: Column[Option[Int]]) = {
      val withDeclarationFib = table.rename("fib")
      val second = withDeclarationFib.
        filter(t ⇒ t.num < n).
        map(t ⇒ (t.seed + t.fibA, t.fibA + t.fibB, t.fibA, t.num + 1))
      withDeclarationFib.`with`(withDeclarationFib, first.unionAll(second)).map(_.fibA)
    }
    def generateExpectedResult(n: Int) = {
      var fibs = List(FibonacciModel(0, 0, 1, 1))
      1.until(n).foreach { _ =>
        val recently = fibs.head
        fibs = FibonacciModel(recently.seed + recently.fibA, recently.fibA + recently.fibA, recently.fibA, recently.num + 1) :: fibs
      }
      fibs.map(_.fibA).reverse
    }
    val compiled = Compiled(fibonacciQuery(first) _)
//    assert(generateExpectedResult(8) == compiled.apply(Option(8)).run)
  }
}
