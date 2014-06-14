package com.typesafe.slick.testkit.tests

import java.util.logging.Logger

import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}
import org.junit.Assert._

import scala.slick.ast._
import scala.slick.driver.PostgresDriver
import scala.slick.util.TreeDump

class WindowFunctionTest extends TestkitTest[JdbcTestDB] {

  import tdb.profile.simple._

  override val reuseInstance = true

  class Students(tag: Tag) extends Table[(Int, String)](tag, "managers") {
    def id = column[Int]("id")

    def name = column[String]("name")

    def * = (id, name)
  }

  lazy val students = TableQuery[Students]

  class ExamScores(tag: Tag) extends Table[(Int, Int, String, Int)](tag, "employees") {
    def id = column[Int]("id")

    def studentId = column[Int]("student_id")

    def subject = column[String]("subject")

    def score = column[Int]("score")

    def * = (id, studentId, subject, score)
  }

  lazy val examScores = TableQuery[ExamScores]

  class FrameClauseTest(tag: Tag) extends Table[(Int, Int)](tag, "frame_test_table") {
    def id = column[Int]("id")

    def score = column[Int]("score")

    def * = (id, score)
  }

  lazy val frameTestTable = TableQuery[FrameClauseTest]

  def testBasic(): Unit = {
    if (!tdb.driver.isInstanceOf[PostgresDriver])
      return
    (students.ddl ++ examScores.ddl).create

    val john = (0, "John")
    val kate = (1, "Kate")
    val johnHistoryScore = (0, 0, "history", 80)
    val johnMathScore = (1, 0, "math", 70)
    val kateHistoryScore = (2, 1, "history", 72)
    val kateMathScore = (3, 1, "math", 82)
    students.++=(Seq(john, kate))
    examScores.++=(Seq(johnHistoryScore, johnMathScore, kateMathScore, kateHistoryScore))

    // http://www.postgresql.org/docs/9.1/static/functions-window.html
    val rank = new TypedWindowFunctionSymbol[Option[Int]]("rank")
    val avg = new TypedWindowFunctionSymbolParams[Column[Int], Option[Double]]("avg")
    val lag = new TypedWindowFunctionSymbolParams[(Column[Int], Column[Int], Column[Option[Int]]), Option[Int]]("lag")

    type QueryResultType = (Students, ExamScores, Column[Option[Int]], Column[Option[Double]], Column[Option[Int]])

    def mapper(x: (Students, ExamScores)): QueryResultType = {
      val (a, b) = x

      val windowFunctionColumn =
        Query(a, b).
          sortBy { case (a: Students, b: ExamScores) => (b.score.desc, a.name.asc)}.
          groupBy { case (a: Students, b: ExamScores) => b.subject}.
          flatMap { case (_, t) => t}.map { case (a: Students, b: ExamScores) => ()}.
          windowFunction(rank)
      val windowFunctionColumn2 =
        Query(a, b).
          sortBy { case (a: Students, b: ExamScores) => (b.score.asc)}.
          map { case (a: Students, b: ExamScores) => (b.score)}.
          windowFunctionParams(avg)
      val windowFunctionColumn3 =
        Query(a, b).
          sortBy { case (a: Students, b: ExamScores) => (b.score.asc)}.
          map { case (a: Students, b: ExamScores) => (b.score, LiteralColumn(1), LiteralColumn[Option[Int]](None))}.
          windowFunctionParams(lag)
      (a, b, windowFunctionColumn, windowFunctionColumn2, windowFunctionColumn3)
    }

    def sortBy(x: QueryResultType) = {
      val (a, b, rank, avg, lag) = x
      b.id.asc
    }

    val queryOne = students.
      join(examScores).
      on(_.id === _.studentId).
      map(mapper).
      sortBy(sortBy)

    val result = queryOne.list
    val expected = Seq(
      (john, johnHistoryScore, Some(1), Some(74.0D), Some(72)), // (70+72+80) / 3 = 74
      (john, johnMathScore, Some(2), Some(70.0D), None), // 70 / 1 = 70
      (kate, kateHistoryScore, Some(2), Some(71.0D), Some(70)), // (70+72) / 2 = 71
      (kate, kateMathScore, Some(1), Some(76.0D), Some(80))) // (70+72+80+82)/ 4 = 76

    (students.ddl ++ examScores.ddl).drop

    assertEquals(expected, result)
  }

  def testFrameClauseWithCompiling(): Unit = {
    if (!tdb.driver.isInstanceOf[PostgresDriver])
      return
    (frameTestTable.ddl).create
    1.to(10).foreach(i => frameTestTable.+=((i, i * 10)))
    val sum = new TypedWindowFunctionSymbolParams[Column[Int], Int]("sum")
    def query(c: Column[Int]): Query[(Column[Int], Column[Int]), (Int, Int), Seq] = {
      // http://www.postgresql.org/docs/9.1/static/sql-expressions.html#SYNTAX-WINDOW-FUNCTIONS
      val frame = Option(RowFrame(PrecedingN(c), FollowingN(c)))
      frameTestTable.
        map { t => (
        t.id,
        Query(t).
          sortBy(t => t.score.asc).
          map(t => t.score).
          windowFunctionParams(sum, frame))
      }
    }
    val compiled = Compiled(query _)
    val expected = 1.to(10).zip(Seq((1 + 2) * 10) ++ 2.to(9).map(n => ((n - 1) + n + (n + 1)) * 10) ++ Seq((9 + 10) * 10))
    val result = compiled.apply(1).run
    (frameTestTable.ddl).drop
    assertEquals(expected, result)
  }
}
