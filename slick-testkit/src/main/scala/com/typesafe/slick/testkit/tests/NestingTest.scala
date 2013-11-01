package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{RelationalTestDB, TestkitTest}

class NestingTest extends TestkitTest[RelationalTestDB] {
  import tdb.profile.simple._

  def testNestedTuples {
    import TupleMethods._

    class T(tag: Tag) extends Table[(Int, String, String)](tag, "T") {
      def a = column[Int]("A")
      def b = column[String]("B")
      def c = column[String]("C")
      def * = (a, b, c)
    }
    val ts = TableQuery[T]

    ts.ddl.create
    ts ++= Seq((1, "1", "a"), (2, "2", "b"), (3, "3", "c"))

    val res1 = List(
      (1, "1", "a", 5), (2, "2", "a", 5), (3, "3", "a", 5),
      (1, "1", "b", 5), (2, "2", "b", 5), (3, "3", "b", 5),
      (1, "1", "c", 5), (2, "2", "c", 5), (3, "3", "c", 5)
    )
    val res1b = res1.map { case (a, b, c, d) => ((a, b), (c, d)) }

    val q1a = (for {
      (a, b) <- ts.map(t => (t.a, t.b))
      c <- ts.map(t => t.c)
    } yield a ~ b ~ c ~ 5).sortBy(t => t._3 ~ t._1)
    assertEquals(res1, q1a.run)

    val q1c = (for {
      a ~ b <- ts.map(t => (t.a, t.b))
      c <- ts.map(t => t.c)
    } yield (a, b, c, LiteralColumn(5))).sortBy(t => t._3 ~ t._1)
    assertEquals(res1, q1c.run)

    val q1d = (for {
      (a, b) <- ts.map(t => (t.a, t.b))
      c <- ts.map(t => t.c)
    } yield ((a, b), (c, 5))).sortBy(t => t._2._1 ~ t._1._1)
    assertEquals(res1b, q1d.run)

    val res2 = Set((1, "1", 8), (2, "2", 10))

    val q2a = for {
      a ~ b ~ c <- ts.where(_.a === 1).map(t => t.a ~ t.b ~ 4) unionAll ts.where(_.a === 2).map(t => t.a ~ t.b ~ 5)
    } yield a ~ b ~ (c*2)
    assertEquals(res2, q2a.run.toSet)

    val q2b = for {
      (a, b, c) <- ts.where(_.a === 1).map(t => (t.a, t.b, LiteralColumn(4))) unionAll ts.where(_.a === 2).map(t => (t.a, t.b, LiteralColumn(5)))
    } yield a ~ b ~ (c*2)
    assertEquals(res2, q2b.run.toSet)

    val q2c = for {
      (a, b, c) <- ts.where(_.a === 1).map(t => (t.a, t.b, 4)) unionAll ts.where(_.a === 2).map(t => (t.a, t.b, 5))
    } yield a ~ b ~ (c*2)
    assertEquals(res2, q2c.run.toSet)
  }
}
