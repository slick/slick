package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{RelationalTestDB, TestkitTest}

class NestingTest extends TestkitTest[RelationalTestDB] {
  import tdb.profile.simple._

  override val reuseInstance = true

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
      a ~ b ~ c <- ts.filter(_.a === 1).map(t => t.a ~ t.b ~ 4) unionAll ts.filter(_.a === 2).map(t => t.a ~ t.b ~ 5)
    } yield a ~ b ~ (c*2)
    assertEquals(res2, q2a.run.toSet)

    val q2b = for {
      (a, b, c) <- ts.filter(_.a === 1).map(t => (t.a, t.b, LiteralColumn(4))) unionAll ts.filter(_.a === 2).map(t => (t.a, t.b, LiteralColumn(5)))
    } yield a ~ b ~ (c*2)
    assertEquals(res2, q2b.run.toSet)

    val q2c = for {
      (a, b, c) <- ts.filter(_.a === 1).map(t => (t.a, t.b, 4)) unionAll ts.filter(_.a === 2).map(t => (t.a, t.b, 5))
    } yield a ~ b ~ (c*2)
    assertEquals(res2, q2c.run.toSet)
  }

  def testNestedOptions {
    class X(tag: Tag) extends Table[(Int, String, Option[String])](tag, "X_OPT") {
      def a = column[Int]("A")
      def b = column[String]("B")
      def c = column[Option[String]]("C")
      def * = (a, b, c)
    }
    val xs = TableQuery[X]

    val q = xs.sortBy(_.a)
    val r = Vector((1, "1", Some("a")), (2, "2", Some("b")), (3, "3", None))
    xs.ddl.create
    xs ++= r

    // Construct all kinds of Option Shapes
    implicitly[Shape[_, Rep[Int], _, _]]
    implicitly[Shape[_, Rep[Option[Int]], _, _]]
    implicitly[Shape[_, Rep[Option[Option[Int]]], _, _]]
    implicitly[Shape[_, Rep[Option[(Rep[Int], Rep[String])]], _, _]]
    implicitly[Shape[_, Rep[Option[X]], _, _]]

    // Construct all different kinds of Options
    val q1 = q.map(t => Rep.Some(t))
    val q1a2 = q.map(t => Rep.Some(Rep.Some(t)))
    val q2 = q.map(t => Rep.Some(t.a))
    val q2a2 = q.map(t => Rep.Some(Rep.Some(t.a)))
    val q3 = q.map(t => t.c)
    val q4 = q.map(t => Rep.Some(t.c))
    val q5 = q.map(t => (t.c, Rep.Some(t.b)))
    val q1t: Query[Rep[Option[X]], _, Seq] = q1
    val q1a2t: Query[Rep[Option[Option[X]]], _, Seq] = q1a2
    val q2t: Query[Rep[Option[Int]], _, Seq] = q2
    val q2a2t: Query[Rep[Option[Option[Int]]], _, Seq] = q2a2
    val q3t: Query[Rep[Option[String]], _, Seq] = q3
    val q4t: Query[Rep[Option[Option[String]]], _, Seq] = q4
    val q5t: Query[(Rep[Option[String]], Rep[Option[String]]), _, Seq] = q5

    assertEquals(r.map(t => Some(t)), q1.run)
    assertEquals(r.map(t => Some(Some(t))), q1a2.run)
    assertEquals(r.map(t => Some(t._1)), q2.run)
    assertEquals(r.map(t => Some(Some(t._1))), q2a2.run)
    assertEquals(r.map(t => t._3), q3.run)
    assertEquals(r.map(t => Some(t._3)), q4.run)
    assertEquals(r.map(t => (t._3, Some(t._2))), q5.run)

    // Get plain values out
    val q1b = q1.map(_.map(x => (x.a, x.b, x.c)).getOrElse((0, "", None: Option[String])))
    val q2b = q2.map(_.get)
    val q3b = q3.filter(_.isDefined).map(_.get)
    val q4b = q4.map(_.getOrElse(None: Option[String]))
    val q1bt: Query[(Rep[Int], Rep[String], Rep[Option[String]]), _, Seq] = q1b
    val q2bt: Query[Rep[Int], _, Seq] = q2b
    val q3bt: Query[Rep[String], _, Seq] = q3b
    val q4bt: Query[Rep[Option[String]], _, Seq] = q4b

    assertEquals(r.map(t => Some(t)).map(_.getOrElse((0, "", None: Option[String]))), q1b.run)
    assertEquals(r.map(t => Some(t._1)).map(_.get), q2b.run)
    assertEquals(r.map(t => t._3).filter(_.isDefined).map(_.get), q3b.run)
    assertEquals(r.map(t => Some(t._3)).map(_.getOrElse(None: Option[String])), q4b.run)

    // Unpack result types
    def r1: Seq[Option[(Int, String, Option[String])]] = q1.run
    def r2: Seq[Option[Int]] = q2.run
    def r3: Seq[Option[String]] = q3.run
    def r2b: Seq[Int] = q2b.run
    def r3b: Seq[String] = q3b.run

    // Perform Option-mapped operations
    val q2c = q2.map(io => io + 42)
    val q3c = q3.map(so => so ++ "x")

    assertEquals(r.map(t => Some(t._1)).map(_.map(_ + 42)), q2c.run)
    assertEquals(r.map(t => t._3).map(_.map(_ + "x")), q3c.run)

    // Use Option.map
    val q1d = q1.map(_.map(_.a))
    val q1d2 = q1.map(_.map(x => (x.a, x.b, x.c)))
    val q2d = q2.map { io: Rep[Option[Int]] =>
      io.map { i: Rep[Int] =>
        i + 1
      }
    }
    val q3d = q3.map(_.map(s => (s, s, 1)))
    val q4d = q4.map(_.filter(_.isDefined).map(_.getOrElse("")))
    val q1dt: Query[Rep[Option[Int]], _, Seq] = q1d
    val q1d2t: Query[Rep[Option[(Rep[Int], Rep[String], Rep[Option[String]])]], _, Seq] = q1d2
    val q2dt: Query[Rep[Option[Int]], _, Seq] = q2d
    val q3dt: Query[Rep[Option[(Rep[String], Rep[String], ConstColumn[Int])]], _, Seq] = q3d
    val q4dt: Query[Rep[Option[String]], _, Seq] = q4d

    assertEquals(r.map(t => Some(t)).map(_.map(_._1)), q1d.run)
    assertEquals(r.map(t => Some(t)).map(_.map(x => (x._1, x._2, x._3))), q1d2.run)
    assertEquals(r.map(t => Some(t._1)).map(_.map(_ + 1)), q2d.run)
    assertEquals(r.map(t => t._3).map(_.map(s => (s, s, 1))), q3d.run)
    assertEquals(r.map(t => Some(t._3)).map(_.filter(_.isDefined).map(_.get)), q4d.run)

    // Use Option.flatMap
    val q1e1 = q1.map { to => to.flatMap { t => Rep.Some(t.b) }}
    val q1e2 = q1.map { to => to.flatMap { t => t.c }}
    val q1e3 = q1.map(to => Rep.Some(to)).map(_.flatMap(identity))
    val q2e = q2.map { io => io.flatMap { i => Rep.Some(i) }}
    val q1e1t: Query[Rep[Option[String]], _, Seq] = q1e1
    val q1e2t: Query[Rep[Option[String]], _, Seq] = q1e2
    val q2et: Query[Rep[Option[Int]], _, Seq] = q2e

    assertEquals(r.map(t => Some(t)).map { to => to.flatMap { t => Some(t._2) }}, q1e1.run)
    assertEquals(r.map(t => Some(t)).map { to => to.flatMap { t => t._3 }}, q1e2.run)
    assertEquals(r.map(t => Some(t)).map(to => Some(to)).map(_.flatMap(identity)), q1e3.run)
    assertEquals(r.map(t => Some(t._1)).map { io => io.flatMap { i => Some(i) }}, q2e.run)

    // Use Option.flatten
    val q1f1 = q1.map { to => Rep.Some(to) }
    val q1f2 = q1.map { to => Rep.Some(to).flatten }
    val q1f3 = q1.map { to => Rep.Some(to) }.map(_.flatten)
    val q2f1 = q2.map { io => Rep.Some(io) }
    val q2f2 = q2.map { io => Rep.Some(io).flatten }
    val q2f3 = q2.map { io => Rep.Some(io) }.map(_.flatten)
    val q1f1t: Query[Rep[Option[Option[X]]], _, Seq] = q1f1
    val q1f2t: Query[Rep[Option[X]], _, Seq] = q1f2
    val q1f3t: Query[Rep[Option[X]], _, Seq] = q1f3
    val q2f1t: Query[Rep[Option[Option[Int]]], _, Seq] = q2f1
    val q2f2t: Query[Rep[Option[Int]], _, Seq] = q2f2
    val q2f3t: Query[Rep[Option[Int]], _, Seq] = q2f3

    assertEquals(Vector(Some(Some((1,"1",Some("a")))), Some(Some((2,"2",Some("b")))), Some(Some((3,"3",None)))), q1f1.run)
    assertEquals(r.map(t => Some(t)).map { to => Some(to).flatten }, q1f2.run)
    assertEquals(r.map(t => Some(t)).map { to => Some(to) }.map(_.flatten), q1f3.run)
    assertEquals(r.map(t => Some(t._1)).map { io => Some(io) }, q2f1.run)
    assertEquals(r.map(t => Some(t._1)).map { io => Some(io).flatten }, q2f2.run)
    assertEquals(r.map(t => Some(t._1)).map { io => Some(io) }.map(_.flatten), q2f3.run)
  }
}
