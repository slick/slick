package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{RelationalTestDB, TestkitTest}

class RelationalMiscTest extends TestkitTest[RelationalTestDB] {
  import tdb.profile.simple._

  override val reuseInstance = true

  def isNotAndOrTest {
    class T(tag: Tag) extends Table[(String, String)](tag, "users") {
      def a = column[String]("a")
      def b = column[String]("b")
      def * = (a, b)
    }
    val ts = TableQuery[T]

    ts.ddl.create
    ts ++= Seq(("1", "a"), ("2", "a"), ("3", "b"))

    val q1 = for(t <- ts if t.a === "1" || t.a === "2") yield t
    assertEquals(Set(("1", "a"), ("2", "a")), q1.run.toSet)

    val q2 = for(t <- ts if (t.a isNot "1") || (t.b isNot "a")) yield t
    assertEquals(Set(("2", "a"), ("3", "b")), q2.run.toSet)

    // No need to test that the unexpected result is actually unexpected
    // now that the compiler prints a warning about it

    val q4 = for(t <- ts if t.a =!= "1" || t.b =!= "a") yield t
    assertEquals(Set(("2", "a"), ("3", "b")), q4.run.toSet)
  }

  def testLike {
    class T1(tag: Tag) extends Table[String](tag, "t1_2") {
      def a = column[String]("a")
      def * = a
    }
    val t1s = TableQuery[T1]

    t1s.ddl.create
    t1s ++= Seq("foo", "bar", "foobar", "foo%")

    val q1 = for { t1 <- t1s if t1.a like "foo" } yield t1.a
    assertEquals(List("foo"), q1.run)

    val q2 = for { t1 <- t1s if t1.a like "foo%" } yield t1.a
    assertEquals(Set("foo", "foobar", "foo%"), q2.run.toSet)

    ifCap(rcap.likeEscape) {
      val q3 = for { t1 <- t1s if t1.a.like("foo^%", '^') } yield t1.a
      assertEquals(List("foo%"), q3.run)
    }
  }

  def testSorting {
    import scala.slick.lifted.{Shape, ShapeLevel, Ordered}

    class T1(tag: Tag) extends Table[(String, String, String)](tag, "t1_3") {
      def a = column[String]("a")
      def b = column[String]("b")
      def c = column[String]("c")
      def * = (a, b, c)
    }
    val t1s = TableQuery[T1]

    t1s.ddl.create
    t1s ++= Seq(("a2", "b2", "c2"), ("a1", "b1", "c1"))

    implicit class TupledQueryExtensionMethods[E1, E2, U1, U2](q: Query[(E1, E2), (U1, U2)]) {
      def sortedValues(implicit ordered: (E1 => Ordered),
                       shape: Shape[ShapeLevel.Flat, E2, U2, E2]): Query[E2, U2] =
          q.sortBy(_._1).map(_._2)
    }

    val q1 = (for {
      t1 <- t1s
    } yield t1.c -> (t1.a, t1.b)).sortedValues

    assertEquals(List(("a1", "b1"), ("a2", "b2")), q1.run)
  }

  def testConditional {
    class T1(tag: Tag) extends Table[Int](tag, "t1_conditional") {
      def a = column[Int]("a")
      def * = a
    }
    val t1s = TableQuery[T1]

    t1s.ddl.create
    t1s ++= Seq(1, 2, 3, 4)

    val q1 = t1s.map { t1 => (t1.a, Case.If(t1.a < 3) Then 1 Else 0) }
    assertEquals(Set((1, 1), (2, 1), (3, 0), (4, 0)), q1.run.toSet)

    val q2 = t1s.map { t1 => (t1.a, Case.If(t1.a < 3) Then 1) }
    assertEquals(Set((1, Some(1)), (2, Some(1)), (3, None), (4, None)), q2.run.toSet)

    val q3 = t1s.map { t1 => (t1.a, Case.If(t1.a < 3) Then 1 If(t1.a < 4) Then 2 Else 0) }
    assertEquals(Set((1, 1), (2, 1), (3, 2), (4, 0)), q3.run.toSet)
  }

  def testCast {
    class T1(tag: Tag) extends Table[(String, Int)](tag, "t1_4") {
      def a = column[String]("a")
      def b = column[Int]("b")
      def * = (a, b)
    }
    val t1s = TableQuery[T1]

    t1s.ddl.create
    t1s ++= Seq(("foo", 1), ("bar", 2))

    val q1 = t1s.map(t1 => t1.a ++ t1.b.asColumnOf[String])
    val r1 = q1.run.toSet
    assertEquals(Set("foo1", "bar2"), r1)
  }

  def testOptionConversions {
    class T1(tag: Tag) extends Table[(Int, Option[Int])](tag, "t1_optconv") {
      def a = column[Int]("a")
      def b = column[Option[Int]]("b")
      def * = (a, b)
    }
    val t1s = TableQuery[T1]

    t1s.ddl.create
    t1s ++= Seq((1, Some(10)), (2, None))

    // GetOrElse in ResultSetMapping on client side
    val q1 = for { t <- t1s } yield (t.a, t.b.getOrElse(0))
    assertEquals(Set((1, 10), (2, 0)), q1.run.toSet)

    // GetOrElse in query on the DB side
    val q2 = for { t <- t1s } yield (t.a, t.b.getOrElse(0) + 1)
    assertEquals(Set((1, 11), (2, 1)), q2.run.toSet)
  }
}
