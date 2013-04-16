package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{RelationalTestDB, TestkitTest}

class RelationalMiscTest extends TestkitTest[RelationalTestDB] {
  import tdb.profile.simple._

  override val reuseInstance = true

  def isNotAndOrTest {
    object T extends Table[(String, String)]("users") {
      def a = column[String]("a")
      def b = column[String]("b")
      def * = a ~ b
    }

    T.ddl.create
    T ++= Seq(("1", "a"), ("2", "a"), ("3", "b"))

    val q1 = for(t <- T if t.a === "1" || t.a === "2") yield t
    assertEquals(Set(("1", "a"), ("2", "a")), q1.run.toSet)

    val q2 = for(t <- T if (t.a isNot "1") || (t.b isNot "a")) yield t
    assertEquals(Set(("2", "a"), ("3", "b")), q2.run.toSet)

    // No need to test that the unexpected result is actually unexpected
    // now that the compiler prints a warning about it

    val q4 = for(t <- T if t.a =!= "1" || t.b =!= "a") yield t
    assertEquals(Set(("2", "a"), ("3", "b")), q4.run.toSet)
  }

  def testLike {
    object T1 extends Table[String]("t1_2") {
      def a = column[String]("a")
      def * = a
    }

    T1.ddl.create
    T1 ++= Seq("foo", "bar", "foobar", "foo%")

    val q1 = for { t1 <- T1 if t1.a like "foo" } yield t1.a
    assertEquals(List("foo"), q1.run)

    val q2 = for { t1 <- T1 if t1.a like "foo%" } yield t1.a
    assertEquals(Set("foo", "foobar", "foo%"), q2.run.toSet)

    ifCap(rcap.likeEscape) {
      val q3 = for { t1 <- T1 if t1.a.like("foo^%", '^') } yield t1.a
      assertEquals(List("foo%"), q3.run)
    }
  }

  def testSorting {
    object T1 extends Table[(String, String, String)]("t1_3") {
      def a = column[String]("a")
      def b = column[String]("b")
      def c = column[String]("c")
      def * = a ~ b ~ c
    }

    T1.ddl.create
    T1 ++= Seq(("a2", "b2", "c2"), ("a1", "b1", "c1"))

    implicit class TupledQueryExtensionMethods[E1, E2, U1, U2](q: Query[(E1, E2), (U1, U2)]) {
      def sortedValues(implicit ordered: (E1 => scala.slick.lifted.Ordered),
                       shape: scala.slick.lifted.Shape[E2, U2, E2]): Query[E2, U2] =
          q.sortBy(_._1).map(_._2)
    }

    val q1 = (for {
      t1 <- T1
    } yield t1.c -> (t1.a, t1.b)).sortedValues

    assertEquals(List(("a1", "b1"), ("a2", "b2")), q1.run)
  }

  def testConditional {
    object T1 extends Table[Int]("t1_conditional") {
      def a = column[Int]("a")
      def * = a
    }

    T1.ddl.create
    T1 ++= Seq(1, 2, 3, 4)

    val q1 = T1.map { t1 => (t1.a, Case.If(t1.a < 3) Then 1 Else 0) }
    assertEquals(Set((1, 1), (2, 1), (3, 0), (4, 0)), q1.run.toSet)

    val q2 = T1.map { t1 => (t1.a, Case.If(t1.a < 3) Then 1) }
    assertEquals(Set((1, Some(1)), (2, Some(1)), (3, None), (4, None)), q2.run.toSet)

    val q3 = T1.map { t1 => (t1.a, Case.If(t1.a < 3) Then 1 If(t1.a < 4) Then 2 Else 0) }
    assertEquals(Set((1, 1), (2, 1), (3, 2), (4, 0)), q3.run.toSet)
  }

  def testCast {
    object T1 extends Table[(String, Int)]("t1_4") {
      def a = column[String]("a")
      def b = column[Int]("b")
      def * = a ~ b
    }

    T1.ddl.create
    T1 ++= Seq(("foo", 1), ("bar", 2))

    val q1 = T1.map(t1 => t1.a ++ t1.b.asColumnOf[String])
    val r1 = q1.run.toSet
    assertEquals(Set("foo1", "bar2"), r1)
  }

  def testOptionConversions {
    object T1 extends Table[(Int, Option[Int])]("t1_optconv") {
      def a = column[Int]("a")
      def b = column[Option[Int]]("b")
      def * = a ~ b
    }

    T1.ddl.create
    T1 ++= Seq((1, Some(10)), (2, None))

    // GetOrElse in ResultSetMapping on client side
    val q1 = for { t <- T1 } yield (t.a, t.b.getOrElse(0))
    assertEquals(Set((1, 10), (2, 0)), q1.run.toSet)

    // GetOrElse in query on the DB side
    val q2 = for { t <- T1 } yield (t.a, t.b.getOrElse(0) + 1)
    assertEquals(Set((1, 11), (2, 1)), q2.run.toSet)
  }
}
