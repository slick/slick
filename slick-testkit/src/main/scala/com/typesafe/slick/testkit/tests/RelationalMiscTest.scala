package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{AsyncTest, RelationalTestDB}

class RelationalMiscTest extends AsyncTest[RelationalTestDB] {
  import tdb.profile.api.*

  def isNotAndOrTest = {
    class T(tag: Tag) extends Table[(String, String)](tag, "users") {
      def a = column[String]("a")
      def b = column[String]("b")
      def * = (a, b)
    }
    val ts = TableQuery[T]

    for {
      _ <- ts.schema.create
      _ <- ts ++= Seq(("1", "a"), ("2", "a"), ("3", "b"))

      q1 = for(t <- ts if t.a === "1" || t.a === "2") yield t
      _ <- q1.result.map(r => r.toSet shouldBe Set(("1", "a"), ("2", "a")))

      q2 = for(t <- ts if (t.a =!= "1") || (t.b =!= "a")) yield t
      _ <- q2.result.map(r => r.toSet shouldBe Set(("2", "a"), ("3", "b")))

      // No need to test that the unexpected result is actually unexpected
      // now that the compiler prints a warning about it

      q4 = for(t <- ts if t.a =!= "1" || t.b =!= "a") yield t
      _ <- q4.result.map(r => r.toSet shouldBe Set(("2", "a"), ("3", "b")))
    } yield ()
  }

  def testLike = {
    class T1(tag: Tag) extends Table[String](tag, "t1_2") {
      def a = column[String]("a")
      def * = a
    }
    val t1s = TableQuery[T1]

    for {
      _ <- t1s.schema.create
      _ <- t1s ++= Seq("foo", "bar", "foobar", "foo%")

      q1 = for { t1 <- t1s if t1.a like "foo" } yield t1.a
      _ <- q1.result.map(_ shouldBe List("foo"))

      q2 = for { t1 <- t1s if t1.a like "foo%" } yield t1.a
      _ <- q2.to[Set].result.map(_ shouldBe Set("foo", "foobar", "foo%"))

      _ <- ifCap(rcap.likeEscape) {
        val q3 = for { t1 <- t1s if t1.a.like("foo^%", '^') } yield t1.a
        q3.result.map(_ shouldBe List("foo%"))
      }
    } yield ()
  }

  def testSorting = {
    import slick.lifted.{Ordered, Shape}

    class T1(tag: Tag) extends Table[(String, String, String)](tag, "t1_3") {
      def a = column[String]("a")
      def b = column[String]("b")
      def c = column[String]("c")
      def * = (a, b, c)
    }
    val t1s = TableQuery[T1]

    implicit class TupledQueryExtensionMethods[E1, E2, U1, U2, C[_]](q: Query[(E1, E2), (U1, U2), C]) {
      def sortedValues(implicit ordered: (E1 => Ordered),
                       shape: Shape[FlatShapeLevel, E2, U2, E2]): Query[E2, U2, C] =
          q.sortBy(_._1).map(_._2)
    }

    for {
      _ <- t1s.schema.create
      _ <- t1s ++= Seq(("a2", "b2", "c2"), ("a1", "b1", "c1"))

      q1 = (for {
        t1 <- t1s
      } yield t1.c -> (t1.a, t1.b)).sortedValues

      _ <- q1.result.map(_ shouldBe List(("a1", "b1"), ("a2", "b2")))
    } yield ()
  }

  def testConditional = {
    class T1(tag: Tag) extends Table[(Int, Option[Int])](tag, "t1_conditional") {
      def a = column[Int]("a")
      def b = column[Option[Int]]("b")
      def * = (a, b)
    }
    val t1s = TableQuery[T1]

    for {
      _ <- t1s.schema.create
      _ <- t1s ++= Seq((1, Some(11)), (2, None), (3, Some(33)), (4, None))

      q1 = t1s.map { t1 => (t1.a, Case.If(t1.a < 3) Then 1 Else 0) }
      _ <- q1.to[Set].result.map(_ shouldBe Set((1, 1), (2, 1), (3, 0), (4, 0)))

      q2 = t1s.map { t1 => (t1.a, Case.If(t1.a < 3) Then 1) }
      _ <- q2.to[Set].result.map(_ shouldBe Set((1, Some(1)), (2, Some(1)), (3, None), (4, None)))

      q3 = t1s.map { t1 => (t1.a, Case.If(t1.a < 3) Then 1 If(t1.a < 4) Then 2 Else 0) }
      _ <- q3.to[Set].result.map(_ shouldBe Set((1, 1), (2, 1), (3, 2), (4, 0)))

      q4 = t1s.map { t1 => Case.If(t1.a < 3) Then t1.b Else t1.a.? }.to[Set]
      _ <- mark("q4", q4.result).map(_ shouldBe Set(Some(11), None, Some(3), Some(4)))
    } yield ()
  }

  def testCast = {
    class T1(tag: Tag) extends Table[(String, Int, Double)](tag, "t1_4") {
      def a = column[String]("a")
      def b = column[Int]("b")
      def c = column[Double]("c")
      def * = (a, b, c)
    }
    val t1s = TableQuery[T1]

    for {
      _ <- t1s.schema.create
      _ <- t1s ++= Seq(("foo", 1, 2.0), ("bar", 2, 2.0))

      q1 = t1s.map(t1 => t1.a ++ t1.b.asColumnOf[String])
      _ <- q1.to[Set].result.map(_ shouldBe Set("foo1", "bar2"))
      q2 = t1s.map(t1 => t1.c * t1.b.asColumnOf[Double])
      _ <- q2.to[Set].result.map(_ shouldBe Set(2.0, 4.0))
    } yield ()
  }

  def testOptionConversions = {
    class T1(tag: Tag) extends Table[(Int, Option[Int])](tag, "t1_optconv") {
      def a = column[Int]("a")
      def b = column[Option[Int]]("b")
      def * = (a, b)
    }
    val t1s = TableQuery[T1]

    for {
      _ <- t1s.schema.create
      _ <- t1s ++= Seq((1, Some(10)), (2, None))

      // GetOrElse in ResultSetMapping on client side
      q1 = for { t <- t1s } yield (t.a, t.b.getOrElse(0))
      _ <- q1.result.map(r => r.toSet shouldBe Set((1, 10), (2, 0)))

      // GetOrElse in query on the DB side
      q2 = for { t <- t1s } yield (t.a, t.b.getOrElse(0) + 1)
      _ <- q2.result.map(r => r.toSet shouldBe Set((1, 11), (2, 1)))
    } yield ()
  }

  def testInitErrors = {
    case class Id(toInt: Int)
    case class Customer(id: Id)
    // Before making `shaped` and `toNode` in `TableQuery` lazy,
    // putting `Tables` before `A` caused a StackOverflowException
    object Tables {
      val as = TableQuery[A]
      implicit val idMapper: BaseColumnType[Id] = MappedColumnType.base[Id, Int](_.toInt, Id)
    }
    class A(tag: Tag) extends Table[Customer](tag, "INIT_A") {
      def id = column[Id]("ID", O.PrimaryKey, O.AutoInc)(Tables.idMapper)
      import Tables.idMapper
      def * = id.mapTo[Customer]
    }
    Tables.as.schema

    case class Id2(toInt: Int)
    implicit val id2Mapper = null.asInstanceOf[BaseColumnType[Id2]]
    class B(tag: Tag) extends Table[Id2](tag, "INIT_A") {
      def id = column[Id2]("ID", O.PrimaryKey, O.AutoInc)
      def * = id
    }
    val bs = TableQuery[B]
    try {
      bs.map(_.id)
      bs.schema
      ???
    } catch {
      case t: NullPointerException if (t.getMessage ne null) && (t.getMessage contains "initialization order") =>
        // This is the expected error message from RelationalTableComponent.Table.column
    }

    try {
      MappedColumnType.base[Id, Int](_.toInt, Id)(implicitly, null.asInstanceOf[BaseColumnType[Int]])
      ???
    } catch {
      case t: NullPointerException if (t.getMessage ne null) && (t.getMessage contains "initialization order") =>
      // This is the expected error message from RelationalTypesComponent.MappedColumnTypeFactory.assertNonNullType
    }

    DBIO.successful(())
  }
}
