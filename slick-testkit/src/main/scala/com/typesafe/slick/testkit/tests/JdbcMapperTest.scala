package com.typesafe.slick.testkit.tests


import java.sql.ResultSet

import scala.reflect.ClassTag

import com.typesafe.slick.testkit.util.{AsyncTest, JdbcTestDB}


//noinspection ScalaUnusedSymbol
class JdbcMapperTest extends AsyncTest[JdbcTestDB] {

  import tdb.profile.api.*

  def testMappedEntity = {
    import TupleMethods.*

    case class User(id: Option[Int], first: String, last: String)
    case class Foo[T](value: T)

    class Users(tag: Tag) extends Table[User](tag, "users") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def first = column[String]("first")
      def last = column[String]("last")
      def * = (id.? ~: baseProjection).mapTo[User]
      def baseProjection = first ~ last
      def forUpdate =
        baseProjection.shaped.<>({ case (f, l) => User(None, f, l) }, { (u: User) => Some((u.first, u.last)) })
      def asFoo = forUpdate.<>((u: User) => Foo(u), (f: Foo[User]) => Some(f.value))
    }
    object users extends TableQuery(new Users(_)) {
      val byID = this.findBy(_.id)
    }

    val updateQ = users.filter(_.id === 2.bind).map(_.forUpdate)
    updateQ.updateStatement.length.should(_ > 0)

    val q1 = for {
      _ <- users
      u2 <- users
    } yield u2

    seq(
      users.schema.create,
      users.map(_.baseProjection) += ("Homer", "Simpson"),
      users ++= Seq(
        User(None, "Marge", "Bouvier"),
        User(None, "Carl", "Carlson")
      ),
      users.map(_.asFoo) += Foo(User(None, "Lenny", "Leonard")),
      users.filter(_.last inSet Set("Bouvier", "Ferdinand")).size.result.map(_ shouldBe 1),
      updateQ.update(User(None, "Marge", "Simpson")),
      Query(users.filter(_.id === 1).exists).result.head.map(_ shouldBe true),
      users
        .filter(_.id.between(1, 2))
        .to[Set]
        .result
        .map(_.shouldBe(Set(User(Some(1), "Homer", "Simpson"), User(Some(2), "Marge", "Simpson")))),
      users
        .filter(_.id.between(1, 2))
        .map(_.asFoo)
        .to[Set]
        .result
        .map(_ shouldBe Set(Foo(User(None, "Homer", "Simpson")), Foo(User(None, "Marge", "Simpson")))),
      users.byID(3).result.head.map(_ shouldBe User(Some(3), "Carl", "Carlson")),
      q1.result.head.map(_.should(_.isInstanceOf[User]))
    )
  }

  def testUpdate = {
    case class Data(a: Int, b: Int)

    class T(tag: Tag) extends Table[Data](tag, "T") {
      def a = column[Int]("A")
      def b = column[Int]("B")
      def * = (a, b).mapTo[Data]
    }
    val ts = TableQuery[T]

    val updateQ = ts.filter(_.a === 1)
    val updateQ2 = ts.filter(_.a === 3).map(identity)

    seq(
      ts.schema.create,
      ts ++= Seq(Data(1, 2), Data(3, 4), Data(5, 6)),
      updateQ.update(Data(7, 8)),
      updateQ2.update(Data(9, 10)),
      ts.to[Set].result.map(_.shouldBe(Set(Data(7, 8), Data(9, 10), Data(5, 6))))
    )
  }

  def testWideMappedEntity = {
    import slick.collection.heterogeneous.*

    case class Part(i1: Int, i2: Int, i3: Int, i4: Int, i5: Int, i6: Int)
    case class Whole(id: Int, p1: Part, p2: Part, p3: Part, p4: Part)
    case class BigCase(id: Int,
                       p1i1: Int, p1i2: Int, p1i3: Int, p1i4: Int, p1i5: Int, p1i6: Int,
                       p2i1: Int, p2i2: Int, p2i3: Int, p2i4: Int, p2i5: Int, p2i6: Int,
                       p3i1: Int, p3i2: Int, p3i3: Int, p3i4: Int, p3i5: Int, p3i6: Int,
                       p4i1: Int, p4i2: Int, p4i3: Int, p4i4: Int, p4i5: Int, p4i6: Int)

    class T(tag: Tag) extends Table[Whole](tag, "t_wide") {
      def id = column[Int]("id", O.PrimaryKey)
      def p1i1 = column[Int]("p1i1")
      def p1i2 = column[Int]("p1i2")
      def p1i3 = column[Int]("p1i3")
      def p1i4 = column[Int]("p1i4")
      def p1i5 = column[Int]("p1i5")
      def p1i6 = column[Int]("p1i6")
      def p2i1 = column[Int]("p2i1")
      def p2i2 = column[Int]("p2i2")
      def p2i3 = column[Int]("p2i3")
      def p2i4 = column[Int]("p2i4")
      def p2i5 = column[Int]("p2i5")
      def p2i6 = column[Int]("p2i6")
      def p3i1 = column[Int]("p3i1")
      def p3i2 = column[Int]("p3i2")
      def p3i3 = column[Int]("p3i3")
      def p3i4 = column[Int]("p3i4")
      def p3i5 = column[Int]("p3i5")
      def p3i6 = column[Int]("p3i6")
      def p4i1 = column[Int]("p4i1")
      def p4i2 = column[Int]("p4i2")
      def p4i3 = column[Int]("p4i3")
      def p4i4 = column[Int]("p4i4")
      def p4i5 = column[Int]("p4i5")
      def p4i6 = column[Int]("p4i6")
      // Composable bidirectional mappings
      def m1 = (
        id,
        (p1i1, p1i2, p1i3, p1i4, p1i5, p1i6).mapTo[Part],
        (p2i1, p2i2, p2i3, p2i4, p2i5, p2i6).mapTo[Part],
        (p3i1, p3i2, p3i3, p3i4, p3i5, p3i6).mapTo[Part],
        (p4i1, p4i2, p4i3, p4i4, p4i5, p4i6).mapTo[Part]
      ).mapTo[Whole]
      // Manually composed mapping functions
      def m2 = (
        id,
        (p1i1, p1i2, p1i3, p1i4, p1i5, p1i6),
        (p2i1, p2i2, p2i3, p2i4, p2i5, p2i6),
        (p3i1, p3i2, p3i3, p3i4, p3i5, p3i6),
        (p4i1, p4i2, p4i3, p4i4, p4i5, p4i6)
        ).shaped.<>({ case (id, p1, p2, p3, p4) =>
        // We could do this without .shaped but then we'd have to write a type annotation for the parameters
        Whole(
          id,
          (Part.apply _).tupled(p1),
          (Part.apply _).tupled(p2),
          (Part.apply _).tupled(p3),
          (Part.apply _).tupled(p4)
        )
      }, { (w: Whole) =>
        def f(p: Part) = (p.i1, p.i2, p.i3, p.i4, p.i5, p.i6)
        Some((w.id, f(w.p1), f(w.p2), f(w.p3), f(w.p4)))
      })
      // HList-based wide case class mapping
      def m3 = (
        id ::
        p1i1 :: p1i2 :: p1i3 :: p1i4 :: p1i5 :: p1i6 ::
        p2i1 :: p2i2 :: p2i3 :: p2i4 :: p2i5 :: p2i6 ::
        p3i1 :: p3i2 :: p3i3 :: p3i4 :: p3i5 :: p3i6 ::
        p4i1 :: p4i2 :: p4i3 :: p4i4 :: p4i5 :: p4i6 :: HNil
      ).mapTo[BigCase]
      def * = m1
    }
    val ts = TableQuery[T]

    val oData = Whole(0,
      Part(11, 12, 13, 14, 15, 16),
      Part(21, 22, 23, 24, 25, 26),
      Part(31, 32, 33, 34, 35, 36),
      Part(41, 42, 43, 44, 45, 46)
    )

    seq(
      ts.schema.create,
      ts += oData,
      ts.result.head.map(_ shouldBe oData),
      ts.map(_.m2).result.head.map(_.shouldBe(oData)),
      ts.map(_.m3).result.head
        .map { bigCase =>
          bigCase.shouldBe(
            BigCase(0, 11, 12, 13, 14, 15, 16, 21, 22, 23, 24, 25, 26, 31, 32, 33, 34, 35, 36, 41, 42, 43, 44, 45, 46)
          )
        }
    )
  }

  def testNestedMappedEntity = {
    case class Part1(i1: Int, i2: String)
    case class Part2(i1: String, i2: Int)
    case class Whole(p1: Part1, p2: Part2)

    class T(tag: Tag) extends Table[Whole](tag, "t_nested") {
      def p1 = column[Int]("p1")
      def p2 = column[String]("p2")
      def p3 = column[String]("p3")
      def p4 = column[Int]("p4")
      def part1 = (p1,p2).mapTo[Part1]
      def part2 = (p3,p4).mapTo[Part2]
      def * = (part1, part2).mapTo[Whole]
    }
    val T = TableQuery[T]

    val data = Seq(
      Whole(
        Part1(1, "2"),
        Part2("3", 4)
      )
    )

    T.schema.create >> (T ++= data) >> T.result.map(_ shouldBe data)
  }

  def testMappedJoin = {
    case class A(id: Int, value: Int)
    case class B(id: Int, value: Option[String])

    class ARow(tag: Tag) extends Table[A](tag, "t4_a") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def data = column[Int]("data")
      def * = (id, data).mapTo[A]
    }
    val as = TableQuery[ARow]

    class BRow(tag: Tag) extends Table[B](tag, "t5_b") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def data = column[String]("data")
      def * = (id, Rep.Some(data)).mapTo[B]
    }
    val bs = TableQuery[BRow]

    val q1 = for {
      a <- as if a.data === 2
      b <- bs if b.id === a.id
    } yield (a, b)

    val q2 = as joinLeft bs

    for {
      _ <- (as.schema ++ bs.schema).create
      _ <- as.map(_.data) ++= Seq(1, 2)
      _ <- bs.map(_.data) ++= Seq("a", "b")
      r1: Set[(A, B)] <- q1.to[Set].result
      _ = r1 shouldBe Set((A(2, 2), B(2, Some("b"))))
      r2: Set[(A, Option[B])] <- q2.to[Set].result
      _ = r2 shouldBe Set(
        (A(1,1), Some(B(1,Some("a")))),
        (A(1,1), Some(B(2,Some("b")))),
        (A(2,2), Some(B(1,Some("a")))),
        (A(2,2), Some(B(2,Some("b"))))
      )
    } yield ()
  }

  def testCaseClassMapping2 = {
    case class LiftedB(
      a: Rep[Option[Long]],
      b: Rep[Option[Long]],
      c: Rep[Option[Long]],
      d: Rep[Option[Int]],
      e: Rep[Option[Double]],
      f: Rep[Option[Int]]
    )

    case class B(
      a: Option[Long],
      b: Option[Long],
      c: Option[Long],
      d: Option[Int],
      e: Option[Double],
      f: Option[Int]
    )

    implicit object shape extends CaseClassShape[Product, (
      Rep[Option[Long]],
      Rep[Option[Long]],
      Rep[Option[Long]],
      Rep[Option[Int]],
      Rep[Option[Double]],
      Rep[Option[Int]]
    ), LiftedB, (
      Option[Long],
      Option[Long],
      Option[Long],
      Option[Int],
      Option[Double],
      Option[Int]
    ), B]((LiftedB.apply _).tupled, (B.apply _).tupled)

    case class ARow(id: Int, s: Long)

    class A(tag: Tag) extends Table[ARow](tag, "A_TestCaseClass2") {
      def id = column[Int]("id", O.PrimaryKey)
      def s = column[Long]("s")
      def * = (id, s).mapTo[ARow]
    }
    val as = TableQuery[A]
    val data = Seq(ARow(1, 1L), ARow(2, 2L))

    as.schema.create >>
      (as ++= data) >>
      (as.length, LiftedB(
        as.map(_.id).sum.getOrElse(0).asColumnOf[Option[Long]],
        LiteralColumn(None),
        as.map(_.s).sum.getOrElse(0L).asColumnOf[Option[Long]],
        LiteralColumn(None),
        LiteralColumn(None),
        LiteralColumn(None)
      )).result.map (
        _._1 shouldBe 2
      )
  }

  def testCaseClassShape = {
    case class C(a: Int, b: String)
    case class LiftedC(a: Rep[Int], b: Rep[String])
    implicit object cShape
      extends CaseClassShape[Product, (Rep[Int], Rep[String]), LiftedC, (Int, String), C](
        (LiftedC.apply _).tupled,
        (C.apply _).tupled
      )

    class A(tag: Tag) extends Table[C](tag, "A_CaseClassShape") {
      def id = column[Int]("id", O.PrimaryKey)
      def s = column[String]("s")
      def * = LiftedC(id, s)
    }
    val as = TableQuery[A]
    val data = Seq(C(1, "a"), C(2, "b"))

    as.schema.create >> (as ++= data) >> as.sortBy(_.id).result.map(_ shouldBe data)
  }

  def testProductClassShape = {
    def columnShape[T](implicit s: Shape[FlatShapeLevel, Rep[T], T, Rep[T]]) = s
    class C(val a: Int, val b: Option[String]) extends Product{
      def canEqual(that: Any): Boolean = that.isInstanceOf[C]
      def productArity: Int = 2
      def productElement(n: Int): Any = Seq(a, b)(n)
      override def equals(a: Any) = a match {
        case that: C => this.a == that.a && this.b == that.b
        case _ => false
      }
    }
    class LiftedC(val a: Rep[Int], val b: Rep[Option[String]]) extends Product{
      def canEqual(that: Any): Boolean = that.isInstanceOf[LiftedC]
      def productArity: Int = 2
      def productElement(n: Int): Any = Seq(a, b)(n)
      override def equals(a: Any) = a match {
        case that: LiftedC => this.a == that.a && this.b == that.b
        case _ => false
      }
    }
    implicit object cShape extends ProductClassShape[C, LiftedC](
      Seq(columnShape[Int], columnShape[Option[String]]),
      seq => new LiftedC(seq(0).asInstanceOf[Rep[Int]], seq(1).asInstanceOf[Rep[Option[String]]]),
      seq => new C(seq(0).asInstanceOf[Int], seq(1).asInstanceOf[Option[String]])
    )

    class A(tag: Tag) extends Table[C](tag, "A_ProductClassShape") {
      def id = column[Int]("id", O.PrimaryKey)
      def s = column[Option[String]]("s")
      def * = new LiftedC(id, s)
    }
    val as = TableQuery[A]
    val data = Seq(new C(1, Some("a")), new C(2, Some("b")))

    as.schema.create >> (as ++= data) >> as.sortBy(_.id).result.map(_ shouldBe data)
  }

  def testCustomShape = {
    // A custom record class
    case class Pair[A, B](a: A, b: B)

    // A Shape that maps Pair to a ProductNode
    final class PairShape[Level <: ShapeLevel, M <: Pair[_,_], U <: Pair[_,_] : ClassTag, P <: Pair[_,_]](val shapes: Seq[Shape[_ <: ShapeLevel, _, _, _]]) extends MappedScalaProductShape[Level, Pair[_,_], M, U, P] {
      def buildValue(elems: IndexedSeq[Any]) = Pair(elems(0), elems(1))
      def copy(shapes: Seq[Shape[_ <: ShapeLevel, _, _, _]]) = new PairShape(shapes)
    }
    implicit def pairShape[Level <: ShapeLevel, M1, M2, U1, U2, P1, P2](implicit s1: Shape[_ <: Level, M1, U1, P1], s2: Shape[_ <: Level, M2, U2, P2]): PairShape[Level, Pair[M1, M2], Pair[U1, U2], Pair[P1, P2]] =
      new PairShape[Level, Pair[M1, M2], Pair[U1, U2], Pair[P1, P2]](Seq(s1, s2))

    // Use it in a table definition
    class A(tag: Tag) extends Table[Pair[Int, String]](tag, "shape_a") {
      def id = column[Int]("id", O.PrimaryKey)
      def s = column[String]("s")
      def * = Pair(id, s)
    }
    val as = TableQuery[A]

    // Use it for returning data from a query
    val q2 = as
      .map { case a => Pair(a.id, (a.s ++ a.s)) }
      .filter { case Pair(id, _) => id =!= 1 }
      .sortBy { case Pair(_, ss) => ss }
      .map { case Pair(id, ss) => Pair(id, Pair(42 , ss)) }

    seq(
      as.schema.create,
      // Insert data with the custom shape
      as += Pair(1, "a"),
      as += Pair(2, "c"),
      as += Pair(3, "b"),
      q2.result.map(_ shouldBe Vector(Pair(3, Pair(42, "bb")), Pair(2, Pair(42, "cc"))))
    )
  }

  def testHList = {
    import slick.collection.heterogeneous.*
    import slick.collection.heterogeneous.syntax.*

    case class Data(id: Int, b: Boolean, s: String)

    class B(tag: Tag) extends Table[Int :: Boolean :: String :: HNil](tag, "hlist_b") {
      def id = column[Int]("id", O.PrimaryKey)
      def b = column[Boolean]("b")
      def s = column[String]("s")
      def * = id :: b :: s :: HNil
      def mapped = *.mapTo[Data]
    }
    val bs = TableQuery[B]

    val q1 =
      (for {
        case id :: b :: s :: HNil <- for (b <- bs) yield b.id :: b.b :: b.s :: HNil
        if !b
      } yield id :: b :: (s ++ s) :: HNil)
        .sortBy(h => h.tail.tail.head)
        .map {
          case id :: _ :: ss :: HNil => id :: ss :: (42 :: HNil) :: HNil
        }
    val q2 = bs
      .map(b => b.id :: b.b :: (b.s ++ b.s) :: HNil)
      .filter { h => !h.tail.head }
      .sortBy { case _ :: _ :: ss :: HNil => ss }
      .map { case id :: _ :: ss :: HNil => id :: ss :: (42 :: HNil) :: HNil }

    val expected = Vector(3 :: "bb" :: (42 :: HNil) :: HNil, 2 :: "cc" :: (42 :: HNil) :: HNil)
    seq(
      bs.schema.create,
      bs += (1 :: true :: "a" :: HNil),
      bs += (2 :: false :: "c" :: HNil),
      bs.map(_.mapped) += Data(3, b = false, "b"),
      q1.result.map(_.shouldBe(expected)),
      q2.result.map(_.shouldBe(expected)),
      bs.map(_.mapped).result
        .map(_.toSet.shouldBe(Set(Data(1, b = true, "a"), Data(2, b = false, "c"), Data(3, b = false, "b"))))
    )
  }

  def testSingleElement = {
    import slick.collection.heterogeneous.*
    import slick.collection.heterogeneous.syntax.*

    class A(tag: Tag) extends Table[String](tag, "single_a") {
      def b = column[String]("b")
      def * = b
    }
    val as = TableQuery[A]

    class B(tag: Tag) extends Table[Tuple1[String]](tag, "single_b") {
      def b = column[String]("b")
      def * = Tuple1(b)
    }
    val bs = TableQuery[B]

    class C(tag: Tag) extends Table[String :: HNil](tag, "single_c") {
      def b = column[String]("b")
      def * = b :: HNil
    }
    val cs = TableQuery[C]

    for {
      _ <- as.schema.create
      _ <- as += "Foo"
      ares: String <- as.result.head
      _ = ares shouldBe "Foo"
      _ <- as.update("Foo")
      _ <- as.map(a => a :: a :: HNil).result.head.map(_ shouldBe "Foo" :: "Foo" :: HNil)
      _ <- bs.schema.create
      _ <- bs += Tuple1("Foo")
      _ <- bs.update(Tuple1("Foo"))
      Tuple1(bres) <- bs.result.head
      _ = bres shouldBe "Foo"
      _ <- cs.schema.create
      _ <- cs += ("Foo" :: HNil)
      _ <- cs.update("Foo" :: HNil)
      case cres :: HNil <- cs.result.head
      _ = cres shouldBe "Foo"
    } yield ()
  }

  def testFastPath = {
    case class Data(a: Int, b: Int)

    class T(tag: Tag) extends Table[Data](tag, "T_fastpath") {
      def a = column[Int]("A")
      def b = column[Int]("B")
      def * = (a, b).mapTo[Data].fastPath(new FastPath(_) {
        val (a, b) = (next[Int], next[Int])
        override def read(r: ResultSet) = Data(a.read(r), b.read(r))
      })
      def auto = (a, b).mapTo[Data]
    }
    val ts = TableQuery[T]

    seq(
      ts.schema.create,
      ts ++= Seq(Data(1, 2), Data(3, 4), Data(5, 6)),
      ts.filter(_.a === 1).update(Data(7, 8)),
      ts.filter(_.a === 3).map(identity).update(Data(9, 10)),
      ts.to[Set].result.map(_ shouldBe Set(Data(7, 8), Data(9, 10), Data(5, 6))),
      ts.map(_.auto).to[Set].result.map(_ shouldBe Set(Data(7, 8), Data(9, 10), Data(5, 6)))
    )
  }
}
