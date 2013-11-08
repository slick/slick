package com.typesafe.slick.testkit.tests

import org.junit.Assert._

import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}

class JdbcMapperTest extends TestkitTest[JdbcTestDB] {
  import tdb.profile.simple._

  override val reuseInstance = true

  def testMappedEntity {
    import TupleMethods._

    case class User(id: Option[Int], first: String, last: String)
    case class Foo[T](value: T)

    class Users(tag: Tag) extends Table[User](tag, "users") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def first = column[String]("first")
      def last = column[String]("last")
      def * = id.? ~: baseProjection <> (User.tupled, User.unapply _)
      def baseProjection = first ~ last
      def forUpdate = baseProjection.shaped <>
        ({ case (f, l) => User(None, f, l) }, { u:User => Some((u.first, u.last)) })
      def asFoo = forUpdate <> ((u: User) => Foo(u), (f: Foo[User]) => Some(f.value))
    }
    object users extends TableQuery(new Users(_)) {
      val byID = this.findBy(_.id)
    }

    users.ddl.create
    users.map(_.baseProjection).insert("Homer", "Simpson")
    users.insertAll(
      User(None, "Marge", "Bouvier"),
      User(None, "Carl", "Carlson")
    )
    users.map(_.asFoo) += Foo(User(None, "Lenny", "Leonard"))

    val lastNames = Set("Bouvier", "Ferdinand")
    assertEquals(1, users.where(_.last inSet lastNames).list.size)

    val updateQ = users.where(_.id === 2.bind).map(_.forUpdate)
    println("Update: "+updateQ.updateStatement)
    updateQ.update(User(None, "Marge", "Simpson"))

    assertTrue(Query(users.where(_.id === 1).exists).first)

    users.where(_.id between(1, 2)).foreach(println)
    println("ID 3 -> " + users.byID(3).first)

    assertEquals(
      Set(User(Some(1), "Homer", "Simpson"), User(Some(2), "Marge", "Simpson")),
      users.where(_.id between(1, 2)).list.toSet
    )
    assertEquals(
      Set(Foo(User(None, "Homer", "Simpson")), Foo(User(None, "Marge", "Simpson"))),
      users.where(_.id between(1, 2)).map(_.asFoo).list.toSet
    )
    assertEquals(
      User(Some(3), "Carl", "Carlson"),
      users.byID(3).first
    )

    val q1 = for {
      u <- users
      u2 <- users
    } yield u2
    val r1 = q1.run.head
    assertTrue("Element class: "+r1.getClass, r1.isInstanceOf[User])
  }

  def testUpdate {
    case class Data(a: Int, b: Int)

    class T(tag: Tag) extends Table[Data](tag, "T") {
      def a = column[Int]("A")
      def b = column[Int]("B")
      def * = (a, b) <> (Data.tupled, Data.unapply _)
    }
    val ts = TableQuery[T]

    ts.ddl.create
    ts.insertAll(new Data(1, 2), new Data(3, 4), new Data(5, 6))

    val updateQ = ts.where(_.a === 1)
    updateQ.update(Data(7, 8))

    val updateQ2 = ts.where(_.a === 3).map(identity)
    updateQ2.update(Data(9, 10))

    assertEquals(
      Set(Data(7, 8), Data(9, 10), Data(5, 6)),
      ts.list.toSet
    )
  }

  def testWideMappedEntity {
    case class Part(i1: Int, i2: Int, i3: Int, i4: Int, i5: Int, i6: Int)
    case class Whole(id: Int, p1: Part, p2: Part, p3: Part, p4: Part)

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
      def * = (
        id,
        (p1i1, p1i2, p1i3, p1i4, p1i5, p1i6),
        (p2i1, p2i2, p2i3, p2i4, p2i5, p2i6),
        (p3i1, p3i2, p3i3, p3i4, p3i5, p3i6),
        (p4i1, p4i2, p4i3, p4i4, p4i5, p4i6)
      ).shaped <> ({ case (id, p1, p2, p3, p4) =>
        // We could do this without .shaped but then we'd have to write a type annotation for the parameters
        Whole(id, Part.tupled.apply(p1), Part.tupled.apply(p2), Part.tupled.apply(p3), Part.tupled.apply(p4))
      }, { w: Whole =>
        def f(p: Part) = Part.unapply(p).get
        Some((w.id, f(w.p1), f(w.p2), f(w.p3), f(w.p4)))
      })
    }
    val ts = TableQuery[T]

    val oData = Whole(0,
      Part(11, 12, 13, 14, 15, 16),
      Part(21, 22, 23, 24, 25, 26),
      Part(31, 32, 33, 34, 35, 36),
      Part(41, 42, 43, 44, 45, 46)
    )

    ts.ddl.create
    ts.insert(oData)

    assertEquals(oData, ts.first)
  }

  def testMappedJoin {
    case class A(id: Int, value: Int)
    case class B(id: Int, value: Option[String])

    class ARow(tag: Tag) extends Table[A](tag, "t4_a") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def data = column[Int]("data")
      def * = (id, data) <> (A.tupled, A.unapply _)
    }
    val as = TableQuery[ARow]

    class BRow(tag: Tag) extends Table[B](tag, "t5_b") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def data = column[String]("data")
      def * = (id, data.?) <> (B.tupled, B.unapply _)
    }
    val bs = TableQuery[BRow]

    (as.ddl ++ bs.ddl).create
    as.map(_.data).insertAll(1, 2)
    bs.map(_.data).insertAll("a", "b")

    val q = for {
      a <- as if a.data === 2
      b <- bs if b.id === a.id
    } yield (a, b)

    val r = q.run.toList
    val r2: List[(A, B)] = r
    assertEquals(List((A(2, 2), B(2, Some("b")))), r2)
  }

  def testCustomShape {
    // A custom record class
    case class Pair[A, B](a: A, b: B)

    // A Shape that maps Pair to a ProductNode
    final class PairShape[Level <: ShapeLevel, M <: Pair[_,_], U <: Pair[_,_], P <: Pair[_,_]](val shapes: Seq[Shape[_, _, _, _]]) extends MappedScalaProductShape[Level, Pair[_,_], M, U, P] {
      def buildValue(elems: IndexedSeq[Any]) = Pair(elems(0), elems(1))
      def copy(shapes: Seq[Shape[_ <: ShapeLevel, _, _, _]]) = new PairShape(shapes)
    }
    implicit def pairShape[Level <: ShapeLevel, M1, M2, U1, U2, P1, P2](implicit s1: Shape[_ <: Level, M1, U1, P1], s2: Shape[_ <: Level, M2, U2, P2]) =
      new PairShape[Level, Pair[M1, M2], Pair[U1, U2], Pair[P1, P2]](Seq(s1, s2))

    // Use it in a table definition
    class A(tag: Tag) extends Table[Pair[Int, String]](tag, "shape_a") {
      def id = column[Int]("id", O.PrimaryKey)
      def s = column[String]("s")
      def * = Pair(id, s)
    }
    val as = TableQuery[A]
    as.ddl.create

    // Insert data with the custom shape
    as += Pair(1, "a")
    as += Pair(2, "c")
    as += Pair(3, "b")

    // Use it for returning data from a query
    val q2 = as
      .map { case a => Pair(a.id, (a.s ++ a.s)) }
      .filter { case Pair(id, _) => id =!= 1 }
      .sortBy { case Pair(_, ss) => ss }
      .map { case Pair(id, ss) => Pair(id, Pair(42 , ss)) }
    assertEquals(Vector(Pair(3, Pair(42, "bb")), Pair(2, Pair(42, "cc"))), q2.run)
  }

  def testHList {
    import scala.slick.collection.heterogenous._
    import scala.slick.collection.heterogenous.syntax._

    class B(tag: Tag) extends Table[Int :: Boolean :: String :: HNil](tag, "hlist_b") {
      def id = column[Int]("id", O.PrimaryKey)
      def b = column[Boolean]("b")
      def s = column[String]("s")
      def * = id :: b :: s :: HNil
    }
    val bs = TableQuery[B]
    bs.ddl.create

    bs += (1 :: true :: "a" :: HNil)
    bs += (2 :: false :: "c" :: HNil)
    bs += (3 :: false :: "b" :: HNil)

    val q1 = (for {
      id :: b :: s :: HNil <- (for { b <- bs } yield b.id :: b.b :: b.s :: HNil) if !b
    } yield id :: b :: (s ++ s) :: HNil).sortBy(h => h(2)).map {
      case id :: b :: ss :: HNil => id :: ss :: (42 :: HNil) :: HNil
    }
    assertEquals(Vector(3 :: "bb" :: (42 :: HNil) :: HNil, 2 :: "cc" :: (42 :: HNil) :: HNil), q1.run)

    val q2 = bs
      .map { case b => b.id :: b.b :: (b.s ++ b.s) :: HNil }
      .filter { h => !h(1) }
      .sortBy { case _ :: _ :: ss :: HNil => ss }
      .map { case id :: b :: ss :: HNil => id :: ss :: (42 :: HNil) :: HNil }
    assertEquals(Vector(3 :: "bb" :: (42 :: HNil) :: HNil, 2 :: "cc" :: (42 :: HNil) :: HNil), q2.run)
  }
}
