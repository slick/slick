package com.typesafe.slick.testkit.tests

import org.junit.Assert._

import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}

class MapperTest extends TestkitTest[JdbcTestDB] {
  import tdb.profile.simple._

  override val reuseInstance = true

  def testMappedEntity {
    import TupleMethods._

    case class User(id: Option[Int], first: String, last: String)

    class Users(tag: Tag) extends Table[User](tag, "users") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def first = column[String]("first")
      def last = column[String]("last")
      def * = id.? ~: baseProjection <> (User.tupled, User.unapply _)
      def baseProjection = first ~ last
      def forInsert = baseProjection.shaped <>
        ({ case (f, l) => User(None, f, l) }, { u:User => Some((u.first, u.last)) })
    }
    val users = TableQuery[Users]
    val usersByID = users.findBy(_.id)

    users.ddl.create
    users.map(_.baseProjection).insert("Homer", "Simpson")
    /* Using Users.forInsert so that we don't put a NULL value into the ID
     * column. H2 and SQLite allow this but PostgreSQL doesn't. */
    users.map(_.forInsert).insertAll(
      User(None, "Marge", "Bouvier"),
      User(None, "Carl", "Carlson"),
      User(None, "Lenny", "Leonard")
    )

    val lastNames = Set("Bouvier", "Ferdinand")
    assertEquals(1, users.where(_.last inSet lastNames).list.size)

    val updateQ = users.where(_.id === 2.bind).map(_.forInsert)
    println("Update: "+updateQ.updateStatement)
    updateQ.update(User(None, "Marge", "Simpson"))

    assertTrue(Query(users.where(_.id === 1).exists).first)

    users.where(_.id between(1, 2)).foreach(println)
    println("ID 3 -> " + usersByID.first(3))

    assertEquals(
      Set(User(Some(1), "Homer", "Simpson"), User(Some(2), "Marge", "Simpson")),
      users.where(_.id between(1, 2)).list.toSet
    )
    assertEquals(
      User(Some(3), "Carl", "Carlson"),
      usersByID.first(3)
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

  def testMappedType {
    sealed trait Bool
    case object True extends Bool
    case object False extends Bool

    implicit val boolTypeMapper = MappedColumnType.base[Bool, Int](
      { b =>
        assertNotNull(b)
        if(b == True) 1 else 0
      }, { i =>
        assertNotNull(i)
        if(i == 1) True else False
      }
    )

    class T(tag: Tag) extends Table[(Int, Bool, Option[Bool])](tag, "t2") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def b = column[Bool]("b")
      def c = column[Option[Bool]]("c")
      def * = (id, b, c)
    }
    val ts = TableQuery[T]

    ts.ddl.create
    ts.map(t => (t.b, t.c)).insertAll((False, None), (True, Some(True)))
    assertEquals(ts.list.toSet, Set((1, False, None), (2, True, Some(True))))
    assertEquals(ts.where(_.b === (True:Bool)).list.toSet, Set((2, True, Some(True))))
    assertEquals(ts.where(_.b === (False:Bool)).list.toSet, Set((1, False, None)))
  }

  def testMappedRefType {
    sealed trait Bool
    case object True extends Bool
    case object False extends Bool

    implicit val boolTypeMapper = MappedColumnType.base[Bool, String](
      { b =>
        assertNotNull(b)
        if(b == True) "y" else "n"
      }, { i =>
        assertNotNull(i)
        if(i == "y") True else False
      }
    )

    class T(tag: Tag) extends Table[(Int, Bool, Option[Bool])](tag, "t3") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def b = column[Bool]("b")
      def c = column[Option[Bool]]("c")
      def * = (id, b, c)
    }
    val ts = TableQuery[T]

    ts.ddl.create
    ts.map(t => (t.b, t.c)).insertAll((False, None), (True, Some(True)))
    assertEquals(ts.list.toSet, Set((1, False, None), (2, True, Some(True))))
    assertEquals(ts.where(_.b === (True:Bool)).list.toSet, Set((2, True, Some(True))))
    assertEquals(ts.where(_.b === (False:Bool)).list.toSet, Set((1, False, None)))
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
    import scala.annotation.unchecked.uncheckedVariance
    import scala.slick.lifted.{Shape, MappedProductShape, ShapeLevel}

    // A simple HList implementation
    sealed trait HList {
      type Self <: HList
      def :: [H](value: H) = new HCons[H, Self](value, this.asInstanceOf[Self])
      def toList: List[Any]
      def apply(idx: Int): Any
      override def equals(that: Any) = that match {
        case that: HList => toList == that.toList
        case _ => false
      }
    }
    case object HNil extends HList {
      type Self = HNil.type
      def toList = Nil
      def apply(idx: Int) = throw new IllegalArgumentException
    }
    type HNil = HNil.type
    class HCons[+H, +T <: HList](val head: H, val tail: T) extends HList {
      type Self = HCons[H @uncheckedVariance, T @uncheckedVariance]
      def toList: List[Any] = head :: tail.toList
      def apply(idx: Int): Any = if(idx == 0) head else tail(idx-1)
      override def toString = s"$head :: $tail"
    }
    type :: [+H, +T <: HList] = HCons[H, T]
    object :: {
      def unapply[H, T <: HList](l: HCons[H, T]) = Some((l.head, l.tail))
    }

    val l1 = 42 :: true :: "foo" :: HNil
    val t1 = l1 match {
      case i :: b :: s :: HNil => (i, b, s)
    }
    val t1t: (Int, Boolean, String) = t1
    assertEquals((42, true, "foo"), t1t)

    // A Shape for our HList, mapping it to a flat ProductNode
    final class HListShape[M <: HList, U <: HList, P <: HList](val shapes: Seq[Shape[ShapeLevel.Flat, _, _, _]]) extends MappedProductShape[ShapeLevel.Flat, HList, M, U, P] {
      def getIterator(value: HList) = value.toList.iterator
      def getElement(value: HList, idx: Int) = value(idx)
      def buildValue(elems: IndexedSeq[Any]) = elems.foldRight(HNil: HList)(_ :: _)
      def copy(shapes: Seq[Shape[_, _, _, _]]) = new HListShape(shapes.asInstanceOf[Seq[Shape[ShapeLevel.Flat, _, _, _]]])
    }
    implicit val hnilShape = new HListShape[HNil, HNil, HNil](Nil)
    implicit def hconsShape[M1, M2 <: HList, U1, U2 <: HList, P1, P2 <: HList](implicit s1: Shape[ShapeLevel.Flat, M1, U1, P1], s2: HListShape[M2, U2, P2]) =
      new HListShape[M1 :: M2, U1 :: U2, P1:: P2](s1 +: s2.shapes)

    // See if we can get the proper Shape
    val sh1 = implicitly[Shape[ShapeLevel.Flat, Int :: Boolean :: String :: HNil, _, _]]

    // Use the shape in queries but not yet for transferring data
    class A(tag: Tag) extends Table[(Int, Boolean, String)](tag, "shape_a") {
      def id = column[Int]("id", O.PrimaryKey)
      def b = column[Boolean]("b")
      def s = column[String]("s")
      def * = (id, b, s)
    }
    val as = TableQuery[A]
    as.ddl.create
    as += (1, true, "a")
    as += (2, false, "c")
    as += (3, false, "b")
    val q1 = as
      .map { case a => a.id :: a.b :: (a.s ++ a.s) :: HNil }
      .filter { case _ :: b :: _ :: HNil => !b }
      .sortBy { case _ :: _ :: ss :: HNil => ss }
      .map { case id :: b :: ss :: HNil => (id, ss) }
    assertEquals(Vector((3, "bb"), (2, "cc")), q1.run)

    // Use it in a table definition
    class B(tag: Tag) extends Table[Int :: Boolean :: String :: HNil](tag, "shape_b") {
      def id = column[Int]("id", O.PrimaryKey)
      def b = column[Boolean]("b")
      def s = column[String]("s")
      def * = id :: b :: s :: HNil
    }
    val bs = TableQuery[B]
    bs.ddl.create

    // Insert data with the custom shape
    bs += (1 :: true :: "a" :: HNil)
    bs += (2 :: false :: "c" :: HNil)
    bs += (3 :: false :: "b" :: HNil)

    // Use it for returning data from a query
    val q2 = bs
      .map { case b => b.id :: b.b :: (b.s ++ b.s) :: HNil }
      .filter { case _ :: b :: _ :: HNil => !b }
      .sortBy { case _ :: _ :: ss :: HNil => ss }
      .map { case id :: b :: ss :: HNil => id :: ss :: (42 :: HNil) :: HNil }
    assertEquals(Vector(3 :: "bb" :: (42 :: HNil) :: HNil, 2 :: "cc" :: (42 :: HNil) :: HNil), q2.run)
  }
}
