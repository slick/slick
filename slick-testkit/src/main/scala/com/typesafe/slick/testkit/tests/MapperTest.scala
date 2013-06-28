package com.typesafe.slick.testkit.tests

import org.junit.Assert._

import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}

class MapperTest extends TestkitTest[JdbcTestDB] {
  import tdb.profile.simple._

  override val reuseInstance = true

  def testMappedEntity {
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
    val users = TableQuery(new Users(_))
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
    val ts = TableQuery(new T(_))

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
    val ts = TableQuery(new T(_))

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
    val ts = TableQuery(new T(_))

    ts.ddl.create
    ts.map(t => (t.b, t.c)).insertAll((False, None), (True, Some(True)))
    assertEquals(ts.list.toSet, Set((1, False, None), (2, True, Some(True))))
    assertEquals(ts.where(_.b === (True:Bool)).list.toSet, Set((2, True, Some(True))))
    assertEquals(ts.where(_.b === (False:Bool)).list.toSet, Set((1, False, None)))
  }

  def testWideMappedEntity {
    case class Part(i1: Int, i2: Int, i3: Int, i4: Int, i5: Int, i6: Int)
    case class Whole(id: Int, p1: Part, p2: Part, p3: Part, p4: Part)

    class T(tag: Tag) extends Table[Int](tag, "t_wide") {
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
      def * = id
      def all = (
        id,
        (p1i1, p1i2, p1i3, p1i4, p1i5, p1i6),
        (p2i1, p2i2, p2i3, p2i4, p2i5, p2i6),
        (p3i1, p3i2, p3i3, p3i4, p3i5, p3i6),
        (p4i1, p4i2, p4i3, p4i4, p4i5, p4i6)
      )
      override def create_* = collectFieldSymbols(all.shaped.packedNode)
    }
    val ts = TableQuery(new T(_))

    val data = (
      0,
      (11, 12, 13, 14, 15, 16),
      (21, 22, 23, 24, 25, 26),
      (31, 32, 33, 34, 35, 36),
      (41, 42, 43, 44, 45, 46)
    )

    val oData = Whole(0,
      Part(11, 12, 13, 14, 15, 16),
      Part(21, 22, 23, 24, 25, 26),
      Part(31, 32, 33, 34, 35, 36),
      Part(41, 42, 43, 44, 45, 46)
    )

    ts.ddl.create
    ts.map(_.all).insert(data)

    val q1 = ts.map(_.all)
    assertEquals(data, q1.first)

    val i2 = q1.mapResult { case (id, p1, p2, p3, p4) =>
      Whole(id, Part.tupled.apply(p1), Part.tupled.apply(p2), Part.tupled.apply(p3), Part.tupled.apply(p4))
    }
    assertEquals(oData, i2.first)
  }

  def testMappedJoin {
    case class A(id: Int, value: Int)
    case class B(id: Int, value: Option[String])

    class ARow(tag: Tag) extends Table[A](tag, "t4_a") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def data = column[Int]("data")
      def * = id ~ data <> (A.tupled, A.unapply _)
    }
    val as = TableQuery(new ARow(_))

    class BRow(tag: Tag) extends Table[B](tag, "t5_b") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def data = column[String]("data")
      def * = id ~ data.? <> (B.tupled, B.unapply _)
    }
    val bs = TableQuery(new BRow(_))

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

  /*
  def testGetOr {
    object T extends Table[Option[Int]]("t4") {
      def year = column[Option[Int]]("YEAR")
      def * = year
    }

    T.ddl.create
    T.insertAll(Some(2000), None)

    val q = T.map(t => (t.year.getOr(2000), (t.year.getOr(2000)-0)))
    println(q.selectStatement)
    q.foreach(println)
  }
  */
}
