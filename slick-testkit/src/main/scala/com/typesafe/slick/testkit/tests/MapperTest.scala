package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import scala.slick.ast.Dump
import com.typesafe.slick.testkit.util.{TestkitTest, TestDB}

class MapperTest(val tdb: TestDB) extends TestkitTest {
  import tdb.profile.simple._

  override val reuseInstance = true

  def testMappedEntity {
    case class User(id: Option[Int], first: String, last: String)

    object Users extends Table[User]("users") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def first = column[String]("first")
      def last = column[String]("last")
      def * = id.? ~ first ~ last <> (User, User.unapply _)
      def forInsert = first ~ last <>
        ({ (f, l) => User(None, f, l) }, { u:User => Some((u.first, u.last)) })
      val findByID = createFinderBy(_.id)
    }

    Users.ddl.create
    (Users.first ~ Users.last).insert("Homer", "Simpson")
    /* Using Users.forInsert so that we don't put a NULL value into the ID
     * column. H2 and SQLite allow this but PostgreSQL doesn't. */
    Users.forInsert.insertAll(
      User(None, "Marge", "Bouvier"),
      User(None, "Carl", "Carlson"),
      User(None, "Lenny", "Leonard")
    )

    val lastNames = Set("Bouvier", "Ferdinand")
    assertEquals(1, Query(Users).where(_.last inSet lastNames).list.size)

    val updateQ = Users.where(_.id === 2.bind).map(_.forInsert)
    println("Update: "+updateQ.updateStatement)
    updateQ.update(User(None, "Marge", "Simpson"))

    Users.where(_.id between(1, 2)).foreach(println)
    println("ID 3 -> " + Users.findByID.first(3))

    assertEquals(
      Set(User(Some(1), "Homer", "Simpson"), User(Some(2), "Marge", "Simpson")),
      Users.where(_.id between(1, 2)).list.toSet
    )
    assertEquals(
      User(Some(3), "Carl", "Carlson"),
      Users.findByID.first(3)
    )
  }

  def testUpdate {
    case class Data(a: Int, b: Int)

    object Ts extends Table[Data]("T") {
      def a = column[Int]("A")
      def b = column[Int]("B")
      def * = a ~ b <> (Data, Data.unapply _)
    }

    Ts.ddl.create
    Ts.insertAll(new Data(1, 2), new Data(3, 4), new Data(5, 6))

    val updateQ = Ts.where(_.a === 1)
    Dump(updateQ, "updateQ: ")
    println("Update: "+updateQ.updateStatement)
    updateQ.update(Data(7, 8))

    val updateQ2 = Ts.where(_.a === 3).map(identity)
    Dump(updateQ2, "updateQ2: ")
    println("Update2: "+updateQ2.updateStatement)
    updateQ2.update(Data(9, 10))

    assertEquals(
      Set(Data(7, 8), Data(9, 10), Data(5, 6)),
      Query(Ts).list.toSet
    )
  }

  def testMappedType {
    sealed trait Bool
    case object True extends Bool
    case object False extends Bool

    implicit val boolTypeMapper = MappedTypeMapper.base[Bool, Int](
      b => if(b == True) 1 else 0,
      i => if(i == 1) True else False)

    object T extends Table[(Int, Bool)]("t2") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def b = column[Bool]("b")
      def * = id ~ b
    }

    T.ddl.create
    T.b.insertAll(False, True)
    assertEquals(Query(T).list.toSet, Set((1, False), (2, True)))
    assertEquals(T.where(_.b === (True:Bool)).list.toSet, Set((2, True)))
    assertEquals(T.where(_.b === (False:Bool)).list.toSet, Set((1, False)))
  }
}
