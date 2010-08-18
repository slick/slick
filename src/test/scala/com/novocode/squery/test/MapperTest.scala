package com.novocode.squery.test

import org.junit.Test
import org.junit.Assert._
import com.novocode.squery.combinator._
import com.novocode.squery.combinator.TypeMapper._
import com.novocode.squery.combinator.extended.{ExtendedTable => Table}
import com.novocode.squery.session._
import com.novocode.squery.session.Database.threadLocalSession
import com.novocode.squery.test.util._
import com.novocode.squery.test.util.TestDB._

object MapperTest extends DBTestObject(H2Mem, SQLiteMem)

class MapperTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit._

  @Test def test() {

    case class User(id: Option[Int], first: String, last: String)

    object Users extends Table[User]("users") {
      def id = column[Int]("id", O PrimaryKey, O NotNull, O AutoInc)
      def first = column[String]("first", O NotNull)
      def last = column[String]("last", O NotNull)
      def * = id.? ~ first ~ last <> (User, User.unapply _)
      val findByID = createFinderBy(_.id)
    }

    db withSession {

      Users.ddl.create
      (Users.first ~ Users.last).insert("Homer", "Simpson")
      Users.insertAll(
        User(None, "Marge", "Simpson"),
        User(None, "Carl", "Carlson"),
        User(None, "Lenny", "Leonard")
      )

      Users.where(_.id between(1, 2)).foreach(println)
      println("ID 3 -> " + Users.findByID.first(3))

      assertEquals(
        Users.where(_.id between(1, 2)).list.toSet,
        Set(User(Some(1), "Homer", "Simpson"), User(Some(2), "Marge", "Simpson"))
      )
      assertEquals(
        Users.findByID.first(3),
        User(Some(3), "Carl", "Carlson")
      )
    }
  }
}
