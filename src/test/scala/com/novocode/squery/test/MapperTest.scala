package com.novocode.squery.test

import org.junit.Test
import org.junit.Assert._
import com.novocode.squery.combinator._
import com.novocode.squery.combinator.TypeMapper._
import com.novocode.squery.combinator.extended.H2Driver.Implicit._
import com.novocode.squery.combinator.extended.{ExtendedTable => Table}
import com.novocode.squery.session._
import com.novocode.squery.session.Database.threadLocalSession

object MapperTest { def main(args: Array[String]) = new MapperTest().test() }

class MapperTest {
  @Test def test() {

    case class User(id: Option[Int], first: String, last: String)

    object Users extends Table[User]("users") {
      def id = column[Int]("id", O AutoInc, O NotNull)
      def first = column[String]("first", O NotNull)
      def last = column[String]("last", O NotNull)
      def * = id.? ~ first ~ last <> (User, User.unapply _)
      val findByID = createFinderBy(_.id)
    }

    Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver") withSession {

      Users.createTable
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
