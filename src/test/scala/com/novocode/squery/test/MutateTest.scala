package com.novocode.squery.test

import org.junit.Test
import org.junit.Assert._
import com.novocode.squery.combinator._
import com.novocode.squery.combinator.TypeMapper._
import com.novocode.squery.combinator.basic.SQLiteDriver.Implicit._
import com.novocode.squery.combinator.basic.{BasicTable => Table}
import com.novocode.squery.session._
import com.novocode.squery.session.Database.threadLocalSession

object MutateTest { def main(args: Array[String]) = new MutateTest().test() }

class MutateTest {
  @Test def test() {

    object Users extends Table[(Option[Int],String,String)]("users") {
      def id = column[Int]("id", O PrimaryKey, O NotNull, O PrimaryKey)
      def first = column[String]("first", O NotNull)
      def last = column[String]("last", O NotNull)
      def * = id.? ~ first ~ last
    }

    Database.forURL("jdbc:sqlite:sample.db", driver = "org.sqlite.JDBC") withSession {

      Users.ddl.create
      Users.first ~ Users.last insertAll(
        ("Marge", "Bouvier"),
        ("Homer", "Simpson"),
        ("Bart", "Simpson"),
        ("Carl", "Carlson")
      )

      println("Before mutating:")
      Query(Users).foreach(u => println("  "+u))

      val q1 = for(u <- Users if u.last === "Simpson" || u.last === "Bouvier") yield u
      q1.mutate { m =>
        if(m.row._3 == "Bouvier") m.row = m.row.copy(_3 = "Simpson")
        else if(m.row._2 == "Homer") m.delete()
        else if(m.row._2 == "Bart") m.insert((None, "Lisa", "Simpson"))
      }

      println("After mutating:")
      Query(Users).foreach(u => println("  "+u))

      assertEquals(
        Set("Marge Simpson", "Bart Simpson", "Lisa Simpson", "Carl Carlson"),
        (for(u <- Users) yield u.first ++ " " ++ u.last).list.toSet
      )
    }
  }
}
