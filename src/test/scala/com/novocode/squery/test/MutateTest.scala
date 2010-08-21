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

object MutateTest extends DBTestObject(H2Mem, Postgres)

class MutateTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit._

  @Test def test() {

    object Users extends Table[(Option[Int],String,String)]("users") {
      def id = column[Int]("id", O NotNull, O PrimaryKey, O AutoInc)
      def first = column[String]("first", O NotNull)
      def last = column[String]("last", O NotNull)
      def * = id.? ~ first ~ last
    }

    db withSession {

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
        else if(m.row._2 == "Bart")
          /* We have to provide an explicit ID here to make it work with
           * Postgres. H2 accepts a NULL and auto-generates the ID but
           * Postgres only does that when you use DEFAULT and there's no way
           * to do that through an updatable ResultSet */
          m.insert((Some(42), "Lisa", "Simpson"))
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
