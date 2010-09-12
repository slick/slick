package org.scalaquery.test

import org.junit.Test
import org.junit.Assert._
import org.scalaquery.ql._
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql.extended.{ExtendedTable => Table}
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.test.util._
import org.scalaquery.test.util.TestDB._

object MutateTest extends DBTestObject(H2Mem, Postgres, MySQL, DerbyMem, HsqldbMem)

class MutateTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit._

  @Test def test() {

    object Users extends Table[(Option[Int],String,String)]("users") {
      def id = column[Int]("id", O PrimaryKey, O AutoInc)
      def first = column[String]("first")
      def last = column[String]("last")
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
