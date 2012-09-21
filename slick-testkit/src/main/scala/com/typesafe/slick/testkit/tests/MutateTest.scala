package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{TestkitTest, TestDB}

class MutateTest(val tdb: TestDB) extends TestkitTest {
  import tdb.profile.simple._

  def test = ifCap(jcap.mutable) {

    object Users extends Table[(Int,String,String)]("USERS") {
      def id = column[Int]("ID", O.PrimaryKey)
      def first = column[String]("FIRST")
      def last = column[String]("LAST")
      def * = id ~ first ~ last
    }

    Users.ddl.create
    Users insertAll(
      (1, "Marge", "Bouvier"),
      (2, "Homer", "Simpson"),
      (3, "Bart", "Simpson"),
      (4, "Carl", "Carlson")
    )

    println("Before mutating:")
    Query(Users).foreach(u => println("  "+u))

    val q1 = for(u <- Users if u.last === "Simpson" || u.last === "Bouvier") yield u
    q1.mutate { m =>
      println("***** Row: "+m.row)
      if(m.row._3 == "Bouvier") m.row = m.row.copy(_3 = "Simpson")
      else if(m.row._2 == "Homer") m.delete()
      else if(m.row._2 == "Bart") m.insert((42, "Lisa", "Simpson"))
    }

    println("After mutating:")
    Query(Users).foreach(u => println("  "+u))

    assertEquals(
      Set("Marge Simpson", "Bart Simpson", "Lisa Simpson", "Carl Carlson"),
      (for(u <- Users) yield u.first ++ " " ++ u.last).list.toSet
    )
  }
}
