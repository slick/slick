package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}

class MutateTest extends TestkitTest[JdbcTestDB] {
  import tdb.profile.simple._

  override val reuseInstance = true

  def testMutate = ifCap(jcap.mutable) {

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

  def testDeleteMutate = ifCap(jcap.mutable) {
    object T extends Table[(Int, Int)]("T_DELMUTABLE") {
      def a = column[Int]("A")
      def b = column[Int]("B", O.PrimaryKey)
      def * = a ~ b
      def findByA = createFinderBy(_.a)
    }

    T.ddl.create
    T.insertAll((1,1), (1,2), (1,3), (1,4))
    T.insertAll((2,5), (2,6), (2,7), (2,8))

    T.findByA(1).mutate(_.delete)

    assertEquals(Set((2,5), (2,6), (2,7), (2,8)), Query(T).to[Set])
  }
}
