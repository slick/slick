package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}

class MutateTest extends TestkitTest[JdbcTestDB] {
  import tdb.profile.simple._

  override val reuseInstance = true

  def testMutate = ifCap(jcap.mutable) {

    class Users(tag: Tag) extends Table[(Int,String,String)](tag, "USERS") {
      def id = column[Int]("ID", O.PrimaryKey)
      def first = column[String]("FIRST")
      def last = column[String]("LAST")
      def * = (id, first, last)
    }
    val users = TableQuery[Users]

    users.ddl.create
    users insertAll(
      (1, "Marge", "Bouvier"),
      (2, "Homer", "Simpson"),
      (3, "Bart", "Simpson"),
      (4, "Carl", "Carlson")
    )

    println("Before mutating:")
    users.foreach(u => println("  "+u))

    val q1 = for(u <- users if u.last === "Simpson" || u.last === "Bouvier") yield u
    q1.mutate { m =>
      println("***** Row: "+m.row)
      if(m.row._3 == "Bouvier") m.row = m.row.copy(_3 = "Simpson")
      else if(m.row._2 == "Homer") m.delete()
      else if(m.row._2 == "Bart") m.insert((42, "Lisa", "Simpson"))
    }

    println("After mutating:")
    users.foreach(u => println("  "+u))

    assertEquals(
      Set("Marge Simpson", "Bart Simpson", "Lisa Simpson", "Carl Carlson"),
      (for(u <- users) yield u.first ++ " " ++ u.last).list.toSet
    )
  }

  def testDeleteMutate = ifCap(jcap.mutable) {
    class T(tag: Tag) extends Table[(Int, Int)](tag, "T_DELMUTABLE") {
      def a = column[Int]("A")
      def b = column[Int]("B", O.PrimaryKey)
      def * = (a, b)
    }
    val ts = TableQuery[T]
    def tsByA = ts.findBy(_.a)

    ts.ddl.create
    ts.insertAll((1,1), (1,2), (1,3), (1,4))
    ts.insertAll((2,5), (2,6), (2,7), (2,8))

    tsByA(1).mutate(_.delete)

    assertEquals(Set((2,5), (2,6), (2,7), (2,8)), ts.to[Set])
  }
}
