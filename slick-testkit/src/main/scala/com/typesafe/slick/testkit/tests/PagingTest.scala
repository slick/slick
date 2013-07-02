package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{RelationalTestDB, TestkitTest}

class PagingTest extends TestkitTest[RelationalTestDB] {
  import tdb.profile.simple._

  class IDs(tag: Tag) extends Table[Int](tag, "ids") {
    def id = column[Int]("id", O.PrimaryKey)
    def * = id
  }
  lazy val ids = TableQuery[IDs]

  def test {

    ids.ddl.create;
    ids ++= (1 to 10)

    val q1 = ids.sortBy(_.id)
    println("    "+q1.run)
    assertEquals((1 to 10).toList, q1.run)

    val q2 = q1 take 5
    println("    "+q2.run)
    assertEquals((1 to 5).toList, q2.run)

    ifCap(rcap.pagingDrop) {
      val q3 = q1 drop 5
      println("    "+q3.run)
      assertEquals((6 to 10).toList, q3.run)

      val q4 = q1 drop 5 take 3
      println("    "+q4.run)
      assertEquals((6 to 8).toList, q4.run)

      val q5 = q1 take 5 drop 3
      println("    "+q5.run)
      assertEquals((4 to 5).toList, q5.run)
    }

    val q6 = q1 take 0
    println("    "+q6.run)
    assertEquals(List(), q6.run)
  }
}
