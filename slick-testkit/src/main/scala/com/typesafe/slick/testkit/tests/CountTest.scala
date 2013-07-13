package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{RelationalTestDB, TestkitTest}

class CountTest extends TestkitTest[RelationalTestDB] {
  import tdb.profile.simple._

  def test {
    class TestTable(tag: Tag) extends Table[Int](tag, "TEST") {
      def id = column[Int]("ID")
      def * = id
    }
    val testTable = TableQuery(new TestTable(_))

    testTable.ddl.create
    testTable ++= Seq(1, 2, 3, 4, 5)

    val q1 = Query(testTable.length)
    assertEquals(Vector(5), q1.run)

    val q2 = testTable.length
    assertEquals(5, q2.run)

    val q3 = testTable.filter(_.id < 3).length
    assertEquals(2, q3.run)

    val q4 = testTable.take(2).length
    assertEquals(2, q4.run)
  }
}
