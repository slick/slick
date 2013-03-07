package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{RelationalTestDB, TestkitTest}

class CountTest extends TestkitTest[RelationalTestDB] {
  import tdb.profile.simple._

  def test {
    object TestTable extends Table[Int]("TEST") {
      def id = column[Int]("ID")
      def * = id
    }
    TestTable.ddl.create
    TestTable ++= Seq(1, 2, 3, 4, 5)

    val q1 = Query(TestTable.length)
    assertEquals(Vector(5), q1.run)

    val q2 = Query(Query(TestTable).length)
    assertEquals(Vector(5), q2.run)

    val q2b = Query(TestTable).length
    assertEquals(5, q2b.run)

    val q3 = TestTable.filter(_.id < 3).length
    assertEquals(2, q3.run)

    val q4 = Query(TestTable).take(2).length
    assertEquals(2, q4.run)
  }
}
