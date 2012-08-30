package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import scala.slick.ast.Dump
import com.typesafe.slick.testkit.util.{TestkitTest, TestDB}

class CountTest(val tdb: TestDB) extends TestkitTest {
  import tdb.profile.simple._

  def test {
    object TestTable extends Table[Int]("TEST") {
      def id = column[Int]("ID")
      def * = id
    }
    TestTable.ddl.create
    TestTable.insertAll(1, 2, 3, 4, 5)

    val q1 = Query(TestTable.length)
    Dump(q1, "q1: ")
    println("q1: "+q1.selectStatement)
    assertEquals(5, q1.first)

    val q2 = Query(Query(TestTable).length)
    Dump(q2, "q2: ")
    println("q2: "+q2.selectStatement)
    assertEquals(5, q2.first)

    val q3 = Query(TestTable.filter(_.id < 3).length)
    Dump(q3, "q3: ")
    println("q3: "+q3.selectStatement)
    assertEquals(2, q3.first)

    val q4 = Query(Query(TestTable).take(2).length)
    Dump(q4, "q4: ")
    println("q4: "+q4.selectStatement)
    assertEquals(2, q4.first)
  }
}
