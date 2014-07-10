package com.typesafe.slick.testkit.tests

import java.util.logging.Logger

import com.typesafe.slick.testkit.util.{JdbcTestDB, RelationalTestDB, TestkitTest}
import org.junit.Assert._

import scala.collection.mutable
import scala.slick.lifted

class SelectDistinctTest extends TestkitTest[JdbcTestDB] {

  import tdb.profile.simple._

  def test {
    class TestTable(tag: Tag) extends Table[(Int, Int)](tag, "TEST") {
      def numberOne = column[Int]("NUMONE")

      def numberTwo = column[Int]("NUMTWO")

      def * = (numberOne, numberTwo)
    }
    val testTable = TableQuery(new TestTable(_))

    testTable.ddl.create
    val data = Seq((1, 1), (1, 1), (1, 2), (1, 2), (2, 3))
    testTable ++= data

    val resultSqlOne = Query(1, 2).union(Query(3, 4)).map(t => (t, Option(4))).distinct.sortBy(t => (t._1._1, t._1._2)).selectStatement
    val resultOne = Query(1, 2).union(Query(3, 4)).map(t => (t, Option(4))).distinct.sortBy(t => (t._1._1, t._1._2)).list
//    val resultSqlTwo = Query(1, 2).union(Query(3, 4)).map(t => (t, Option(4))).groupBy(t => t).map(_._1).sortBy(t => (t._1._1, t._1._2)).selectStatement
    val resultTwo = Query(1, 2).union(Query(3, 4)).map(t => (t, Option(4))).groupBy(t => t).map(_._1).sortBy(t => (t._1._1, t._1._2)).list
    val resultSqlThree = testTable.groupBy(_.numberOne).map(x => (x._1, x._2.map(_.numberTwo).sum)).sortBy(t => (t._1, t._2)).selectStatement
    val resultThree = testTable.groupBy(_.numberOne).map(x => (x._1, x._2.map(_.numberTwo).sum)).sortBy(t => (t._1, t._2)).list
    val resultFour = testTable.distinct.sortBy(t => (t.numberOne, t.numberTwo)).list
    val resultSqlFour = testTable.distinct.sortBy(t => (t.numberOne, t.numberTwo)).selectStatement
    val resultFive = testTable.groupBy(t => t).map(_._1).sortBy(t => (t.numberOne, t.numberTwo)).list
//    val resultSqlFive = testTable.groupBy(t => t).map(_._1).sortBy(t => (t.numberOne, t.numberTwo)).selectStatement

    assertTrue(-1 == resultSqlOne.indexOf("group by"))
    assertTrue(-1 != resultSqlOne.indexOf("distinct"))
//    assertTrue(-1 == resultSqlTwo.indexOf("group by"))
//    assertTrue(-1 != resultSqlTwo.indexOf("distinct"))
    assertTrue(-1 != resultSqlThree.indexOf("group by"))
    assertTrue(-1 == resultSqlThree.indexOf("distinct"))
    assertTrue(-1 == resultSqlFour.indexOf("group by"))
    assertTrue(-1 != resultSqlFour.indexOf("distinct"))
//    assertTrue(-1 == resultSqlFive.indexOf("group by"))
//    assertTrue(-1 != resultSqlFive.indexOf("distinct"))

    val expectedOne = List((1, 2), (3, 4)).map(x => (x, Some(4))).distinct
    assertEquals(expectedOne, resultOne)
    assertEquals(expectedOne, resultTwo)

    val map = mutable.HashMap[Int, Int]()
    data.foreach {
      case (a, b) => map.put(a, map.get(a).getOrElse(0) + b)
    }
    val expectedMap = map.map {
      case (a, b) => (a, Some(b))
    }
    val expectedThree = resultThree.map {
      case (a, b) => expectedMap.get(a).map(_ == b).getOrElse(false)
    }.forall(_ == true)
    assertTrue(expectedThree)
    val expectedTwo = data.distinct
    assertEquals(expectedTwo, resultFour)
    assertEquals(expectedTwo, resultFive)

    val query = testTable.
      map(t => (t.numberOne, t.numberTwo,
      testTable.
        filter(x => x.numberOne === t.numberOne).
        map(_.numberTwo).sum))
    val resultSix = query.sortBy(t => (t._1, t._2, t._3)).run
    val resultSeven = query.distinct.sortBy(t => (t._1, t._2, t._3)).run
    def macroOne(i: Int): Option[Int] = Some(data.filter(_._1 == i).map(_._2).reduceLeft((a: Int, b: Int) => a + b))
    val expectedSix = data.map(x => (x._1, x._2, macroOne(x._1))).sortBy(x => (x._1, x._2, x._3))
    val expectedSeven = expectedTwo.map(x => (x._1, x._2, macroOne(x._1))).sortBy(x => (x._1, x._2, x._3))
    assertEquals(expectedSix, resultSix)
    assertEquals(expectedSeven, resultSeven)

    testTable.ddl.drop
  }
}
