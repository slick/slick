package com.typesafe.slick.testkit.tests

import scala.slick.ast.Library.SqlFunction
import org.junit.Assert._
import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}

class WindowFuncTest extends TestkitTest[JdbcTestDB] {
  import tdb.profile.simple._

  def testWindowFunctions {
    if (List("postgres").contains(tdb.confName)) {
      import scala.slick.lifted.FunctionSymbolExtensionMethods._

      case class Tab(col1: String, col2: String, col3: String, col4: Int)

      class Tabs(tag: Tag) extends Table[Tab](tag, "TAB_Window_func") {
        def col1 = column[String]("COL1")
        def col2 = column[String]("COL2")
        def col3 = column[String]("COL3")
        def col4 = column[Int]("COL4")

        def * = (col1, col2, col3, col4) <> (Tab.tupled, Tab.unapply)
      }
      val Tabs = TableQuery[Tabs]

      Tabs.ddl.create
      Tabs ++= Seq(
        Tab("foo", "bar",  "bat", 1),
        Tab("foo", "bar",  "bat", 2),
        Tab("foo", "quux", "bat", 3),
        Tab("baz", "quux", "bat", 4),
        Tab("az", "quux", "bat", 5)
      )

      println("============== testing window functions =============")
      val Avg = new SqlFunction("avg")

      val q = Tabs.map(r => {
        val avg4 = Avg.column[Int](r.col4.toNode)
        val w = Over.partitionBy(r.col1).orderBy(r.col1, r.col4)
        (r.col1, r.col2, r.col4, avg4 :: w)
      })
      println(q.selectStatement)
      val expected = List(
        ("az","quux",5,5),
        ("baz","quux",4,4),
        ("foo","bar",1,1),
        ("foo","bar",2,1),
        ("foo","quux",3,2)
      )
      assertEquals(expected, q.list)

      val q1 = Tabs.filter(r => r.col4 < 5).map(r => {
        val avg4 = Avg.column[Int](r.col4.toNode)
        val w = Over.partitionBy(r.col1).orderBy(r.col1, r.col4.desc).rowsFrame(RowCursor.BoundPreceding(3), Some(RowCursor.CurrentRow))
        (r.col1, r.col2, r.col4, avg4 :: w)
      })
      println(q1.selectStatement)
      val expected1 = List(
        ("baz","quux",4,4),
        ("foo","quux",3,3),
        ("foo","bar",2,2),
        ("foo","bar",1,2)
      )
      assertEquals(expected1, q1.list)
    }
  }
}
