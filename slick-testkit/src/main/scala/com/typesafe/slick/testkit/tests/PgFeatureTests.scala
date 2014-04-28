package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}
import scala.slick.driver.PostgresDriver
import scala.slick.ast.Library.SqlFunction
import scala.slick.ast.LiteralNode
import org.junit.Assert._

class PgFeatureTests extends TestkitTest[JdbcTestDB] {

  def testInherits {
    if (List("postgres").contains(tdb.confName)) {

      val driver = tdb.driver.asInstanceOf[PostgresDriver]
      import driver.simple._

      ///
      abstract class BaseT[T](tag: Tag, tname: String = "test_tab1") extends Table[T](tag, tname) {
        def col1 = column[String]("COL1")
        def col2 = column[String]("COL2")
        def col3 = column[String]("COL3")
        def col4 = column[Int]("COL4", O.PrimaryKey)
      }

      case class Tab(col1: String, col2: String, col3: String, col4: Int)

      class Tabs(tag: Tag) extends BaseT[Tab](tag, "test_tab1") {
        def * = (col1, col2, col3, col4) <> (Tab.tupled, Tab.unapply)
      }
      val tabs = TableQuery[Tabs]

      ///
      case class Tab1(col1: String, col2: String, col3: String, col4: Int, col5: Long)

      class Tabs1(tag: Tag) extends BaseT[Tab1](tag, "test_tab2") with InheritingTable {
        val inherited = tabs.baseTableRow
        def col5 = column[Long]("col5")

        def * = (col1, col2, col3, col4, col5) <> (Tab1.tupled, Tab1.unapply)
      }
      val tabs1 = TableQuery[Tabs1]

      ////////////////////////////////////////////////////////////
      (tabs.ddl ++ tabs1.ddl).create

      ///
      tabs ++= Seq(
        Tab("foo", "bar",  "bat", 1),
        Tab("foo", "bar",  "bat", 2),
        Tab("foo", "quux", "bat", 3),
        Tab("baz", "quux", "bat", 4)
      )
      tabs1 ++= Seq(
        Tab1("plus", "bar",  "bat", 5, 101),
        Tab1("plus", "quux", "bat", 6, 102)
      )

      ///
      val expected = Seq(
        Tab("foo", "bar",  "bat", 1),
        Tab("foo", "bar",  "bat", 2),
        Tab("foo", "quux", "bat", 3),
        Tab("baz", "quux", "bat", 4),
        Tab("plus", "bar",  "bat", 5),
        Tab("plus", "quux", "bat", 6)
      )
      val q = tabs.sortBy(_.col4)
      println(s"q = ${q.selectStatement}")
      assertEquals(expected, q.list)

      val expected1 = Seq(
        Tab1("plus", "bar",  "bat", 5, 101),
        Tab1("plus", "quux", "bat", 6, 102)
      )
      val q1 = tabs1.sortBy(_.col4)
      println(s"q1 = ${q1.selectStatement}")
      assertEquals(expected1, q1.list)

      //
      tabs1.filter(_.col4 === 5.bind).mutate { m =>
        m.row = m.row.copy(col3 = "bat1")
      }
      val q2 = tabs1.filter(_.col4 === 5.bind)
      val expect2 = Tab1("plus", "bar",  "bat1", 5, 101)
      assertEquals(expect2, q2.first)

      // NOTES: (auto) reverse order!!!
      (tabs.ddl ++ tabs1.ddl).drop
    }
  }

  def testAggregates {
    if (List("postgres").contains(tdb.confName)) {

      val driver = tdb.driver.asInstanceOf[PostgresDriver]
      import driver.simple._

      ///
      case class Tab(name: String, count: Int)

      class Tabs(tag: Tag) extends Table[Tab](tag, "test1_tab1") {
        def name = column[String]("COL2")
        def count = column[Int]("count")

        def * = (name, count) <> (Tab.tupled, Tab.unapply)
      }
      val tabs = TableQuery[Tabs]

      (tabs.ddl).create

      ///
      tabs ++= Seq(
        Tab("foo", 1),
        Tab("quux", 3),
        Tab("bar", 2),
        Tab("bar", 11)
      )

      println("============== testing pg aggregate function support =============")

      object AggLibrary {
        val Avg = new SqlFunction("avg")
        val StringAdd = new SqlFunction("string_add")
      }

      case class Avg[T]() extends AggFuncStarter[T,T](AggLibrary.Avg)
      case class StringAdd(delimiter: String) extends AggFuncStarter[String,String](AggLibrary.StringAdd, List(LiteralNode(delimiter)))

      val q = tabs.map(t => (t.name ^: StringAdd(",").forDistinct().orderBy(t.name), t.count ^: Avg[Int]))
      println(s"q = ${q.selectStatement}")
      assertEquals(("bar,foo,quux", 4), q.first)
    }
  }
}