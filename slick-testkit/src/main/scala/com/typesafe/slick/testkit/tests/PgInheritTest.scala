package com.typesafe.slick.testkit.tests

import scala.slick.driver.PostgresDriver
import org.junit.Assert._
import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}

class PgInheritTest extends TestkitTest[JdbcTestDB] {

  def testPgInherits {
    if (List("postgres").contains(tdb.confName)) {
      import PostgresDriver.simple._

      case class Tab(col1: String, col2: String, col3: String, col4: Int)

      class Tabs(tag: Tag) extends Table[Tab](tag, "test_tab1") {
        def col1 = column[String]("COL1")
        def col2 = column[String]("COL2")
        def col3 = column[String]("COL3")
        def col4 = column[Int]("COL4")

        def * = (col1, col2, col3, col4) <> (Tab.tupled, Tab.unapply)
      }
      val tabs = TableQuery[Tabs]

      ///
      case class Tab1(col1: String, col2: String, col3: String, col4: Int, col5: Long)

      class Tabs1(tag: Tag) extends SubTable[Tab1](tag, "test_tab2") {
        val h = new Tabs(tag)
        def col5 = column[Long]("col5")

        def * = (h.col1, h.col2, h.col3, h.col4, col5) <> (Tab1.tupled, Tab1.unapply)
      }
      val tabs1 = TableQuery[Tabs1]

      // use this way, because I encountered compiler error like below:
      // ----------------
      // value dll is not a member of scala.slick.lifted.TableQuery[PgInheritTest.this.Tabs,PgInheritTest.this.Tabs#TableElementType]
      // ----------------
      // And I can't find out why
      val ddl1 = new PostgresDriver.TableDDLBuilder(tabs.baseTableRow).buildDDL
      val ddl2 = new PostgresDriver.TableDDLBuilder(tabs1.baseTableRow).buildDDL

      (ddl1 ++ ddl2).create

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
      val q1 = tabs1.sortBy(_.h.col4)
      println(s"q1 = ${q1.selectStatement}")
      assertEquals(expected1, q1.list)

      // NOTES: reverse order!!!
      (ddl2 ++ ddl1).drop
    }
  }
}