package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}
import scala.slick.driver.PostgresDriver
import scala.slick.ast.Library.SqlFunction
import scala.slick.ast.LiteralNode
import org.junit.Assert._

class PgFeatureTests extends TestkitTest[JdbcTestDB] {

  def testAggregates {
    if (List("postgres").contains(tdb.confName)) {

      val driver = tdb.driver.asInstanceOf[PostgresDriver]
      import driver.simple._

      ///
      case class Tab(name: String, count: Int, x: Double, y: Double)

      class Tabs(tag: Tag) extends Table[Tab](tag, "test1_tab1") {
        def name = column[String]("COL2")
        def count = column[Int]("count")
        def x = column[Double]("x")
        def y = column[Double]("y")

        def * = (name, count, x, y) <> (Tab.tupled, Tab.unapply)
      }
      val tabs = TableQuery(new Tabs(_))

      (tabs.ddl).create

      ///
      tabs ++= Seq(
        Tab("foo", 1, 103.05, 179.17),
        Tab("quux", 3, 57.39, 99.07),
        Tab("bar", 2, 35.89, 101.33),
        Tab("bar", 11, 73.75, 28.57)
      )

      println("============== testing pg aggregate function support =============")

      object AggLibrary {
        val Avg = new SqlFunction("avg")
        val StringAgg = new SqlFunction("string_agg")
        val Corr = new SqlFunction("corr")
      }

      case class Avg[T]() extends UnaryAggFuncPartsBasic[T, T](AggLibrary.Avg)
      case class StringAgg(delimiter: String) extends UnaryAggFuncPartsBasic[String, String](AggLibrary.StringAgg, List(LiteralNode(delimiter)))
      case class Corr() extends BinaryAggFuncPartsBasic[Double, Double](AggLibrary.Corr)

      val q = tabs.map(t => (t.name ^: StringAgg(",").forDistinct().orderBy(t.name), t.count ^: Avg[Int]))
      println(s"q = ${q.selectStatement}")
      assertEquals(("bar,foo,quux", 4), q.first)

      val q1 = tabs.map(t => (t.y, t.x) ^: Corr())
      println(s"q = ${q1.selectStatement}")
      assertEquals(0.45, q1.first, 0.01)
    }
  }
}