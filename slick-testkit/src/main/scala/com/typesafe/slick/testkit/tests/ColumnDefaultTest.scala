package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{RelationalTestDB, TestkitTest}

class ColumnDefaultTest extends TestkitTest[RelationalTestDB] {
  import tdb.profile.simple._

  abstract class A[T](tag: Tag, tableName: String) extends Table[T](tag, tableName) {
    def id = column[Int]("id")
    def a = column[String]("a", O Default "foo")
    def b = column[Option[Boolean]]("b", O Default Option(true))
  }

  class B(tag: Tag) extends A[(Int, String, Option[Boolean])](tag, "b") {
    def * = (id, a, b)
  }
  lazy val bs = TableQuery[B]

  class C(tag: Tag) extends A[(Int, String, Option[Boolean], String)](tag, "c") {
    def c = column[String]("c", O Default SimpleFunction.unary[String, String]("UPPER").apply("test"))
    def * = (id, a, b, c)
  }
  lazy val cs = TableQuery[C]

  def test = ifCap(rcap.columnDefaults) {
    bs.ddl.create
    bs.map(_.id) += 42
    assertEquals(List((42, "foo", Some(true))), bs.run)

    ifCap(jcap.columnFunctionDefaults) {
      cs.ddl.create
      cs.map(_.id) += 42
      assertEquals(List((42, "foo", Some(true), "TEST")), cs.run)
    }
  }
}

