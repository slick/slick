package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{ RelationalTestDB, TestkitTest }

class ColumnDefaultTest extends TestkitTest[RelationalTestDB] {
  import tdb.profile.simple._

  class A(tag: Tag) extends Table[(Int, String, Option[Boolean])](tag, "a") {
    def id = column[Int]("id")
    def a = column[String]("a", O.Default("foo"))
    def b = column[Boolean]("b", O.Default(true))
    def * = (id, a, b.?)
  }
  lazy val as = TableQuery[A]

  class B(tag: Tag) extends Table[(Int, String, Option[Boolean], String)](tag, "b") {
    def id = column[Int]("id")
    def a = column[String]("a", O.Default("foo"))
    def b = column[Boolean]("b", O.Default(true))
    def c = column[String]("c", O.Default(SimpleFunction.unary[String, String]("UPPER").apply("test")))
    def * = (id, a, b.?, c)
  }
  lazy val bs = TableQuery[B]

  def test = ifCap(rcap.columnDefaults) {

    as.ddl.create
    as.map(_.id) += 42
    assertEquals(List((42, "foo", Some(true))), as.run)
      
	ifCap(jcap.columnFunctionDefaults) {
		bs.ddl.create
	    bs.map(_.id) += 42
	    assertEquals(List((42, "foo", Some(true), "TEST")), bs.run)
	}
  }
}

