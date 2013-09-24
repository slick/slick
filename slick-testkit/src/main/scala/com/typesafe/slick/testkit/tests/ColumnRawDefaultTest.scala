package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}

class ColumnRawDefaultTest extends TestkitTest[JdbcTestDB] {
  import tdb.profile.simple._

  class A(tag: Tag) extends Table[(Int, String, Option[Boolean])](tag, "a") {
    def id = column[Int]("id")
    def a = column[String]("a", O RawDefault "'foo'") //can't think of a common sql function to test yet
    def b = column[Option[Boolean]]("b", O RawDefault "1") //can't think of a common sql function to test yet
    def * = (id, a, b)
  }
  lazy val as = TableQuery[A]

  def test = ifCap(rcap.columnDefaults) {
    as.ddl.create
    as.map(_.id) += 42
    assertEquals(List((42, "foo", Some(true))), as.run)
  }
}

