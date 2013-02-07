package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{TestkitTest, TestDB}

class ColumnDefaultTest(val tdb: TestDB) extends TestkitTest {
  import tdb.profile.simple._

  case class User(id: Int, first: String, last: String)

  object A extends Table[(Int, String, Option[Boolean])]("a") {
    def id = column[Int]("id")
    def a = column[String]("a", O Default "foo")
    def b = column[Option[Boolean]]("b", O Default Some(true))
    def * = id ~ a ~ b
  }

  def test = ifCap(rcap.columnDefaults) {
    A.ddl.createStatements foreach println
    A.ddl.create
    A.id insert 42
    assertEquals(List((42, "foo", Some(true))), Query(A).list)
  }
}
