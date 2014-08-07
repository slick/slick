package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{RelationalTestDB, TestkitTest}

/**
 * Created by KangWoo,Lee on 14. 8. 7.
 */
class VariableLengthSortByTest extends TestkitTest[RelationalTestDB] {

  import tdb.profile.simple._

  override val reuseInstance = true

  class Managers(tag: Tag) extends Table[(Int, String, String)](tag, "managers") {
    def id = column[Int]("id")
    def name = column[String]("name")
    def department = column[String]("department")
    def * = (id, name, department)
  }

  lazy val managers = TableQuery[Managers]

  def testBasic {
    (managers.ddl).create

    managers ++= Seq(
      (1, "Peter", "HR"),
      (2, "Peter", "HR"),
      (3, "Peter", "IT")
    )

    val expected = Seq(
      (2, "Peter", "HR"),
      (1, "Peter", "HR"),
      (3, "Peter", "IT")
    )

    val result = managers.sortByUsingList(x => List(x.name.asc, x.department.asc, x.id.desc)).run
    assertEquals(expected, result)
  }
}
