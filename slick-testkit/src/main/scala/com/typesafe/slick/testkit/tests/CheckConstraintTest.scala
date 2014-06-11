package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{JdbcTestDB, RelationalTestDB, TestkitTest}
import scala.util.Try
import java.util.logging.Logger
import scala.slick.driver.{MySQLDriver, SQLiteDriver}

class CheckConstraintTest extends TestkitTest[JdbcTestDB] {

  import tdb.profile.simple._

  override val reuseInstance = true

  class Categories(tag: Tag) extends Table[(Int, String)](tag, "cat_checkconstraint") {
    def id = column[Int]("id")

    def name = column[String]("name")

    def checkConstraintOne = checkConstraint("chk_1", (t: Categories) => (t.id < 10))

    def checkConstraintTwo = checkConstraint("chk_2", (t: Categories) => (t.name.like("%10%")))

    def checkConstraintThree = checkConstraint("chk_3", (t: Categories) => (t.id < 20 && t.name.like("%100%")))

    def * = (id, name)
  }

  def testCheckConstraint(): Unit = {
    tdb.driver match {
      case SQLiteDriver => return // currently(20140612), it looks like that there is no support with "alter table" while it is possible with "create table" in SQLite3.7.
      case MySQLDriver => return // currently(20140612), MySQL5.5's support is just fake.
      case _ =>
    }
    val categories = TableQuery[Categories]
    categories.ddl.create
    val succ1 = Try(categories.insert((9, "1001")))
    val fail2 = Try(categories.insert((9, "101"))) // because three
    val fail3 = Try(categories.insert((20, "1001"))) // because three
    val fail4 = Try(categories.insert((9, "11"))) // because two
    val fail5 = Try(categories.insert((19, "10"))) // because one
    assertTrue(succ1.isSuccess)
    assertTrue(fail2.isFailure)
    assertTrue(fail3.isFailure)
    assertTrue(fail4.isFailure)
    assertTrue(fail5.isFailure)
    categories.ddl.drop
  }
}
