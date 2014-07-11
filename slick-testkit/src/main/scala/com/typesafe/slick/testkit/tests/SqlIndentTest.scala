package com.typesafe.slick.testkit.tests

import java.util.logging.Logger

import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}
import org.junit.Assert._

import scala.slick.driver.MySQLDriver

class SqlIndentTest extends TestkitTest[JdbcTestDB] {

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

    val expectedOne = Some( """select x2.x3, x2.x4, x2.x5, x2.x6
                              |from (
                              |  select x7."id" as x3, x7."name" as x4, x7."department" as x5, cast((
                              |    select sum(x8."id")
                              |    from "managers" x8
                              |    where x8."id" = x7."id"
                              |  ) as INTEGER) as x6
                              |  from "managers" x7
                              |  where (x7."id" < 10) and (x7."id" in (
                              |    select x9."id"
                              |    from "managers" x9
                              |  ))
                              |) x2
                              |order by x2.x6""").map(_.stripMargin).map(_.toLowerCase)
    val mysqlExpectedOne = Some( """select x2.x3, x2.x4, x2.x5, x2.x6
                                   |from (
                                   |  select x7.`id` as x3, x7.`name` as x4, x7.`department` as x5, {fn convert(
                                   |    select sum(x8.`id`)
                                   |    from `managers` x8
                                   |    where x8.`id` = x7.`id`
                                   |  ,INTEGER)} as x6
                                   |  from `managers` x7
                                   |  where (x7.`id` < 10) and (x7.`id` in (
                                   |    select x9.`id`
                                   |    from `managers` x9
                                   |  ))
                                   |) x2
                                   |order by x2.x6""").map(_.stripMargin).map(_.toLowerCase)
    val expectedTwo = Some( """select x2.x3, x2.x4, x2.x5
                              |from (
                              |  select x6."id" as x3, x6."name" as x4, x6."department" as x5
                              |  from "managers" x6
                              |) x2
                              |inner join (
                              |  select x7."id" as x8, x7."name" as x9, x7."department" as x10
                              |  from "managers" x7
                              |) x11
                              |on x2.x3 = x11.x8
                              |group by x2.x3, x2.x4, x2.x5""").map(_.stripMargin).map(_.toLowerCase)
    val mysqlExpectedTwo = Some( """select x2.x3, x2.x4, x2.x5
                                   |from (
                                   |  select x6.`id` as x3, x6.`name` as x4, x6.`department` as x5
                                   |  from `managers` x6
                                   |) x2
                                   |inner join (
                                   |  select x7.`id` as x8, x7.`name` as x9, x7.`department` as x10
                                   |  from `managers` x7
                                   |) x11
                                   |on x2.x3 = x11.x8
                                   |group by x2.x3, x2.x4, x2.x5""").map(_.stripMargin).map(_.toLowerCase)

    val resultOne = managers.
      filter(t => t.id < 10 && t.id.in(managers.map(_.id))).
      map(t => (t, managers.filter(x => x.id === t.id).map(_.id).sum.asColumnOf[Option[Int]])).
      sortBy(t => t._2).selectStatement
    val resultTwo = managers.
      join(managers).on(_.id === _.id).
      groupBy { case (a, b) => a}.map { case (g, _) => g}.selectStatement
    if (tdb.driver == MySQLDriver) {
      assertEquals(mysqlExpectedOne.get, resultOne.toLowerCase)
      assertEquals(mysqlExpectedTwo.get, resultTwo.toLowerCase)
    }
    else {
      assertEquals(expectedOne.get, resultOne.toLowerCase)
      assertEquals(expectedTwo.get, resultTwo.toLowerCase)
    }

    (managers.ddl).drop
  }
}
