package org.scalaquery.test

import org.junit.{AfterClass, BeforeClass, Test}
import org.junit.Assert._
import org.scalaquery.ql._
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql.extended.H2Driver.Implicit._
import org.scalaquery.ql.extended.{ExtendedTable => Table}
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession

object TablesInObjectTest {
  def main(args: Array[String]) { new TablesInObjectTest().test1() }

  object Categories extends Table[Int]("categories") {
    def id = column[Int]("id")
    def * = id
  }

  /* This needs to be a val instead of an object with a singleton type
   * because scalac assumes that the object is a singleton and pulls the
   * wrong "this" reference into the closure -- where "category" is
   * referenced -- when it is used in a clone()d Posts instance.
   */
  val Posts = new Table[Int]("posts") {
    def category = column[Int]("category")
    def * = category
    def categoryJoin = Categories.where(_.id === category)
  }
}

class TablesInObjectTest {
  import TablesInObjectTest._

  @Test def test1() = {

    def categoryJoin(p: Posts.type) = Categories.where(_.id === p.category)

    val q1 = for {
      p <- Posts
      c <- categoryJoin(p.asInstanceOf[Posts.type])
    } yield p.category ~ c.id
    //q1.dump("Local function")
    val sel1 = q1.selectStatement
    println("Local function:  "+sel1)

    val q2 = for {
      p <- Posts
      c <- p.categoryJoin
    } yield p.category ~ c.id
    //q1.dump("Method on table")
    val sel2 = q2.selectStatement
    println("Method on table: "+sel2)

    assertEquals(sel1, sel2)
  }
}
