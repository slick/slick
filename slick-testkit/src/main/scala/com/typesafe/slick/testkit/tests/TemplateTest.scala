package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{RelationalTestDB, TestkitTest}

class TemplateTest extends TestkitTest[RelationalTestDB] {
  import tdb.profile.simple._

  object Users extends Table[(Int, String)]("users") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def first = column[String]("first")
    def * = (id, first)
  }

  object Orders extends Table[(Int, Int, String)]("orders") {
    def userID = column[Int]("userID")
    def orderID = column[Int]("orderID", O.PrimaryKey, O.AutoInc)
    def product = column[String]("product")
    def * = (userID, orderID, product)
  }

  def test {

    (Users.ddl ++ Orders.ddl).create

    Users.first ++= Seq("Homer", "Marge", "Apu", "Carl", "Lenny")
    for(uid <- Users.map(_.id).run)
      (Orders.userID, Orders.product).shaped += (uid, if(uid < 4) "Product A" else "Product B")

    def userNameByID1(id: Int) = for(u <- Users if u.id is id.bind) yield u.first
    def q1 = userNameByID1(3)
    assertEquals(List("Apu"), q1.run)

    val userNameByID2 = for {
      id <- Parameters[Int]
      u <- Users if u.id is id
    } yield u.first
    val q2 = userNameByID2(3)
    assertEquals(List("Apu"), q2.run)

    val userNameByIDRange = for {
      (min, max) <- Parameters[(Int, Int)]
      u <- Users if u.id >= min && u.id <= max
    } yield u.first
    val q3 = userNameByIDRange(2,5)
    assertEquals(List("Marge","Apu","Carl","Lenny"), q3.run)

    val userNameByIDRangeAndProduct = for {
      (min, (max, product)) <- Parameters[(Int, (Int, String))]
      u <- Users if u.id >= min && u.id <= max && Orders.where(o => (u.id is o.userID) && (o.product is product)).exists
    } yield u.first
    val q4 = userNameByIDRangeAndProduct(2,(5,"Product A"))
    assertEquals(List("Marge","Apu"), q4.run)

    def userNameByIDOrAll(id: Option[Int]) = for(
      u <- Users if id.map(u.id is _.bind).getOrElse(ConstColumn(true))
    ) yield u.first
    val q5a = userNameByIDOrAll(Some(3))
    assertEquals(List("Apu"), q5a.run)
    val q5b = userNameByIDOrAll(None)
    assertEquals(List("Homer","Marge","Apu","Carl","Lenny"), q5b.run)
  }
}
