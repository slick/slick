package org.scalaquery.test

import org.junit.Test
import org.junit.Assert._
import org.scalaquery.ql._
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql.extended.{ExtendedTable => Table}
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.test.util._
import org.scalaquery.test.util.TestDB._

object TemplateTest extends DBTestObject(H2Mem, Postgres, MySQL, DerbyMem, HsqldbMem)

class TemplateTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit._

  object Users extends Table[(Int, String)]("users") {
    def id = column[Int]("id", O PrimaryKey, O AutoInc)
    def first = column[String]("first")
    def * = id ~ first
  }

  object Orders extends Table[(Int, Int, String)]("orders") {
    def userID = column[Int]("userID")
    def orderID = column[Int]("orderID", O PrimaryKey, O AutoInc)
    def product = column[String]("product")
    def * = userID ~ orderID ~ product
  }

  @Test def test() {
    db withSession {

      (Users.ddl ++ Orders.ddl) create

      Users.first.insertAll("Homer", "Marge", "Apu", "Carl", "Lenny")
      for(uid <- Users.map(_.id))
        (Orders.userID ~ Orders.product).insert(uid, if(uid < 4) "Product A" else "Product B")

      def userNameByID1(id: Int) = for(u <- Users if u.id is id.bind) yield u.first
      def q1 = userNameByID1(3)
      println("q1: " + q1.selectStatement)
      for(t <- q1) println("User: "+t)
      assertEquals(List("Apu"), q1.list)

      val userNameByID2 = for {
        id <- Parameters[Int]
        u <- Users if u.id is id
      } yield u.first
      val q2 = userNameByID2(3)
      println("q2: " + userNameByID2.selectStatement)
      for(t <- q2) println("User: "+t)
      assertEquals(List("Apu"), q2.list)

      val userNameByIDRange = for {
        Projection(min, max) <- Parameters[Int, Int]
        u <- Users if u.id >= min && u.id <= max
      } yield u.first
      val q3 = userNameByIDRange(2,5)
      println("q3: " + userNameByIDRange.selectStatement)
      for(t <- q3) println("User: "+t)
      assertEquals(List("Marge","Apu","Carl","Lenny"), q3.list)

      val userNameByIDRangeAndProduct = for {
        min ~ max ~ product <- Parameters[Int, Int, String]
        u <- Users if u.id >= min && u.id <= max && Orders.where(o => (u.id is o.userID) && (o.product is product)).exists
      } yield u.first
      val q4 = userNameByIDRangeAndProduct(2,5,"Product A")
      println("q4: " + userNameByIDRangeAndProduct.selectStatement)
      for(t <- q4) println("User: "+t)
      assertEquals(List("Marge","Apu"), q4.list)

      def userNameByIDOrAll(id: Option[Int]) = for(
        u <- Users if id.map(u.id is _.bind).getOrElse(ConstColumn(true))
      ) yield u.first
      val q5a = userNameByIDOrAll(Some(3))
      println("q5a: " + q5a.selectStatement)
      for(t <- q5a) println("User: "+t)
      assertEquals(List("Apu"), q5a.list)
      val q5b = userNameByIDOrAll(None)
      println("q5b: " + q5b.selectStatement)
      for(t <- q5b) println("User: "+t)
      assertEquals(List("Homer","Marge","Apu","Carl","Lenny"), q5b.list)
    }
  }
}
