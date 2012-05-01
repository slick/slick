package scala.slick.test.ql

import org.junit.Test
import org.junit.Assert._
import scala.slick.ql._
import scala.slick.ql.TypeMapper._
import scala.slick.driver.{ExtendedTable => Table}
import scala.slick.session._
import scala.slick.session.Database.threadLocalSession
import scala.slick.testutil._
import scala.slick.testutil.TestDB._

object TemplateTest extends DBTestObject(H2Mem, SQLiteMem, Postgres, MySQL, DerbyMem, HsqldbMem, MSAccess, SQLServer)

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

      (Users.ddl ++ Orders.ddl).createStatements foreach println
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
        (min, max) <- Parameters[(Int, Int)]
        u <- Users if u.id >= min && u.id <= max
      } yield u.first
      val q3 = userNameByIDRange(2,5)
      println("q3: " + userNameByIDRange.selectStatement)
      for(t <- q3) println("User: "+t)
      assertEquals(List("Marge","Apu","Carl","Lenny"), q3.list)

      val userNameByIDRangeAndProduct = for {
        (min, (max, product)) <- Parameters[(Int, (Int, String))]
        u <- Users if u.id >= min && u.id <= max && Orders.where(o => (u.id is o.userID) && (o.product is product)).exists
      } yield u.first
      val q4 = userNameByIDRangeAndProduct(2,(5,"Product A"))
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
