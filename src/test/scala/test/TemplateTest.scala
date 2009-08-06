package test

import com.novocode.squery.combinator._
import com.novocode.squery.combinator.Implicit._
import com.novocode.squery.session._
import com.novocode.squery.session.Database._
import com.novocode.squery.session.TypeMapper._

object TemplateTest {
  def main(args: Array[String]) {

    object Users extends Table[(Int, String)]("users") {
      def id = column[Int]("id", O AutoInc, O NotNull)
      def first = column[String]("first", O NotNull)
      def * = id ~ first
    }

    object Orders extends Table[(Int, Int, String)]("orders") {
      def userID = column[Int]("userID", O NotNull)
      def orderID = column[Int]("orderID", O AutoInc, O NotNull)
      def product = column[String]("product")
      def * = userID ~ orderID ~ product
    }

    Class.forName("org.h2.Driver")
    Database.forURL("jdbc:h2:mem:test1") withSession {

      Users.createTable
      Orders.createTable
      Users.first.insertAll("Homer", "Marge", "Apu", "Carl", "Lenny")
      for(uid <- Users.map(_.id))
        (Orders.userID ~ Orders.product).insert(uid, if(uid < 4) "Product A" else "Product B")

      def userNameByID1(id: Int) = for(u <- Users if u.id is id.bind) yield u.first
      def q1 = userNameByID1(3)
      println("q1: " + q1.selectStatement)
      for(t <- q1) println("User: "+t)

      val userNameByID2 = for {
        id <- Parameters[Int]
        u <- Users if u.id is id
      } yield u.first
      val q2 = userNameByID2(3)
      println("q2: " + q2.selectStatement)
      for(t <- q2) println("User: "+t)

      val userNameByIDRange = for {
        Projection(min, max) <- Parameters[Int, Int]
        u <- Users if u.id >= min && u.id <= max
      } yield u.first
      val q3 = userNameByIDRange(2,5)
      println("q3: " + q3.selectStatement)
      for(t <- q3) println("User: "+t)

      val userNameByIDRangeAndProduct = for {
        min ~ max ~ product <- Parameters[Int, Int, String]
        u <- Users if u.id >= min && u.id <= max && Orders.where(o => (u.id is o.userID) && (o.product is product)).exists
      } yield u.first
      val q4 = userNameByIDRangeAndProduct(2,5,"Product A")
      println("q4: " + q4.selectStatement)
      for(t <- q4) println("User: "+t)

      def userNameByIDOrAll(id: Option[Int]) = for(
        u <- Users if id.map(u.id is _.bind).getOrElse(ConstColumn(true))
      ) yield u.first
      val q5a = userNameByIDOrAll(Some(3))
      println("q5a: " + q5a.selectStatement)
      for(t <- q5a) println("User: "+t)
      val q5b = userNameByIDOrAll(None)
      println("q5b: " + q5b.selectStatement)
      for(t <- q5b) println("User: "+t)
    }
  }
}
