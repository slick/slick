package test

import java.lang.Integer
import com.novocode.squery.combinator._
import com.novocode.squery.combinator.Implicit._
import com.novocode.squery.session._
import com.novocode.squery.session.SessionFactory._

object SQuery2Test2 {
  def main(args: Array[String]) {

    case class User(id: Integer, first: String, last: String)

    object Users extends Table[(Integer, String, String)]("users") {
      def id = intColumn("id", O.AutoInc, O.NotNull)
      def first = stringColumn("first")
      def last = stringColumn("last")
      def * = id ~ first ~ last

      //def orders = Orders where { _.userID is id }
    }

    object Orders extends Table[(Integer, Integer, String)]("orders") {
      def userID = intColumn("userID", O.NotNull)
      def orderID = intColumn("orderID", O.AutoInc, O.NotNull)
      def product = stringColumn("product")
      def * = userID ~ orderID ~ product
    }

    val sf = new DriverManagerSessionFactory("org.h2.Driver", "jdbc:h2:mem:test1")
    sf withSession {

      println(Users.createTableStatement)
      println(Orders.createTableStatement)
      Users.createTable
      Orders.createTable
      val ins1 = (Users.first ~ Users.last).insert("Homer", "Simpson")
      val ins2 = (Users.first ~ Users.last).insertAll(
        ("Marge", "Simpson"), ("Apu", "Nahasapeemapetilon"), ("Carl", "Carlson"), ("Lenny", "Leonard") )
      val ins3 = Users.first.insertAll("Santa's Little Helper", "Snowball")
      println("Inserted "+(ins1+ins2+ins3)+" users")

      val q1 = for(u <- Users) yield u
      println("q1: " + q1.selectStatement)
      for(t <- q1) println("User tuple: "+t)
      val allUsers = q1.mapResult{ case (id,f,l) => User(id,f,l) }.list
      for(u <- allUsers) println("User object: "+u)

      val q2 = for(u <- Users where {_.first is "Apu" }) yield u.last ~ u.id
      println("q2: " + q2.selectStatement)
      println("Apu's last name and ID are: " + q2.first)

      for(u <- allUsers
          if u.first != "Apu" && u.first != "Snowball"; i <- 1 to 2)
        Orders.insert(u.id, null, "Gizmo "+((Math.random*10)+1).toInt)

      val q3 = for {
        u <- Users
        o <- Orders where { o => (u.id is o.userID) && (u.last isNot null) }
      } yield (u.first ~ u.last ~ o.orderID ~ o.product).sortBy(u.first)
      println("q3: " + q3.selectStatement)
      println("All Orders by Users with a last name by first name:")
      q3.foreach(o => println("  "+o))

      val q4 = for (
        u <- Users;
        o <- Orders
          where { o => o.orderID is queryToSubQuery(for { o2 <- Orders where(o.userID is _.userID) } yield o2.orderID.max) }
          where { _.userID is u.id }
      ) yield u.first ~ o.orderID
      println("q4: " + q4.selectStatement)
      println("Latest Order per User:")
      q4.foreach(o => println("  "+o))

      def maxOfPer[T <: TableBase.T_]
        (c: T, m: (T => Column[Integer]), p: (T => Column[_])) =
        c where { o => m(o) is queryToSubQuery(for { o2 <- c where( n => p(o) is p(n)) } yield m(o2).max) }

      val q4b = for (
        u <- Users;
        o <- maxOfPer[Orders.type](Orders, _.orderID, _.userID)
          where { _.userID is u.id }
      ) yield u.first ~ o.orderID
      println("q4b: " + q4b.selectStatement)
      println("Latest Order per User, using maxOfPer:")
      q4b.foreach(o => println("  "+o))

      val q5 = Users where { _.id notIn Orders.map(_.userID) }
      println("q5: " + q5.selectStatement)
      println("Users without Orders:")
      q5.foreach(o => println("  "+o))

      println("q5: " + q5.deleteStatement)
      println("Deleting them...")
      val deleted = q5.delete
      println("Deleted "+deleted+" rows")

      val q6 = q5.map(_.count)
      println("q6: " + q6.selectStatement)
      println("Users without Orders left: " + q6.first)
    }
  }
}
