package test

import com.novocode.squery.combinator._
import com.novocode.squery.combinator.Implicit._
import com.novocode.squery.session._
import com.novocode.squery.session.Database._
import com.novocode.squery.session.TypeMapper._

object SQuery2Test2 {
  def main(args: Array[String]) {

    case class User(id: Int, first: String, last: String)

    object Users extends Table[(Int, String, Option[String])]("users") {
      def id = column[Int]("id", O AutoInc, O NotNull)
      def first = column[String]("first", O Default "NFN", O DBType "varchar(64)")
      def last = column[Option[String]]("last")
      def * = id ~ first ~ last

      def orders = Orders where { _.userID is id }
    }

    object Orders extends Table[(Int, Int, String, Boolean, Option[Boolean])]("orders") {
      def userID = column[Int]("userID", O NotNull)
      def orderID = column[Int]("orderID", O AutoInc, O NotNull)
      def product = column[String]("product")
      def shipped = column[Boolean]("shipped", O Default false, O NotNull)
      def rebate = column[Option[Boolean]]("rebate", O Default Some(false))
      def * = userID ~ orderID ~ product ~ shipped ~ rebate
    }

    Class.forName("org.h2.Driver")
    Database.forURL("jdbc:h2:mem:test1") withSession {

      println(Users.createTableStatement)
      println(Orders.createTableStatement)
      Users.createTable
      Orders.createTable
      val ins1 = (Users.first ~ Users.last).insert("Homer", Some("Simpson"))
      val ins2 = (Users.first ~ Users.last).insertAll(
        ("Marge", Some("Simpson")), ("Apu", Some("Nahasapeemapetilon")), ("Carl", Some("Carlson")), ("Lenny", Some("Leonard")) )
      val ins3 = Users.first.insertAll("Santa's Little Helper", "Snowball")
      println("Inserted "+(ins1+ins2+ins3)+" users")

      val q1 = for(u <- Users) yield u.id ~ u.first ~ u.last.orElse(null)
      println("q1: " + q1.selectStatement)
      for(t <- q1) println("User tuple: "+t)
      val allUsers = q1.mapResult{ case (id,f,l) => User(id,f,l.getOrElse(null)) }.list
      for(u <- allUsers) println("User object: "+u)

      val q1b = for(u <- Users) yield u.id ~ u.first.? ~ u.last
      for(t <- q1b) println("With Options: "+t)

      val q2 = for(u <- Users if u.first is "Apu".bind) yield u.last ~ u.id
      println("q2: " + q2.selectStatement)
      println("Apu's last name and ID are: " + q2.first)

      for(u <- allUsers
          if u.first != "Apu" && u.first != "Snowball"; i <- 1 to 2)
        (Orders.userID ~ Orders.product ~ Orders.shipped ~ Orders.rebate).insert(
          u.id, "Gizmo "+((Math.random*10)+1).toInt, i == 2, Some(u.first == "Marge"))

      val q3 = for (
        u <- Users if u.last isNotNull;
        o <- u.orders
             orderBy u.first
      ) yield u.first ~ u.last ~ o.orderID ~ o.product ~ o.shipped ~ o.rebate
      println("q3: " + q3.selectStatement)
      println("All Orders by Users with a last name by first name:")
      q3.foreach(o => println("  "+o))

      val q4 = for (
        u <- Users;
        o <- Orders
          if (o.orderID in (for { o2 <- Orders where(o.userID is _.userID) } yield o2.orderID.max))
             && (o.userID is u.id)
      ) yield u.first ~ o.orderID
      println("q4: " + q4.selectStatement)
      println("Latest Order per User:")
      q4.foreach(o => println("  "+o))

      def maxOfPer[T <: TableBase.T_]
        (c: T, m: (T => Column[Int]), p: (T => Column[Int])) =
        c where { o => m(o) in (for { o2 <- c if p(o) is p(o2) } yield m(o2).max) }

      val q4b = for (
        u <- Users;
        o <- maxOfPer[Orders.type](Orders, _.orderID, _.userID)
          if o.userID is u.id
      ) yield u.first ~ o.orderID
      println("q4b: " + q4b.selectStatement)
      println("Latest Order per User, using maxOfPer:")
      q4b.foreach(o => println("  "+o))

      val q4c = for (
        u <- Users;
        o <- Orders if o.userID is u.id;
        _ <- Query groupBy u.id
                   having { _ => o.orderID.max > 5 }
                   orderBy o.orderID.max
      ) yield u.first ~ o.orderID.max
      println("q4c: " + q4c.selectStatement)
      println("Latest Order per User, using GroupBy, with orderID > 5:")
      q4c.foreach(o => println("  "+o))

      val b1 = Orders.where( o => o.shipped && o.shipped ).map( o => o.shipped && o.shipped )
      val b2 = Orders.where( o => o.shipped && o.rebate ).map( o => o.shipped && o.rebate )
      val b3 = Orders.where( o => o.rebate && o.shipped ).map( o => o.rebate && o.shipped )
      val b4 = Orders.where( o => o.rebate && o.rebate ).map( o => o.rebate && o.rebate )
      val b5 = Orders.where( o => !o.shipped ).map( o => !o.shipped )
      val b6 = Orders.where( o => !o.rebate ).map( o => !o.rebate )
      val b7 = Orders.map( o => o.shipped is o.shipped )
      val b8 = Orders.map( o => o.rebate is o.shipped )
      val b9 = Orders.map( o => o.shipped is o.rebate )
      val b10 = Orders.map( o => o.rebate is o.rebate )

      println("b1: " + b1.selectStatement)
      println("b2: " + b2.selectStatement)
      println("b3: " + b3.selectStatement)
      println("b4: " + b4.selectStatement)
      println("b5: " + b5.selectStatement)
      println("b6: " + b6.selectStatement)
      println("b7: " + b7.selectStatement)
      println("b8: " + b8.selectStatement)

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
