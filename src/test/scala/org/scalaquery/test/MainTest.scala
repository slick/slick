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

object MainTest extends DBTestObject(H2Mem, Postgres, MySQL, DerbyMem, HsqldbMem)

class MainTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit._

  case class User(id: Int, first: String, last: String)

  object Users extends Table[(Int, String, Option[String])]("users") {
    def id = column[Int]("id", O PrimaryKey, O AutoInc)
    def first = column[String]("first", O Default "NFN", O DBType "varchar(64)")
    def last = column[Option[String]]("last")
    def * = id ~ first ~ last

    def orders = Orders where { _.userID is id }
  }

  object Orders extends Table[(Int, Int, String, Boolean, Option[Boolean])]("orders") {
    def userID = column[Int]("userID")
    def orderID = column[Int]("orderID", O PrimaryKey, O AutoInc)
    def product = column[String]("product")
    def shipped = column[Boolean]("shipped", O Default false)
    def rebate = column[Option[Boolean]]("rebate", O Default Some(false))
    def * = userID ~ orderID ~ product ~ shipped ~ rebate
  }

  @Test def test() {
    db withSession {

      val ddl = Users.ddl ++ Orders.ddl
      ddl.createStatements.foreach(println)
      ddl.create
      println((Users.first ~ Users.last).insertStatement)
      val ins1 = (Users.first ~ Users.last).insert("Homer", Some("Simpson"))
      val ins2 = (Users.first ~ Users.last).insertAll(
        ("Marge", Some("Simpson")), ("Apu", Some("Nahasapeemapetilon")), ("Carl", Some("Carlson")), ("Lenny", Some("Leonard")) )
      val ins3 = Users.first.insertAll("Santa's Little Helper", "Snowball")
      val total = for(i2 <- ins2; i3 <- ins3) yield ins1 + i2 + i3
      println("Inserted "+total.getOrElse("<unknown>")+" users")
      /* All test DBs seem to report the actual number of rows.
       * None would also be an acceptable result here. */
      assertEquals(Some(7), total)

      val q1 = for(u <- Users) yield u.id ~ u.first ~ u.last
      println("q1: " + q1.selectStatement)
      for(t <- q1) println("User tuple: "+t)
      val allUsers = q1.mapResult{ case (id,f,l) => User(id,f,l.orNull) }.list
      for(u <- allUsers) println("User object: "+u)

      val expectedUserTuples = List(
        (1,"Homer",Some("Simpson")),
        (2,"Marge",Some("Simpson")),
        (3,"Apu",Some("Nahasapeemapetilon")),
        (4,"Carl",Some("Carlson")),
        (5,"Lenny",Some("Leonard")),
        (6,"Santa's Little Helper",None),
        (7,"Snowball",None) )
      assertEquals(expectedUserTuples, q1.list)
      assertEquals(expectedUserTuples.map{ case (id,f,l) => User(id,f,l.orNull) }, allUsers)

      val q1b = for(u <- Users) yield u.id ~ u.first.? ~ u.last ~
        (Case when u.id < 3 then "low" when u.id < 6 then "medium" otherwise "high")
      println("q1b: " + q1b.selectStatement)
      for(t <- q1b) println("With options and sequence: "+t)

      assertEquals(expectedUserTuples.map {
        case (id,f,l) => (id, Some(f), l, if(id < 3) "low" else if(id < 6) "medium" else "high")
      }, q1b.list)

      val q2 = for(u <- Users if u.first is "Apu".bind) yield u.last ~ u.id
      println("q2: " + q2.selectStatement)
      println("Apu's last name and ID are: " + q2.first)
      assertEquals((Some("Nahasapeemapetilon"),3), q2.first)

      //TODO verifyable non-random test
      for(u <- allUsers
          if u.first != "Apu" && u.first != "Snowball"; i <- 1 to 2)
        (Orders.userID ~ Orders.product ~ Orders.shipped ~ Orders.rebate).insert(
          u.id, "Gizmo "+((scala.math.random*10)+1).toInt, i == 2, Some(u.first == "Marge"))

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
      assertEquals(
        Set(("Homer",2), ("Marge",4), ("Carl",6), ("Lenny",8), ("Santa's Little Helper",10)),
        q4.list.toSet)

      def maxOfPer[T <: TableBase[_]]
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
      assertEquals(
        Set(("Homer",2), ("Marge",4), ("Carl",6), ("Lenny",8), ("Santa's Little Helper",10)),
        q4b.list.toSet)

      val q4c = for (
        u <- Users;
        o <- Orders if o.userID is u.id;
        _ <- Query groupBy u.id
                   having { _ => o.orderID.max > 5 }
                   orderBy o.orderID.max
      ) yield u.first.min.get ~ o.orderID.max
      println("q4c: " + q4c.selectStatement)
      println("Latest Order per User, using GroupBy, with orderID > 5:")
      q4c.foreach(o => println("  "+o))
      assertEquals(
        Set(("Carl",Some(6)), ("Lenny",Some(8)), ("Santa's Little Helper",Some(10))),
        q4c.list.toSet)

      val q4d = for (
        u <- Users if u.first inSetBind List("Homer", "Marge");
        o <- Orders if o.userID is u.id
      ) yield u.first ~ (ConstColumn(1) + o.orderID - 1) ~ o.product
      println("q4d: " + q4d.selectStatement)
      println("Orders for Homer and Marge:")
      q4d.foreach(o => println("  "+o))

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
      assertEquals(List((3,"Apu",Some("Nahasapeemapetilon")), (7,"Snowball",None)), q5.list)

      println("q5: " + q5.deleteStatement)
      println("Deleting them...")
      val deleted = q5.delete
      println("Deleted "+deleted+" rows")
      assertEquals(2, deleted)

      val q6 = Query(q5.count)
      println("q6: " + q6.selectStatement)
      println("Users without Orders left: " + q6.first)
      assertEquals(0, q6.first)

      val q7 = Users.where(_.first is "Homer".bind).map(_.first)
      println("q7: " + q7.updateStatement)
      val updated1 = q7.update("Homer Jay")
      println("Updated "+updated1+" row(s)")
      assertEquals(1, updated1)

      val q8 = for(u <- Users if u.last.isNull) yield u.first ~ u.last
      println("q8: " + q8.updateStatement)
      val updated2 = q8.update("n/a", Some("n/a"))
      println("Updated "+updated2+" row(s)")
      assertEquals(1, updated2)

      for(t <- q1) println("User tuple: "+t)
    }
  }
}
