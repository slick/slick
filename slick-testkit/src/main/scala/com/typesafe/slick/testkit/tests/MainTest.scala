package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}

class MainTest extends TestkitTest[JdbcTestDB] { mainTest =>
  import tdb.profile.simple._

  case class User(id: Int, first: String, last: String)

  class Users(tag: Tag) extends Table[(Int, String, Option[String])](tag, "users") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def first = column[String]("first", O DBType "varchar(64)")
    def last = column[Option[String]]("last")
    def * = (id, first, last)
    def orders = mainTest.orders where { _.userID is id }
  }
  lazy val users = TableQuery[Users]

  class Orders(tag: Tag) extends Table[(Int, Int, String, Boolean, Option[Boolean])](tag, "orders") {
    def userID = column[Int]("userID")
    def orderID = column[Int]("orderID", O.PrimaryKey, O.AutoInc)
    def product = column[String]("product")
    def shipped = column[Boolean]("shipped")
    def rebate = column[Option[Boolean]]("rebate")
    def * = (userID, orderID, product, shipped, rebate)
  }
  lazy val orders = TableQuery[Orders]

  def test {

    val ddl = users.ddl ++ orders.ddl
    ddl.createStatements.foreach(println)
    ddl.create
    println(users.map(u => (u.first, u.last)).insertStatement)
    val ins1 = users.map(u => (u.first, u.last)).insert("Homer", Some("Simpson"))
    val ins2 = users.map(u => (u.first, u.last)).insertAll(
      ("Marge", Some("Simpson")), ("Apu", Some("Nahasapeemapetilon")), ("Carl", Some("Carlson")), ("Lenny", Some("Leonard")) )
    val ins3 = users.map(_.first).insertAll("Santa's Little Helper", "Snowball")
    val total = for(i2 <- ins2; i3 <- ins3) yield ins1 + i2 + i3
    println("Inserted "+total.getOrElse("<unknown>")+" users")
    /* All test DBs seem to report the actual number of rows.
     * None would also be an acceptable result here. */
    assertTrue("returned row count should be 7 or unknown", total == Some(7) || total == None)

    val q1 = for(u <- users) yield (u.id, u.first, u.last)
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

    val q1b = for(u <- users) yield (u.id, u.first.?, u.last,
      (Case If u.id < 3 Then "low" If u.id < 6 Then "medium" Else "high"))
    println("q1b: " + q1b.selectStatement)
    for(t <- q1b) println("With options and sequence: "+t)

    assertEquals(expectedUserTuples.map {
      case (id,f,l) => (id, Some(f), l, if(id < 3) "low" else if(id < 6) "medium" else "high")
    }, q1b.list)

    val q2 = for(u <- users if u.first is "Apu".bind) yield (u.last, u.id)
    println("q2: " + q2.selectStatement)
    println("Apu's last name and ID are: " + q2.first)
    assertEquals((Some("Nahasapeemapetilon"),3), q2.first)

    //TODO verifyable non-random test
    for(u <- allUsers
        if u.first != "Apu" && u.first != "Snowball"; i <- 1 to 2)
      orders.map(o => (o.userID, o.product, o.shipped, o.rebate)).insert(
        u.id, "Gizmo "+((scala.math.random*10)+1).toInt, i == 2, Some(u.first == "Marge"))

    val q3 = for (
      u <- users.sortBy(_.first) if u.last.isNotNull;
      o <- u.orders
    ) yield (u.first, u.last, o.orderID, o.product, o.shipped, o.rebate)
    println("q3: " + q3.selectStatement)
    println("All Orders by Users with a last name by first name:")
    q3.foreach(o => println("  "+o))

    val q4 = for {
      u <- users
      o <- u.orders
        if (o.orderID === (for { o2 <- orders where(o.userID is _.userID) } yield o2.orderID).max)
    } yield (u.first, o.orderID)
    println("q4: " + q4.selectStatement)
    println("Latest Order per User:")
    q4.foreach(o => println("  "+o))
    assertEquals(
      Set(("Homer",2), ("Marge",4), ("Carl",6), ("Lenny",8), ("Santa's Little Helper",10)),
      q4.list.toSet)

    def maxOfPer[T <: Table[_]](c: Query[T, _])(m: (T => Column[Int]), p: (T => Column[Int])) =
      c where { o => m(o) is (for { o2 <- c if p(o) is p(o2) } yield m(o2)).max }

    val q4b = for (
      u <- users;
      o <- maxOfPer(orders)(_.orderID, _.userID)
        if o.userID is u.id
    ) yield (u.first, o.orderID)
    println("q4b: " + q4b.selectStatement)
    println("Latest Order per User, using maxOfPer:")
    q4b.foreach(o => println("  "+o))
    assertEquals(
      Set(("Homer",2), ("Marge",4), ("Carl",6), ("Lenny",8), ("Santa's Little Helper",10)),
      q4b.list.toSet)

    /*
    val q4c = for (
      u <- Users;
      o <- Orders if o.userID is u.id;
      _ <- Query /*TODO groupBy u.id
                 having { _ => o.orderID.max > 5 }
                 orderBy o.orderID.max*/
    ) yield (u.first.min.get, o.orderID.max)
    println("q4c: " + q4c.selectStatement)
    println("Latest Order per User, using GroupBy, with orderID > 5:")
    q4c.foreach(o => println("  "+o))
    assertEquals(
      Set(("Carl",Some(6)), ("Lenny",Some(8)), ("Santa's Little Helper",Some(10))),
      q4c.list.toSet)
    */

    val q4d = for (
      u <- users if u.first inSetBind List("Homer", "Marge");
      o <- orders if o.userID is u.id
    ) yield (u.first, (LiteralColumn(1) + o.orderID, 1), o.product)
    println("q4d: " + q4d.selectStatement)
    println("Orders for Homer and Marge:")
    q4d.foreach(o => println("  "+o))

    val b1 = orders.where( o => o.shipped && o.shipped ).map( o => o.shipped && o.shipped )
    val b2 = orders.where( o => o.shipped && o.rebate ).map( o => o.shipped && o.rebate )
    val b3 = orders.where( o => o.rebate && o.shipped ).map( o => o.rebate && o.shipped )
    val b4 = orders.where( o => o.rebate && o.rebate ).map( o => o.rebate && o.rebate )
    val b5 = orders.where( o => !o.shipped ).map( o => !o.shipped )
    val b6 = orders.where( o => !o.rebate ).map( o => !o.rebate )
    val b7 = orders.map( o => o.shipped is o.shipped )
    val b8 = orders.map( o => o.rebate is o.shipped )
    val b9 = orders.map( o => o.shipped is o.rebate )
    val b10 = orders.map( o => o.rebate is o.rebate )

    println("b1: " + b1.selectStatement)
    println("b2: " + b2.selectStatement)
    println("b3: " + b3.selectStatement)
    println("b4: " + b4.selectStatement)
    println("b5: " + b5.selectStatement)
    println("b6: " + b6.selectStatement)
    println("b7: " + b7.selectStatement)
    println("b8: " + b8.selectStatement)

    val q5 = users where { _.id notIn orders.map(_.userID) }
    println("q5: " + q5.selectStatement)
    println("Users without Orders:")
    q5.foreach(o => println("  "+o))
    assertEquals(List((3,"Apu",Some("Nahasapeemapetilon")), (7,"Snowball",None)), q5.list)

    println("q5: " + q5.deleteStatement)
    println("Deleting them...")
    val deleted = q5.delete
    println("Deleted "+deleted+" rows")
    assertEquals(2, deleted)

    val q6 = Query(q5.length)
    println("q6: " + q6.selectStatement)
    println("Users without Orders left: " + q6.first)
    assertEquals(0, q6.first)

    val q7 = users.where(_.first is "Homer".bind).map(_.first)
    println("q7: " + q7.updateStatement)
    val updated1 = q7.update("Homer Jay")
    println("Updated "+updated1+" row(s)")
    assertEquals(1, updated1)

    val q8 = for(u <- users if u.last.isNull) yield (u.first, u.last)
    println("q8: " + q8.updateStatement)
    val updated2 = q8.update("n/a", Some("n/a"))
    println("Updated "+updated2+" row(s)")
    assertEquals(1, updated2)

    for(t <- q1) println("User tuple: "+t)
  }
}
