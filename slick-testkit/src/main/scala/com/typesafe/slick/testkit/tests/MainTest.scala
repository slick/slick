package com.typesafe.slick.testkit.tests

import scala.language.higherKinds
import com.typesafe.slick.testkit.util.{JdbcTestDB, AsyncTest}

class MainTest extends AsyncTest[JdbcTestDB] { mainTest =>
  import tdb.profile.api._

  case class User(id: Int, first: String, last: String)

  class Users(tag: Tag) extends Table[(Int, String, Option[String])](tag, "users") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def first = column[String]("first", O SqlType "varchar(64)")
    def last = column[Option[String]]("last")
    def * = (id, first, last)
    def orders = mainTest.orders filter { _.userID === id }
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

  def test = {
    val ddl = users.schema ++ orders.schema
    ddl.create.statements.toSeq.length.should(_ >= 2)
    users.map(u => (u.first, u.last)).insertStatement

    val q1 = (for(u <- users) yield (u.id, u.first, u.last)).sortBy(_._1)
    q1.result.statements.toSeq.length.should(_ >= 1)

    val q1b = for(u <- users) yield (u.id, u.first.?, u.last,
      (Case If u.id < 3 Then "low" If u.id < 6 Then "medium" Else "high"))
    q1b.result.statements.toSeq.length.should(_ >= 1)

    val q2 = for(u <- users if u.first === "Apu".bind) yield (u.last, u.id)
    q2.result.statements.toSeq.length.should(_ >= 1)

    val expectedUserTuples = List(
      (1,"Homer",Some("Simpson")),
      (2,"Marge",Some("Simpson")),
      (3,"Apu",Some("Nahasapeemapetilon")),
      (4,"Carl",Some("Carlson")),
      (5,"Lenny",Some("Leonard")),
      (6,"Santa's Little Helper",None),
      (7,"Snowball",None)
    )

    val p1 = db.stream(((for {
      _ <- ddl.create
      ins1 <- users.map(u => (u.first, u.last)) += ("Homer", Some("Simpson"))
      ins2 <- users.map(u => (u.first, u.last)) ++= Seq(
        ("Marge", Some("Simpson")), ("Apu", Some("Nahasapeemapetilon")), ("Carl", Some("Carlson")), ("Lenny", Some("Leonard")) )
      ins3 <- users.map(_.first) ++= Seq("Santa's Little Helper", "Snowball")
      total = for(i2 <- ins2; i3 <- ins3) yield ins1 + i2 + i3
      /* All test DBs seem to report the actual number of rows. None would also be acceptable: */
      _ = total.map(_ shouldBe 7)
      r1 <- q1.result
      _ = r1 shouldBe expectedUserTuples
    } yield ()) andThen q1.result).withPinnedSession)

    materialize(p1.mapResult { case (id,f,l) => User(id,f,l.orNull) }).flatMap { allUsers =>
      allUsers shouldBe expectedUserTuples.map{ case (id,f,l) => User(id,f,l.orNull) }
      db.run(for {
        r1b <- q1b.result
        _ = r1b shouldBe expectedUserTuples.map {
          case (id,f,l) => (id, Some(f), l, if(id < 3) "low" else if(id < 6) "medium" else "high")
        }
        _ <- q2.result.head.map(_ shouldBe (Some("Nahasapeemapetilon"),3))
      } yield allUsers)
    }.flatMap { allUsers =>
      //TODO verifyable non-random test
      val ordersInserts =
        for(u <- allUsers if u.first != "Apu" && u.first != "Snowball"; i <- 1 to 2)
          yield orders.map(o => (o.userID, o.product, o.shipped, o.rebate)) += (
            u.id, "Gizmo "+((scala.math.random*10)+1).toInt, i == 2, Some(u.first == "Marge"))
      db.run(seq(ordersInserts: _*))
    }.flatMap { _ =>
      val q3 = for (
        u <- users.sortBy(_.first) if u.last.isDefined;
        o <- u.orders
      ) yield (u.first, u.last, o.orderID, o.product, o.shipped, o.rebate)
      q3.result.statements.toSeq.length.should(_ >= 1)
      // All Orders by Users with a last name by first name:
      materialize(db.stream(q3.result)).map(s => s.length shouldBe 8)
    }.flatMap { _ =>
      val q4 = for {
        u <- users
        o <- u.orders
        if (o.orderID === (for { o2 <- orders filter(o.userID === _.userID) } yield o2.orderID).max)
      } yield (u.first, o.orderID)
      q4.result.statements.toSeq.length.should(_ >= 1)

      def maxOfPer[T <: Table[_], C[_]](c: Query[T, _, C])(m: (T => Rep[Int]), p: (T => Rep[Int])) =
        c filter { o => m(o) === (for { o2 <- c if p(o) === p(o2) } yield m(o2)).max }

      val q4b = for (
        u <- users;
        o <- maxOfPer(orders)(_.orderID, _.userID)
        if o.userID === u.id
      ) yield (u.first, o.orderID)
      q4b.result.statements.toSeq.length.should(_ >= 1)

      val q4d = for (
        u <- users if u.first inSetBind List("Homer", "Marge");
        o <- orders if o.userID === u.id
      ) yield (u.first, (LiteralColumn(1) + o.orderID, 1), o.product)
      q4d.result.statements.toSeq.length.should(_ >= 1)

      db.run(for {
        r4 <- q4.to[Set].result.named("Latest Order per User")
        _ = r4 shouldBe Set(("Homer",2), ("Marge",4), ("Carl",6), ("Lenny",8), ("Santa's Little Helper",10))
        r4b <- q4b.to[Set].result.named("Latest Order per User, using maxOfPer")
        _ = r4b shouldBe Set(("Homer",2), ("Marge",4), ("Carl",6), ("Lenny",8), ("Santa's Little Helper",10))
        _ <- q4d.result.map(r => r.length shouldBe 4)
      } yield ())
    }.flatMap { _ =>
      val b1 = orders.filter( o => o.shipped && o.shipped ).map( o => o.shipped && o.shipped )
      val b2 = orders.filter( o => o.shipped && o.rebate ).map( o => o.shipped && o.rebate )
      val b3 = orders.filter( o => o.rebate && o.shipped ).map( o => o.rebate && o.shipped )
      val b4 = orders.filter( o => o.rebate && o.rebate ).map( o => o.rebate && o.rebate )
      val b5 = orders.filter( o => !o.shipped ).map( o => !o.shipped )
      val b6 = orders.filter( o => !o.rebate ).map( o => !o.rebate )
      val b7 = orders.map( o => o.shipped === o.shipped )
      val b8 = orders.map( o => o.rebate === o.shipped )
      val b9 = orders.map( o => o.shipped === o.rebate )
      val b10 = orders.map( o => o.rebate === o.rebate )
      b1.result.statements.toSeq.length.should(_ >= 1)
      b2.result.statements.toSeq.length.should(_ >= 1)
      b3.result.statements.toSeq.length.should(_ >= 1)
      b4.result.statements.toSeq.length.should(_ >= 1)
      b5.result.statements.toSeq.length.should(_ >= 1)
      b6.result.statements.toSeq.length.should(_ >= 1)
      b7.result.statements.toSeq.length.should(_ >= 1)
      b8.result.statements.toSeq.length.should(_ >= 1)
      b9.result.statements.toSeq.length.should(_ >= 1)
      b10.result.statements.toSeq.length.should(_ >= 1)

      val q5 = users filterNot { _.id in orders.map(_.userID) }
      q5.result.statements.toSeq.length.should(_ >= 1)
      q5.delete.statements.toSeq.length.should(_ >= 1)
      val q6 = Query(q5.length)
      q6.result.statements.toSeq.length.should(_ >= 1)

      db.run(for {
        r5 <- q5.to[Set].result.named("Users without Orders")
        _ = r5 shouldBe Set((3,"Apu",Some("Nahasapeemapetilon")), (7,"Snowball",None))
        deleted <- q5.delete
        _ = deleted shouldBe 2
        _ <- q6.result.head.map(_ shouldBe 0)
      } yield ())
    }.flatMap { _ =>
      val q7 = Compiled { (s: Rep[String]) => users.filter(_.first === s).map(_.first) }
      q7("Homer").updateStatement
      val q7b = Compiled { users.filter(_.first === "Homer Jay").map(_.first) }
      q7b.updateStatement

      db.run(for {
        _ <- q7("Homer").update("Homer Jay").map(_ shouldBe 1)
        _ <- q7b.update("Homie").map(_ shouldBe 1)
        _ <- q7("Marge").map(_.length).result.map(_ shouldBe 1)
        _ <- q7("Marge").delete
        _ <- q7("Marge").map(_.length).result.map(_ shouldBe 0)
      } yield ())
    }.flatMap { _ =>
      val q8 = for(u <- users if u.last.isEmpty) yield (u.first, u.last)
      q8.updateStatement
      val q9 = users.length
      q9.result.statements.toSeq.length.should(_ >= 1)
      val q10 = users.filter(_.last inSetBind Seq()).map(u => (u.first, u.last))

      db.run(for {
        updated2 <- q8.update("n/a", Some("n/a"))
        _ = updated2 shouldBe 1
        _ <- q9.result.map(_ shouldBe 4)
        _ <- q10.result.map(_ shouldBe Nil)
      } yield ())
    }
  }
}
