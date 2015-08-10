package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{RelationalTestDB, AsyncTest}

class TemplateTest extends AsyncTest[RelationalTestDB] {
  import tdb.profile.api._

  def testParameters = {
    class Users(tag: Tag) extends Table[(Int, String)](tag, "users") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def first = column[String]("first")
      def * = (id, first)
    }
    lazy val users = TableQuery[Users]

    class Orders(tag: Tag) extends Table[(Int, Int, String)](tag, "orders") {
      def userID = column[Int]("userID")
      def orderID = column[Int]("orderID", O.PrimaryKey, O.AutoInc)
      def product = column[String]("product")
      def * = (userID, orderID, product)
    }
    lazy val orders = TableQuery[Orders]

    def userNameByID1(id: Int) = for(u <- users if u.id === id.bind) yield u.first
    def q1 = userNameByID1(3)

    val userNameByID2 = for {
      id <- Parameters[Int]
      u <- users if u.id === id
    } yield u.first
    val q2 = userNameByID2(3)

    val userNameByIDRange = for {
      (min, max) <- Parameters[(Int, Int)]
      u <- users if u.id >= min && u.id <= max
    } yield u.first
    val q3 = userNameByIDRange(2,5)

    val userNameByIDRangeAndProduct = for {
      (min, (max, product)) <- Parameters[(Int, (Int, String))]
      u <- users if u.id >= min && u.id <= max && orders.filter(o => (u.id === o.userID) && (o.product === product)).exists
    } yield u.first
    val q4 = userNameByIDRangeAndProduct(2,(5,"Product A"))

    def userNameByIDOrAll(id: Option[Int]) = for(
      u <- users if id.map(u.id === _.bind).getOrElse(LiteralColumn(true))
    ) yield u.first
    val q5a = userNameByIDOrAll(Some(3))
    val q5b = userNameByIDOrAll(None)

    for {
      _ <- (users.schema ++ orders.schema).create
      _ <- users.map(_.first) ++= Seq("Homer", "Marge", "Apu", "Carl", "Lenny")
      uids <- users.map(_.id).result
      _ <- DBIO.seq(uids.map(uid => orders.map(o => (o.userID, o.product)) += (uid, if(uid < 4) "Product A" else "Product B")): _*)
      _ <- q1.result.map(_ shouldBe List("Apu"))
      _ <- q2.result.map(_ shouldBe List("Apu"))
      _ <- q3.result.map(_.toSet shouldBe Set("Marge","Apu","Carl","Lenny"))
      _ <- q4.result.map(_.toSet shouldBe Set("Marge", "Apu"))
      _ <- q5a.result.map(_ shouldBe List("Apu"))
      _ <- q5b.result.map(_.toSet shouldBe Set("Homer","Marge","Apu","Carl","Lenny"))
    } yield ()
  }

  def testCompiled = {
    class T(tag: Tag) extends Table[(Int, String)](tag, "t_lifted") {
      def id = column[Int]("id", O.PrimaryKey)
      def s = column[String]("s")
      def * = (id, s)
    }
    def ts = TableQuery[T]

    val byIdAndS = { (id: Rep[Int], s: ConstColumn[String]) => ts.filter(t => t.id === id && t.s === s) }
    val byIdAndSC = Compiled(byIdAndS)
    val byIdAndFixedSC = byIdAndSC.map(f => f((_: Rep[Int]), "b"))
    val byIdC = Compiled { id: Rep[Int] => ts.filter(_.id === id) }
    val byId = byIdC.extract
    val byIdC3 = byIdC(3)
    val byId3 = byIdC3.extract
    val countBelow = { (id: Rep[Int]) => ts.filter(_.id < id).length }
    val countBelowC = Compiled(countBelow)
    val joinC = Compiled { id: Rep[Int] => ts.filter(_.id === id).join(ts.filter(_.id === id)) }

    implicitly[slick.lifted.Executable[(Rep[Int], Rep[Int]), _]]
    implicitly[slick.lifted.Compilable[(Rep[Int], Rep[Int]), _]]
    val impShaped = (ts.length, ts.length)
    val impShapedC = Compiled(impShaped)
    implicitly[slick.lifted.Executable[slick.lifted.ShapedValue[(Rep[Int], Rep[Int]), (Int, Int)], _]]
    implicitly[slick.lifted.Compilable[slick.lifted.ShapedValue[(Rep[Int], Rep[Int]), (Int, Int)], _]]
    val expShaped = impShaped.shaped
    val expShapedC = Compiled(expShaped)

    seq(
      ts.schema.create,
      Compiled(ts.map(identity)) += (1, "a"),
      Compiled(ts.map(identity)).result.map(_ shouldBe Seq((1, "a"))),
      Compiled(ts) ++= Seq((2, "b"), (3, "c")),
      byIdAndS(1, "a").result.map(r => r.toSet shouldBe Set((1, "a"))),
      byIdAndSC(1, "a").result.map((r: Seq[(Int, String)]) => r.toSet shouldBe Set((1, "a"))),
      byIdAndFixedSC(2).result.map(r => r.toSet shouldBe Set((2, "b"))),
      byIdC3.result.map(r => r.toSet shouldBe Set((3, "c"))),
      byId3.result.map(r => r.toSet shouldBe Set((3, "c"))),
      countBelow(3).result.map((r: Int) => r shouldBe 2),
      countBelowC(3).result.map((r: Int) => r shouldBe 2),
      joinC(1).result.map(_ shouldBe Seq(((1, "a"), (1, "a")))),
      impShapedC.result.map((r: (Int, Int)) => r shouldBe (3, 3)),
      expShapedC.result.map((r: (Int, Int)) => r shouldBe (3, 3))
    )
  }
}
