package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{RelationalTestDB, TestkitTest}

class TemplateTest extends TestkitTest[RelationalTestDB] {
  import tdb.profile.simple._

  override val reuseInstance = true

  def testParameters {
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

    (users.ddl ++ orders.ddl).create

    users.map(_.first) ++= Seq("Homer", "Marge", "Apu", "Carl", "Lenny")
    for(uid <- users.map(_.id).run)
      orders.map(o => (o.userID, o.product)) += (uid, if(uid < 4) "Product A" else "Product B")

    def userNameByID1(id: Int) = for(u <- users if u.id is id.bind) yield u.first
    def q1 = userNameByID1(3)
    assertEquals(List("Apu"), q1.run)

    val userNameByID2 = for {
      id <- Parameters[Int]
      u <- users if u.id is id
    } yield u.first
    val q2 = userNameByID2(3)
    assertEquals(List("Apu"), q2.run)

    val userNameByIDRange = for {
      (min, max) <- Parameters[(Int, Int)]
      u <- users if u.id >= min && u.id <= max
    } yield u.first
    val q3 = userNameByIDRange(2,5)
    assertEquals(List("Marge","Apu","Carl","Lenny"), q3.run)

    val userNameByIDRangeAndProduct = for {
      (min, (max, product)) <- Parameters[(Int, (Int, String))]
      u <- users if u.id >= min && u.id <= max && orders.filter(o => (u.id is o.userID) && (o.product is product)).exists
    } yield u.first
    val q4 = userNameByIDRangeAndProduct(2,(5,"Product A"))
    assertEquals(List("Marge","Apu"), q4.run)

    def userNameByIDOrAll(id: Option[Int]) = for(
      u <- users if id.map(u.id is _.bind).getOrElse(LiteralColumn(true))
    ) yield u.first
    val q5a = userNameByIDOrAll(Some(3))
    assertEquals(List("Apu"), q5a.run)
    val q5b = userNameByIDOrAll(None)
    assertEquals(List("Homer","Marge","Apu","Carl","Lenny"), q5b.run)
  }

  def testCompiled {
    class T(tag: Tag) extends Table[(Int, String)](tag, "t_lifted") {
      def id = column[Int]("id", O.PrimaryKey)
      def s = column[String]("s")
      def * = (id, s)
    }
    def ts = TableQuery[T]
    ts.ddl.create
    ts ++= Seq((1, "a"), (2, "b"), (3, "c"))

    val byIdAndS = { (id: Column[Int], s: ConstColumn[String]) => ts.filter(t => t.id === id && t.s === s) }
    val byIdAndSC = Compiled(byIdAndS)
    val byIdAndFixedSC = byIdAndSC.map(f => f((_: Column[Int]), "b"))
    val byIdC = Compiled { id: Column[Int] => ts.filter(_.id === id) }
    val byId = byIdC.extract
    val byIdC3 = byIdC(3)
    val byId3 = byIdC3.extract
    val countBelow = { (id: Column[Int]) => ts.filter(_.id < id).length }
    val countBelowC = Compiled(countBelow)

    val r0 = byIdAndS(1, "a").run
    assertEquals(Set((1, "a")), r0.toSet)

    val r1 = byIdAndSC(1, "a").run
    val r1t: Seq[(Int, String)] = r1
    assertEquals(Set((1, "a")), r1.toSet)

    val r2 = byIdAndFixedSC(2).run
    assertEquals(Set((2, "b")), r2.toSet)

    val r3a = byIdC3.run
    val r3b = byId3.run
    assertEquals(Seq((3, "c")), r3a)
    assertEquals(Seq((3, "c")), r3b)

    val r4 = countBelow(3).run
    val r4t: Int = r4
    assertEquals(2, r4)
    val r5 = countBelowC(3).run
    val r5t: Int = r5
    assertEquals(2, r5)
  }
}
