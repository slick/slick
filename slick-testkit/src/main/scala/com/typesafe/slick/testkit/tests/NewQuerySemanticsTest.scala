package com.typesafe.slick.testkit.tests

import scala.language.higherKinds
import org.junit.Assert._
import com.typesafe.slick.testkit.util.{RelationalTestDB, TestkitTest}

class NewQuerySemanticsTest extends TestkitTest[RelationalTestDB] {
  import tdb.profile.simple._

  val doRun = true

  override val reuseInstance = true

  def testNewComposition {

    class SuppliersStd(tag: Tag) extends Table[(Int, String, String, String, String, String)](tag, "SUPPLIERS") {
      def id = column[Int]("SUP_ID", O.PrimaryKey) // This is the primary key column
      def name = column[String]("SUP_NAME")
      def street = column[String]("STREET")
      def city = column[String]("CITY")
      def state = column[String]("STATE")
      def zip = column[String]("ZIP")
      def * = (id, name, street, city, state, zip)
    }
    val suppliersStd = TableQuery[SuppliersStd]

    class CoffeesStd(tag: Tag) extends Table[(String, Int, Int, Int, Int)](tag, "COFFEES") {
      def name = column[String]("COF_NAME", O.PrimaryKey)
      def supID = column[Int]("SUP_ID")
      def price = column[Int]("PRICE")
      def sales = column[Int]("SALES")
      def total = column[Int]("TOTAL")
      def * = (name, supID, price, sales, total)
      def supplier = foreignKey("SUP_FK", supID, suppliersStd)(_.id)
    }
    val coffeesStd = TableQuery[CoffeesStd]

    class Suppliers(tag: Tag) extends Table[(Int, String, String)](tag, "SUPPLIERS") {
      def id = column[Int]("SUP_ID", O.PrimaryKey) // This is the primary key column
      def name = column[String]("SUP_NAME")
      def street = column[String]("STREET")
      def city = column[String]("CITY")
      def state = column[String]("STATE")
      def zip = column[String]("ZIP")
      def * = (id, name, street)
    }
    val suppliers = TableQuery[Suppliers]

    class Coffees(tag: Tag) extends Table[(String, Int, Int, Int, Int)](tag, "COFFEES") {
      def name = column[String]("COF_NAME", O.PrimaryKey)
      def supID = column[Int]("SUP_ID")
      def price = column[Int]("PRICE")
      def sales = column[Int]("SALES")
      def total = column[Int]("TOTAL")
      def * = (name, supID, price, sales, (total * 10))
      def totalComputed = sales * price
      def supplier = foreignKey("SUP_FK", supID, suppliers)(_.id)
    }
    val coffees = TableQuery[Coffees]

    (suppliersStd.ddl ++ coffeesStd.ddl).create

    suppliersStd += (101, "Acme, Inc.",      "99 Market Street", "Groundsville", "CA", "95199")
    suppliersStd += ( 49, "Superior Coffee", "1 Party Place",    "Mendocino",    "CA", "95460")
    suppliersStd += (150, "The High Ground", "100 Coffee Lane",  "Meadows",      "CA", "93966")

    coffeesStd ++= Seq(
      ("Colombian",         101, 799, 1, 0),
      ("French_Roast",       49, 799, 2, 0),
      ("Espresso",          150, 999, 3, 0),
      ("Colombian_Decaf",   101, 849, 4, 0),
      ("French_Roast_Decaf", 49, 999, 5, 0)
    )

    def show[C[_]](name: String, g: Query[_,_, C]) =
      println("=========================================== "+name)

    val qa = for {
      c <- coffees.take(3)
    } yield (c.supID, (c.name, 42))
    show("qa", qa)
    if(doRun) {
      val ra = qa.run.toSet
      println("ra: "+ra)
      assertEquals(3, ra.size)
      // No sorting, so result contents can vary
      assertAllMatch(ra){ case (s: Int, (i: String, 42)) => () }
    }

    val qb = qa.take(2).map(_._2)
    show("qb", qb)
    if(doRun) {
      val rb = qb.run.toSet
      println("rb: "+rb)
      assertEquals(2, rb.size)
      // No sorting, so result contents can vary
      assertAllMatch(rb){ case (i: String, 42) => () }
    }

    val qb2 = qa.map(n => n).take(2).map(_._2)
    show("qb2", qb2)
    if(doRun) {
      val rb2 = qb2.run.toSet
      println("rb2: "+rb2)
      assertEquals(2, rb2.size)
      // No sorting, so result contents can vary
      assertAllMatch(rb2){ case (i: String, 42) => () }
    }

    val qc = qa.map(_._2).take(2)
    show("qc", qc)
    if(doRun) {
      val rc = qc.run.toSet
      println("rc: "+rc)
      assertEquals(2, rc.size)
      // No sorting, so result contents can vary
      assertAllMatch(rc){ case (i: String, 42) => () }
    }

    val q0 = coffees
    show("q0: Plain table", q0)
    if(doRun) {
      val r0 = q0.run.toSet
      println("r0: "+r0)
      val r0e = Set(
        ("Colombian",         101, 799, 1, 0),
        ("French_Roast",       49, 799, 2, 0),
        ("Espresso",          150, 999, 3, 0),
        ("Colombian_Decaf",   101, 849, 4, 0),
        ("French_Roast_Decaf", 49, 999, 5, 0)
      )
      assertEquals(r0e, r0)
    }

    val q1 = for {
      c <- coffees.sortBy(c => (c.name, c.price.desc)).take(2)
      s <- suppliers
    } yield ((c.name, (s.city ++ ":")), c, s, c.totalComputed)
    show("q1: Plain implicit join", q1)
    if(doRun) {
      val r1 = q1.run.toSet
      println("r1: "+r1)
      val r1e = Set(
        (("Colombian","Groundsville:"),("Colombian",101,799,1,0),(101,"Acme, Inc.","99 Market Street"),799),
        (("Colombian","Mendocino:"),("Colombian",101,799,1,0),(49,"Superior Coffee","1 Party Place"),799),
        (("Colombian","Meadows:"),("Colombian",101,799,1,0),(150,"The High Ground","100 Coffee Lane"),799),
        (("Colombian_Decaf","Groundsville:"),("Colombian_Decaf",101,849,4,0),(101,"Acme, Inc.","99 Market Street"),3396),
        (("Colombian_Decaf","Mendocino:"),("Colombian_Decaf",101,849,4,0),(49,"Superior Coffee","1 Party Place"),3396),
        (("Colombian_Decaf","Meadows:"),("Colombian_Decaf",101,849,4,0),(150,"The High Ground","100 Coffee Lane"),3396)
      )
      assertEquals(r1e, r1)
    }

    ifCap(rcap.pagingNested) {
      val q1b_0 = coffees.sortBy(_.price).take(3) join suppliers on (_.supID === _.id)
      val q1b = for {
        (c, s) <- q1b_0.sortBy(_._1.price).take(2).filter(_._1.name =!= "Colombian")
        (c2, s2) <- q1b_0
      } yield (c.name, s.city, c2.name)
      show("q1b: Explicit join with condition", q1b)
      if(doRun) {
        val r1b = q1b.run.toSet
        println("r1b: "+r1b)
        val r1be = Set(
          ("French_Roast","Mendocino","Colombian"),
          ("French_Roast","Mendocino","French_Roast"),
          ("French_Roast","Mendocino","Colombian_Decaf")
        )
        assertEquals(r1be, r1b)
      }
    }

    val q2 = for {
      c <- coffees.filter(_.price < 900).map(_.*)
      s <- suppliers if s.id === c._2
    } yield (c._1, s.name)
    show("q2: More elaborate query", q2)
    if(doRun) {
      val r2 = q2.run.toSet
      println("r2: "+r2)
      val r2e = Set(
        ("Colombian","Acme, Inc."),
        ("French_Roast","Superior Coffee"),
        ("Colombian_Decaf","Acme, Inc.")
      )
      assertEquals(r2e, r2)
    }

    val q3 = coffees.flatMap { c =>
      val cf = Query(c).filter(_.price === 849)
      cf.flatMap { cf =>
        suppliers.filter(_.id === c.supID).map { s =>
          (c.name, s.name, cf.name, cf.total, cf.totalComputed)
        }
      }
    }
    show("q3: Lifting scalar values", q3)
    if(doRun) {
      val r3 = q3.run.toSet
      println("r3: "+r3)
      val r3e = Set(("Colombian_Decaf","Acme, Inc.","Colombian_Decaf",0,3396))
      assertEquals(r3e, r3)
    }

    val q3b = coffees.flatMap { c =>
      val cf = Query((c, 42)).filter(_._1.price < 900)
      cf.flatMap { case (cf, num) =>
        suppliers.filter(_.id === c.supID).map { s =>
          (c.name, s.name, cf.name, cf.total, cf.totalComputed, num)
        }
      }
    }
    show("q3b: Lifting scalar values, with extra tuple", q3b)
    if(doRun) {
      val r3b = q3b.run.toSet
      println("r3b: "+r3b)
      val r3be = Set(
        ("Colombian","Acme, Inc.","Colombian",0,799,42),
        ("French_Roast","Superior Coffee","French_Roast",0,1598,42),
        ("Colombian_Decaf","Acme, Inc.","Colombian_Decaf",0,3396,42)
      )
      assertEquals(r3be, r3b)
    }

    ifCap(rcap.pagingNested) {
      val q4 = for {
        c <- coffees.map(c => (c.name, c.price, 42)).sortBy(_._1).take(2).filter(_._2 < 800)
      } yield (c._1, c._3)
      show("q4: Map to tuple, then filter", q4)
      if(doRun) {
        val r4 = q4.run.toSet
        println("r4: "+r4)
        val r4e = Set(("Colombian",42))
        assertEquals(r4e, r4)
      }
    }

    val q4b_0 = coffees.map(c => (c.name, c.price, 42)).filter(_._2 < 800)
    val q4b = for {
      c <- q4b_0
      d <- q4b_0
    } yield (c,d)
    show("q4b: Map to tuple, then filter, with self-join", q4b)
    if(doRun) {
      val r4b = q4b.run.toSet
      println("r4b: "+r4b)
      val r4be = Set(
        (("Colombian",799,42),("Colombian",799,42)),
        (("Colombian",799,42),("French_Roast",799,42)),
        (("French_Roast",799,42),("Colombian",799,42)),
        (("French_Roast",799,42),("French_Roast",799,42))
      )
      assertEquals(r4be, r4b)
    }

    val q5_0 = coffees.sortBy(_.price).take(2)
    val q5 = for {
      c1 <- q5_0
      c2 <- q5_0
    } yield (c1, c2)
    show("q5: Implicit self-join", q5)
    if(doRun) {
      val r5 = q5.run.toSet
      println("r5: "+r5)
      val r5e = Set(
        (("Colombian",101,799,1,0),("Colombian",101,799,1,0)),
        (("Colombian",101,799,1,0),("French_Roast",49,799,2,0)),
        (("French_Roast",49,799,2,0),("Colombian",101,799,1,0)),
        (("French_Roast",49,799,2,0),("French_Roast",49,799,2,0))
      )
      assertEquals(r5e, r5)
    }

    val q5b = for {
      t <- q5_0 join q5_0 on (_.name === _.name)
    } yield (t._1, t._2)
    show("q5b: Explicit self-join with condition", q5b)
    if(doRun) {
      val r5b = q5b.run.toSet
      println("r5b: "+r5b)
      val r5be = Set(
        (("Colombian",101,799,1,0),("Colombian",101,799,1,0)),
        (("French_Roast",49,799,2,0),("French_Roast",49,799,2,0))
      )
      assertEquals(r5be, r5b)
    }

    val q6 = coffees.flatMap(c => suppliers)
    show("q6: Unused outer query result, unbound TableQuery", q6)
    if(doRun) {
      val r6 = q6.run.toSet
      println("r6: "+r6)
      val r6e = Set(
        (101,"Acme, Inc.","99 Market Street"),
        (49,"Superior Coffee","1 Party Place"),
        (150,"The High Ground","100 Coffee Lane")
      )
      assertEquals(r6e, r6)
    }

    val q7a = for {
      c <- coffees.filter(_.price < 800) union coffees.filter(_.price > 950)
    } yield (c.name, c.supID, c.total)
    show("q7a: Simple union", q7a)
    if(doRun) {
      val r7a = q7a.run.toSet
      println("r7a: "+r7a)
      val r7ae = Set(
        ("Colombian",101,0),
        ("French_Roast",49,0),
        ("Espresso",150,0),
        ("French_Roast_Decaf",49,0)
      )
      assertEquals(r7ae, r7a)
    }

    val q7 = for {
      c <- coffees.filter(_.price < 800).map((_, 1)) union coffees.filter(_.price > 950).map((_, 2))
    } yield (c._1.name, c._1.supID, c._2)
    show("q7: Union", q7)
    if(doRun) {
      val r7 = q7.run.toSet
      println("r7: "+r7)
      val r7e = Set(
        ("Colombian",101,1),
        ("French_Roast",49,1),
        ("Espresso",150,2),
        ("French_Roast_Decaf",49,2)
      )
      assertEquals(r7e, r7)
    }

    val q71 = for {
      c <- coffees.filter(_.price < 800).map((_, 1))
    } yield (c._1.name, c._1.supID, c._2)
    show("q71: Transitive push-down without union", q71)
    if(doRun) {
      val r71 = q71.run.toSet
      println("r71: "+r71)
      val r71e = Set(
        ("Colombian",101,1),
        ("French_Roast",49,1)
      )
      assertEquals(r71e, r71)
    }

    val q7b = q7 filter (_._1 =!= "Colombian")
    show("q7b: Union with filter on the outside", q7b)
    if(doRun) {
      val r7b = q7b.run.toSet
      println("r7b: "+r7b)
      val r7be = Set(
        ("French_Roast",49,1),
        ("Espresso",150,2),
        ("French_Roast_Decaf",49,2)
      )
      assertEquals(r7be, r7b)
    }

    val q8 = for {
      (c1, c2) <- coffees.filter(_.price < 900) leftJoin coffees.filter(_.price < 800) on (_.name === _.name)
    } yield (c1.name, c2.name.?)
    show("q8: Outer join", q8)
    if(doRun) {
      val r8 = q8.run.toSet
      println("r8: "+r8)
      val r8e = Set(
        ("Colombian",Some("Colombian")),
        ("French_Roast",Some("French_Roast")),
        ("Colombian_Decaf",None)
      )
      assertEquals(r8e, r8)
    }

    val q8b = for {
      t <- coffees.sortBy(_.sales).take(1) leftJoin coffees.sortBy(_.sales).take(2) on (_.name === _.name) leftJoin coffees.sortBy(_.sales).take(4) on (_._1.supID === _.supID)
    } yield (t._1, t._2)
    show("q8b: Nested outer joins", q8b)
    if(doRun) {
      val r8b = q8b.run.toSet
      println("r8b: "+r8b)
      val r8be = Set(
        ((("Colombian",101,799,1,0),("Colombian",101,799,1,0)),("Colombian",101,799,1,0)),
        ((("Colombian",101,799,1,0),("Colombian",101,799,1,0)),("Colombian_Decaf",101,849,4,0))
      )
      assertEquals(r8be, r8b)
    }

    (suppliersStd.ddl ++ coffeesStd.ddl).drop
  }

  def testOldComposition {
    import TupleMethods._

    class Users(tag: Tag) extends Table[(Int, String, String)](tag, "users") {
      def id = column[Int]("id")
      def first = column[String]("first")
      def last = column[String]("last")
      def * = id ~ first ~ last
    }
    val users = TableQuery[Users]

    class Orders(tag: Tag) extends Table[(Int, Int)](tag, "orders") {
      def userID = column[Int]("userID")
      def orderID = column[Int]("orderID")
      def * = userID ~ orderID
    }
    val orders = TableQuery[Orders]

    val q2 = for {
      u <- users.sortBy(u => (u.first, u.last.desc))
      o <- orders filter { o => u.id === o.userID }
    } yield u.first ~ u.last ~ o.orderID

    (users.ddl ++ orders.ddl).create

    val q3 = for (u <- users filter (_.id === 42)) yield u.first ~ u.last
    q3.run

    val q4 = (for {
      (u, o) <- users innerJoin orders on (_.id === _.userID)
    } yield (u.last, u.first ~ o.orderID)).sortBy(_._1).map(_._2)
    q4.run

    val q6a =
      (for (o <- orders if o.orderID === (for {o2 <- orders if o.userID === o2.userID} yield o2.orderID).max) yield o.orderID).sorted
    q6a.run

    val q6b =
      (for (o <- orders if o.orderID === (for {o2 <- orders if o.userID === o2.userID} yield o2.orderID).max) yield o.orderID ~ o.userID).sortBy(_._1)
    q6b.run

    val q6c =
      (for (o <- orders if o.orderID === (for {o2 <- orders if o.userID === o2.userID} yield o2.orderID).max) yield o).sortBy(_.orderID).map(o => o.orderID ~ o.userID)
    q6c.run

    (users.ddl ++ orders.ddl).drop
  }

  def testAdvancedFusion {
    class TableA(tag: Tag) extends Table[Int](tag, "TableA") {
      def id = column[Int]("id")
      def * = id
    }
    val tableA = TableQuery[TableA]

    class TableB(tag: Tag) extends Table[(Int, Int)](tag, "TableB") {
      def id = column[Int]("id")
      def start = column[Int]("start")
      def * = (id, start)
    }
    val tableB = TableQuery[TableB]

    class TableC(tag: Tag) extends Table[Int](tag, "TableC") {
      def start = column[Int]("start")
      def * = start
    }
    val tableC = TableQuery[TableC]

    val queryErr2 = for {
      a <- tableA
      b <- tableB if b.id === a.id
      start = a.id + 1
      c <- tableC if c.start <= start
    } yield (b, c)

    (tableA.ddl ++ tableB.ddl ++ tableC.ddl).create
    queryErr2.run
  }

  def testSubquery {
    class A(tag: Tag) extends Table[Int](tag, "A_subquery") {
      def id = column[Int]("id")
      def * = id
    }
    val as = TableQuery[A]
    as.ddl.create
    as += 42

    val q0 = as.filter(_.id === 42.bind).length
    val r0 = q0.run
    assertEquals(1, r0)

    val q1 = Compiled { (n: Rep[Int]) =>
      as.filter(_.id === n).map(a => as.length)
    }
    val r1 = q1(42).run
    assertEquals(List(1), r1)

    val q2 = as.filter(_.id in as.sortBy(_.id).map(_.id))
    assertEquals(Vector(42), q2.run)
  }

  def testExpansion {
    class A(tag: Tag) extends Table[(Int, String)](tag, "A_refexp") {
      def id = column[Int]("id")
      def a = column[String]("a")
      def b = column[String]("b")
      def * = (id, a)
      override def create_* = collectFieldSymbols((id, a, b).shaped.toNode)
    }
    val as = TableQuery[A]
    as.ddl.create
    as.map(a => (a.id, a.a, a.b)) ++= Seq(
      (1, "a1", "b1"),
      (2, "a2", "b2"),
      (3, "a3", "b3")
    )

    val q1 = as.map(identity).filter(_.b === "b3")
    val r1 = q1.run
    assertEquals(Set((3, "a3")), r1.toSet)

    val q2a = as.sortBy(_.a) join as on (_.b === _.b)
    val q2 = for {
      (c, s) <- q2a
      c2 <- as
    } yield (c.id, c2.a)
    val r2 = q2.run.toSet
    val r2e = Set((1, "a1"), (1, "a2"), (1, "a3"), (2, "a1"), (2, "a2"), (2, "a3"), (3, "a1"), (3, "a2"), (3, "a3"))
    assertEquals(r2e, r2)
  }
}
