package scala.slick.test.lifted

import org.junit.Test
import org.junit.Assert._
import scala.slick.lifted._
import scala.slick.session.Database.threadLocalSession
import scala.slick.jdbc.StaticQuery._
import scala.slick.ast._
import scala.slick.testutil._
import scala.slick.testutil.TestDB._

object NewQuerySemanticsTest extends DBTestObject(H2Mem)

class NewQuerySemanticsTest(val tdb: TestDB) extends DBTest {
  import tdb.profile.Table
  import tdb.profile.Implicit._

  val run = true

  @Test def test(): Unit = db withSession {

    object SuppliersStd extends Table[(Int, String, String, String, String, String)]("SUPPLIERS") {
      def id = column[Int]("SUP_ID", O.PrimaryKey) // This is the primary key column
      def name = column[String]("SUP_NAME")
      def street = column[String]("STREET")
      def city = column[String]("CITY")
      def state = column[String]("STATE")
      def zip = column[String]("ZIP")
      def * = id ~ name ~ street ~ city ~ state ~ zip
    }

    object CoffeesStd extends Table[(String, Int, Double, Int, Int)]("COFFEES") {
      def name = column[String]("COF_NAME", O.PrimaryKey)
      def supID = column[Int]("SUP_ID")
      def price = column[Double]("PRICE")
      def sales = column[Int]("SALES")
      def total = column[Int]("TOTAL")
      def * = name ~ supID ~ price ~ sales ~ total
      def supplier = foreignKey("SUP_FK", supID, SuppliersStd)(_.id)
    }

    object Suppliers extends Table[(Int, String, String)]("SUPPLIERS") {
      def id = column[Int]("SUP_ID", O.PrimaryKey) // This is the primary key column
      def name = column[String]("SUP_NAME")
      def street = column[String]("STREET")
      def city = column[String]("CITY")
      def state = column[String]("STATE")
      def zip = column[String]("ZIP")
      def * = id ~ name ~ street
      def forInsert = id ~ name ~ street ~ city ~ state ~ zip
    }

    object Coffees extends Table[(String, Int, Double, Int, Int)]("COFFEES") {
      def name = column[String]("COF_NAME", O.PrimaryKey)
      def supID = column[Int]("SUP_ID")
      def price = column[Double]("PRICE")
      def sales = column[Int]("SALES")
      def total = column[Int]("TOTAL")
      def * = name ~ supID ~ price ~ sales ~ (total * 10)
      def forInsert = name ~ supID ~ price ~ sales ~ total
      def totalComputed = sales.asColumnOf[Double] * price
      def supplier = foreignKey("SUP_FK", supID, Suppliers)(_.id)
    }

    (SuppliersStd.ddl ++ CoffeesStd.ddl).create

    SuppliersStd.insert(101, "Acme, Inc.",      "99 Market Street", "Groundsville", "CA", "95199")
    SuppliersStd.insert( 49, "Superior Coffee", "1 Party Place",    "Mendocino",    "CA", "95460")
    SuppliersStd.insert(150, "The High Ground", "100 Coffee Lane",  "Meadows",      "CA", "93966")

    CoffeesStd.insertAll(
      ("Colombian",         101, 7.99, 1, 0),
      ("French_Roast",       49, 7.99, 2, 0),
      ("Espresso",          150, 9.99, 3, 0),
      ("Colombian_Decaf",   101, 8.49, 4, 0),
      ("French_Roast_Decaf", 49, 9.99, 5, 0)
    )

    val l1 = queryNA[String]("select c.cof_name from coffees c").list
    println("l1: "+l1)
    assertEquals(5, l1.length)
    val l2 = queryNA[String]("select c.cof_name from coffees c, suppliers s").list
    println("l2: "+l2)
    assertEquals(15, l2.length)

    def show(name: String, g: Query[_,_]) {
      val n = Node(g)
      println("=========================================== "+name)
      //n.dump("source: ")
      //println
      //val n2 = Optimizer(n)
      //n2.dump("optimized: ")
      //println
      //val n3 = Relational(n2)
      //n3.dump("relational: ")
      //println
      //println("SQL: "+g.selectStatement)
    }

    val qa = for {
      c <- Coffees.take(3)
    } yield (c.name, (c.supID, 42))
    show("qa", qa)
    if(run) {
      val ra = qa.to[Set]
      println("ra: "+ra)
      assertEquals(3, ra.size)
      // No sorting, so result contents can vary
      assertAllMatch(ra){ case (s: String, (i: Int, 42)) => () }
    }

    val qb = qa.take(2).map(_._2)
    show("qb", qb)
    if(run) {
      val rb = qb.to[Set]
      println("rb: "+rb)
      assertEquals(2, rb.size)
      // No sorting, so result contents can vary
      assertAllMatch(rb){ case (i: Int, 42) => () }
    }

    val qb2 = qa.map(n => n).take(2).map(_._2)
    show("qb2", qb2)
    if(run) {
      val rb2 = qb2.to[Set]
      println("rb2: "+rb2)
      assertEquals(2, rb2.size)
      // No sorting, so result contents can vary
      assertAllMatch(rb2){ case (i: Int, 42) => () }
    }

    val qc = qa.map(_._2).take(2)
    show("qc", qc)
    if(run) {
      val rc = qc.to[Set]
      println("rc: "+rc)
      assertEquals(2, rc.size)
      // No sorting, so result contents can vary
      assertAllMatch(rc){ case (i: Int, 42) => () }
    }

    val q0 = Query(Coffees)
    show("q0: Plain table", q0)
    if(run) {
      val r0 = q0.to[Set]
      println("r0: "+r0)
      val r0e = Set(
        ("Colombian",         101, 7.99, 1, 0),
        ("French_Roast",       49, 7.99, 2, 0),
        ("Espresso",          150, 9.99, 3, 0),
        ("Colombian_Decaf",   101, 8.49, 4, 0),
        ("French_Roast_Decaf", 49, 9.99, 5, 0)
      )
      assertEquals(r0e, r0)
    }

    val q1 = for {
      c <- Coffees.sortBy(c => (c.name, c.price.desc)).take(2)
      s <- Suppliers
    } yield (c.name ~ (s.city ++ ":"), c, s, c.totalComputed)
    show("q1: Plain implicit join", q1)
    if(run) {
      val r1 = q1.to[Set]
      println("r1: "+r1)
      val r1e = Set(
        (("Colombian","Groundsville:"),("Colombian",101,7.99,1,0),(101,"Acme, Inc.","99 Market Street"),7.99),
        (("Colombian","Mendocino:"),("Colombian",101,7.99,1,0),(49,"Superior Coffee","1 Party Place"),7.99),
        (("Colombian","Meadows:"),("Colombian",101,7.99,1,0),(150,"The High Ground","100 Coffee Lane"),7.99),
        (("Colombian_Decaf","Groundsville:"),("Colombian_Decaf",101,8.49,4,0),(101,"Acme, Inc.","99 Market Street"),33.96),
        (("Colombian_Decaf","Mendocino:"),("Colombian_Decaf",101,8.49,4,0),(49,"Superior Coffee","1 Party Place"),33.96),
        (("Colombian_Decaf","Meadows:"),("Colombian_Decaf",101,8.49,4,0),(150,"The High Ground","100 Coffee Lane"),33.96)
      )
      assertEquals(r1e, r1)
    }

    val q1b_0 = Coffees.sortBy(_.price).take(3) join Suppliers on (_.supID === _.id)
    val q1b = for {
      (c, s) <- q1b_0.take(2).filter(_._1.name =!= "Colombian")
      (c2, s2) <- q1b_0
    } yield c.name ~ s.city ~ c2.name
    show("q1b: Explicit join with condition", q1b)
    if(run) {
      val r1b = q1b.to[Set]
      println("r1b: "+r1b)
      val r1be = Set(
        ("French_Roast","Mendocino","Colombian"),
        ("French_Roast","Mendocino","French_Roast"),
        ("French_Roast","Mendocino","Colombian_Decaf")
      )
      assertEquals(r1be, r1b)
    }

    val q2 = for {
      c <- Coffees.filter(_.price < 9.0).map(_.*)
      s <- Suppliers if s.id === c._2
    } yield c._1 ~ s.name
    show("q2: More elaborate query", q2)
    if(run) {
      val r2 = q2.to[Set]
      println("r2: "+r2)
      val r2e = Set(
        ("Colombian","Acme, Inc."),
        ("French_Roast","Superior Coffee"),
        ("Colombian_Decaf","Acme, Inc.")
      )
      assertEquals(r2e, r2)
    }

    val q3 = Coffees.flatMap { c =>
      val cf = Query(c).where(_.price === 8.49)
      cf.flatMap { cf =>
        Suppliers.where(_.id === c.supID).map { s =>
          c.name ~ s.name ~ cf.name ~ cf.total ~ cf.totalComputed
        }
      }
    }
    show("q3: Lifting scalar values", q3)
    if(run) {
      val r3 = q3.to[Set]
      println("r3: "+r3)
      val r3e = Set(("Colombian_Decaf","Acme, Inc.","Colombian_Decaf",0,33.96))
      assertEquals(r3e, r3)
    }

    val q3b = Coffees.flatMap { c =>
      val cf = Query((c, 42)).where(_._1.price < 9.0)
      cf.flatMap { case (cf, num) =>
        Suppliers.where(_.id === c.supID).map { s =>
          c.name ~ s.name ~ cf.name ~ cf.total ~ cf.totalComputed ~ num
        }
      }
    }
    show("q3b: Lifting scalar values, with extra tuple", q3b)
    if(run) {
      val r3b = q3b.to[Set]
      println("r3b: "+r3b)
      val r3be = Set(
        ("Colombian","Acme, Inc.","Colombian",0,7.99,42),
        ("French_Roast","Superior Coffee","French_Roast",0,15.98,42),
        ("Colombian_Decaf","Acme, Inc.","Colombian_Decaf",0,33.96,42)
      )
      assertEquals(r3be, r3b)
    }

    val q4 = for {
      c <- Coffees.map(c => (c.name, c.price, 42)).sortBy(_._1).take(2).filter(_._2 < 8.0)
    } yield c._1 ~ c._3
    show("q4: Map to tuple, then filter", q4)
    if(run) {
      val r4 = q4.to[Set]
      println("r4: "+r4)
      val r4e = Set(("Colombian",42))
      assertEquals(r4e, r4)
    }

    val q4b_0 = Coffees.map(c => (c.name, c.price, 42)).filter(_._2 < 8.0)
    val q4b = for {
      c <- q4b_0
      d <- q4b_0
    } yield (c,d)
    show("q4b: Map to tuple, then filter, with self-join", q4b)
    if(run) {
      val r4b = q4b.to[Set]
      println("r4b: "+r4b)
      val r4be = Set(
        (("Colombian",7.99,42),("Colombian",7.99,42)),
        (("Colombian",7.99,42),("French_Roast",7.99,42)),
        (("French_Roast",7.99,42),("Colombian",7.99,42)),
        (("French_Roast",7.99,42),("French_Roast",7.99,42))
      )
      assertEquals(r4be, r4b)
    }

    val q5_0 = Query(Coffees).sortBy(_.price).take(2)
    val q5 = for {
      c1 <- q5_0
      c2 <- q5_0
    } yield (c1, c2)
    show("q5: Implicit self-join", q5)
    if(run) {
      val r5 = q5.to[Set]
      println("r5: "+r5)
      val r5e = Set(
        (("Colombian",101,7.99,1,0),("Colombian",101,7.99,1,0)),
        (("Colombian",101,7.99,1,0),("French_Roast",49,7.99,2,0)),
        (("French_Roast",49,7.99,2,0),("Colombian",101,7.99,1,0)),
        (("French_Roast",49,7.99,2,0),("French_Roast",49,7.99,2,0))
      )
      assertEquals(r5e, r5)
    }

    val q5b = for {
      t <- q5_0 join q5_0 on (_.name === _.name)
    } yield (t._1, t._2)
    show("q5b: Explicit self-join with condition", q5b)
    if(run) {
      val r5b = q5b.to[Set]
      println("r5b: "+r5b)
      val r5be = Set(
        (("Colombian",101,7.99,1,0),("Colombian",101,7.99,1,0)),
        (("French_Roast",49,7.99,2,0),("French_Roast",49,7.99,2,0))
      )
      assertEquals(r5be, r5b)
    }

    val q6 = Coffees.flatMap(c => Query(Suppliers))
    show("q6: Unused outer query result, unbound TableQuery", q6)
    if(run) {
      val r6 = q6.to[Set]
      println("r6: "+r6)
      val r6e = Set(
        (101,"Acme, Inc.","99 Market Street"),
        (49,"Superior Coffee","1 Party Place"),
        (150,"The High Ground","100 Coffee Lane")
      )
      assertEquals(r6e, r6)
    }

    val q7a = for {
      c <- Coffees.filter(_.price < 8.0) union Coffees.filter(_.price > 9.5)
    } yield c.name ~ c.supID ~ c.total
    show("q7a: Simple union", q7a)
    if(run) {
      val r7a = q7a.to[Set]
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
      c <- Coffees.filter(_.price < 8.0).map((_, 1)) union Coffees.filter(_.price > 9.5).map((_, 2))
    } yield c._1.name ~ c._1.supID ~ c._2
    show("q7: Union", q7)
    if(run) {
      val r7 = q7.to[Set]
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
      c <- Coffees.filter(_.price < 8.0).map((_, 1))
    } yield c._1.name ~ c._1.supID ~ c._2
    show("q71: Transitive push-down without union", q71)
    if(run) {
      val r71 = q71.to[Set]
      println("r71: "+r71)
      val r71e = Set(
        ("Colombian",101,1),
        ("French_Roast",49,1)
      )
      assertEquals(r71e, r71)
    }

    val q7b = q7 where (_._1 =!= "Colombian")
    show("q7b: Union with filter on the outside", q7b)
    if(run) {
      val r7b = q7b.to[Set]
      println("r7b: "+r7b)
      val r7be = Set(
        ("French_Roast",49,1),
        ("Espresso",150,2),
        ("French_Roast_Decaf",49,2)
      )
      assertEquals(r7be, r7b)
    }

    val q8 = for {
      (c1, c2) <- Coffees.where(_.price < 9.0) leftJoin Coffees.where(_.price < 8.0) on (_.name === _.name)
    } yield (c1.name, c2.name.?)
    show("q8: Outer join", q8)
    if(run) {
      val r8 = q8.to[Set]
      println("r8: "+r8)
      val r8e = Set(
        ("Colombian",Some("Colombian")),
        ("French_Roast",Some("French_Roast")),
        ("Colombian_Decaf",None)
      )
      assertEquals(r8e, r8)
    }

    val q8b = for {
      t <- Coffees.sortBy(_.sales).take(1) leftJoin Coffees.sortBy(_.sales).take(2) on (_.name === _.name) leftJoin Coffees.sortBy(_.sales).take(4) on (_._1.supID === _.supID)
    } yield (t._1, t._2)
    show("q8b: Nested outer joins", q8b)
    if(run) {
      val r8b = q8b.to[Set]
      println("r8b: "+r8b)
      val r8be = Set(
        ((("Colombian",101,7.99,1,0),("Colombian",101,7.99,1,0)),("Colombian",101,7.99,1,0)),
        ((("Colombian",101,7.99,1,0),("Colombian",101,7.99,1,0)),("Colombian_Decaf",101,8.49,4,0))
      )
      assertEquals(r8be, r8b)
    }

    (SuppliersStd.ddl ++ CoffeesStd.ddl).drop
    (SuppliersStd.ddl ++ CoffeesStd.ddl).dropStatements.foreach(s => println("drop: "+s))
  }
}
