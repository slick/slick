package com.typesafe.slick.testkit.tests

import scala.language.higherKinds
import com.typesafe.slick.testkit.util.{RelationalTestDB, AsyncTest}

class NewQuerySemanticsTest extends AsyncTest[RelationalTestDB] {
  import tdb.profile.api._

  def testNewComposition = {
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
      def name = column[String]("COF_NAME", O.PrimaryKey, O.Length(254))
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

    val setup = seq(
      (suppliersStd.schema ++ coffeesStd.schema).create,
      suppliersStd += (101, "Acme, Inc.",      "99 Market Street", "Groundsville", "CA", "95199"),
      suppliersStd += ( 49, "Superior Coffee", "1 Party Place",    "Mendocino",    "CA", "95460"),
      suppliersStd += (150, "The High Ground", "100 Coffee Lane",  "Meadows",      "CA", "93966"),
      coffeesStd ++= Seq(
        ("Colombian",         101, 799, 1, 0),
        ("French_Roast",       49, 799, 2, 0),
        ("Espresso",          150, 999, 3, 0),
        ("Colombian_Decaf",   101, 849, 4, 0),
        ("French_Roast_Decaf", 49, 999, 5, 0)
      )
    ).named("setup")

    val qa = for {
      c <- coffees.take(3)
    } yield (c.supID, (c.name, 42))
    val qb = qa.take(2).map(_._2)
    val qb2 = qa.map(n => n).take(2).map(_._2)
    val qc = qa.map(_._2).take(2)

    val a1 = seq(
      qa.result.map(_.toSet).map { ra =>
        ra.size shouldBe 3
        // No sorting, so result contents can vary
        ra shouldAllMatch { case (s: Int, (i: String, 42)) => () }
      },
      qb.result.map(_.toSet).map { rb =>
        rb.size shouldBe 2
        // No sorting, so result contents can vary
        rb shouldAllMatch { case (i: String, 42) => () }
      },
      qb2.result.map(_.toSet).map { rb2 =>
        rb2.size shouldBe 2
        // No sorting, so result contents can vary
        rb2 shouldAllMatch { case (i: String, 42) => () }
      },
      qc.result.map(_.toSet).map { rc =>
        rc.size shouldBe 2
        // No sorting, so result contents can vary
        rc shouldAllMatch { case (i: String, 42) => () }
      }
    )

    // Plain table
    val q0 = coffees

    // Plain implicit join
    val q1 = for {
      c <- coffees.sortBy(c => (c.name, c.price.desc)).take(2)
      s <- suppliers
    } yield ((c.name, (s.city ++ ":")), c, s, c.totalComputed)

    // Explicit join with condition
    val q1b_0 = coffees.sortBy(_.price).take(3) join suppliers on (_.supID === _.id)
    def q1b = for {
      (c, s) <- q1b_0.sortBy(_._1.price).take(2).filter(_._1.name =!= "Colombian")
      (c2, s2) <- q1b_0
    } yield (c.name, s.city, c2.name)

    def a2 = seq(
      q0.result.named("Plain table").map(_.toSet).map { r0 =>
        r0 shouldBe Set(
          ("Colombian",         101, 799, 1, 0),
          ("French_Roast",       49, 799, 2, 0),
          ("Espresso",          150, 999, 3, 0),
          ("Colombian_Decaf",   101, 849, 4, 0),
          ("French_Roast_Decaf", 49, 999, 5, 0)
        )
      },
      q1.result.named("Plain implicit join").map(_.toSet).map { r1 =>
       r1 shouldBe Set(
          (("Colombian","Groundsville:"),("Colombian",101,799,1,0),(101,"Acme, Inc.","99 Market Street"),799),
          (("Colombian","Mendocino:"),("Colombian",101,799,1,0),(49,"Superior Coffee","1 Party Place"),799),
          (("Colombian","Meadows:"),("Colombian",101,799,1,0),(150,"The High Ground","100 Coffee Lane"),799),
          (("Colombian_Decaf","Groundsville:"),("Colombian_Decaf",101,849,4,0),(101,"Acme, Inc.","99 Market Street"),3396),
          (("Colombian_Decaf","Mendocino:"),("Colombian_Decaf",101,849,4,0),(49,"Superior Coffee","1 Party Place"),3396),
          (("Colombian_Decaf","Meadows:"),("Colombian_Decaf",101,849,4,0),(150,"The High Ground","100 Coffee Lane"),3396)
        )
      },
      ifCap(rcap.pagingNested) {
        q1b.result.named("Explicit join with condition").map { r1b =>
          r1b.toSet shouldBe Set(
            ("French_Roast","Mendocino","Colombian"),
            ("French_Roast","Mendocino","French_Roast"),
            ("French_Roast","Mendocino","Colombian_Decaf")
          )
        }
      }
    )

    // More elaborate query
    val q2 = for {
      c <- coffees.filter(_.price < 900).map(_.*)
      s <- suppliers if s.id === c._2
    } yield (c._1, s.name)

    // Lifting scalar values
    val q3 = coffees.flatMap { c =>
      val cf = Query(c).filter(_.price === 849)
      cf.flatMap { cf =>
        suppliers.filter(_.id === c.supID).map { s =>
          (c.name, s.name, cf.name, cf.total, cf.totalComputed)
        }
      }
    }

    // Lifting scalar values, with extra tuple
    val q3b = coffees.flatMap { c =>
      val cf = Query((c, 42)).filter(_._1.price < 900)
      cf.flatMap { case (cf, num) =>
        suppliers.filter(_.id === c.supID).map { s =>
          (c.name, s.name, cf.name, cf.total, cf.totalComputed, num)
        }
      }
    }

    // Map to tuple, then filter
    def q4 = for {
      c <- coffees.map(c => (c.name, c.price, 42)).sortBy(_._1).take(2).filter(_._2 < 800)
    } yield (c._1, c._3)

    // Map to tuple, then filter, with self-join
    val q4b_0 = coffees.map(c => (c.name, c.price, 42)).filter(_._2 < 800)
    val q4b = for {
      c <- q4b_0
      d <- q4b_0
    } yield (c,d)

    def a3 = seq(
      q2.result.named("More elaborate query").map(_.toSet).map { r2 =>
        r2 shouldBe Set(
          ("Colombian","Acme, Inc."),
          ("French_Roast","Superior Coffee"),
          ("Colombian_Decaf","Acme, Inc.")
        )
      },
      q3.result.named("Lifting scalar values").map(_.toSet).map { r3 =>
        r3 shouldBe Set(("Colombian_Decaf","Acme, Inc.","Colombian_Decaf",0,3396))
      },
      q3b.result.named("Lifting scalar values, with extra tuple").map(_.toSet).map { r3b =>
        r3b shouldBe Set(
          ("Colombian","Acme, Inc.","Colombian",0,799,42),
          ("French_Roast","Superior Coffee","French_Roast",0,1598,42),
          ("Colombian_Decaf","Acme, Inc.","Colombian_Decaf",0,3396,42)
        )
      },
      ifCap(rcap.pagingNested) {
        q4.result.named("Map to tuple, then filter").map { r4 =>
          r4.toSet shouldBe Set(("Colombian",42))
        }
      },
      q4b.result.map(_.toSet).map { r4b =>
        r4b shouldBe Set(
          (("Colombian",799,42),("Colombian",799,42)),
          (("Colombian",799,42),("French_Roast",799,42)),
          (("French_Roast",799,42),("Colombian",799,42)),
          (("French_Roast",799,42),("French_Roast",799,42))
        )
      }
    )

    // Implicit self-join
    val q5_0 = coffees.sortBy(_.price).take(2)
    val q5 = for {
      c1 <- q5_0
      c2 <- q5_0
    } yield (c1, c2)

    // Explicit self-join with condition
    val q5b = for {
      t <- q5_0 join q5_0 on (_.name === _.name)
    } yield (t._1, t._2)

    // Unused outer query result, unbound TableQuery
    val q6 = coffees.flatMap(c => suppliers)

    def a4 = seq(
      q5.result.map(_.toSet).map { r5 =>
        r5 shouldBe Set(
          (("Colombian",101,799,1,0),("Colombian",101,799,1,0)),
          (("Colombian",101,799,1,0),("French_Roast",49,799,2,0)),
          (("French_Roast",49,799,2,0),("Colombian",101,799,1,0)),
          (("French_Roast",49,799,2,0),("French_Roast",49,799,2,0))
        )
      },
      q5b.result.named("Explicit self-join with condition").map(_.toSet).map { r5b =>
        r5b shouldBe Set(
          (("Colombian",101,799,1,0),("Colombian",101,799,1,0)),
          (("French_Roast",49,799,2,0),("French_Roast",49,799,2,0))
        )
      },
      q6.result.named("Unused outer query result, unbound TableQuery").map(_.toSet).map { r6 =>
        r6 shouldBe Set(
          (101,"Acme, Inc.","99 Market Street"),
          (49,"Superior Coffee","1 Party Place"),
          (150,"The High Ground","100 Coffee Lane")
        )
      }
    )

    // Simple union
    val q7a = for {
      c <- coffees.filter(_.price < 800) union coffees.filter(_.price > 950)
    } yield (c.name, c.supID, c.total)

    // Union
    val q7 = for {
      c <- coffees.filter(_.price < 800).map((_, 1)) union coffees.filter(_.price > 950).map((_, 2))
    } yield (c._1.name, c._1.supID, c._2)

    // Transitive push-down without union
    val q71 = for {
      c <- coffees.filter(_.price < 800).map((_, 1))
    } yield (c._1.name, c._1.supID, c._2)

    def a5 = seq(
      q7a.result.named("Simple union").map(_.toSet).map { r7a =>
        r7a shouldBe Set(
          ("Colombian",101,0),
          ("French_Roast",49,0),
          ("Espresso",150,0),
          ("French_Roast_Decaf",49,0)
        )
      },
      q7.result.named("Union").map(_.toSet).map { r7 =>
        r7 shouldBe Set(
          ("Colombian",101,1),
          ("French_Roast",49,1),
          ("Espresso",150,2),
          ("French_Roast_Decaf",49,2)
        )
      },
      q71.result.named("Transitive push-down without union").map(_.toSet).map { r71 =>
        r71 shouldBe Set(
          ("Colombian",101,1),
          ("French_Roast",49,1)
        )
      }
    )

    // Union with filter on the outside
    val q7b = q7 filter (_._1 =!= "Colombian")

    // Outer join
    val q8 = for {
      (c1, c2) <- coffees.filter(_.price < 900) joinLeft coffees.filter(_.price < 800) on (_.name === _.name)
    } yield (c1.name, c2.map(_.name))

    // Nested outer join
    val q8b = for {
      t <- coffees.sortBy(_.sales).take(1) joinLeft coffees.sortBy(_.sales).take(2) on (_.name === _.name) joinLeft coffees.sortBy(_.sales).take(4) on (_._1.supID === _.supID)
    } yield (t._1, t._2)

    def a6 = seq(
      q7b.result.named("Union with filter on the outside").map(_.toSet).map { r7b =>
        r7b shouldBe Set(
          ("French_Roast",49,1),
          ("Espresso",150,2),
          ("French_Roast_Decaf",49,2)
        )
      },
      q8.result.named("Outer join").map(_.toSet).map { r8 =>
        r8 shouldBe Set(
          ("Colombian",Some("Colombian")),
          ("French_Roast",Some("French_Roast")),
          ("Colombian_Decaf",None)
        )
      },
      q8b.result.named("Nested outer join").map(_.toSet).map { r8b =>
        r8b shouldBe Set(
          ((("Colombian",101,799,1,0),Some(("Colombian",101,799,1,0))),Some(("Colombian",101,799,1,0))),
          ((("Colombian",101,799,1,0),Some(("Colombian",101,799,1,0))),Some(("Colombian_Decaf",101,849,4,0)))
        )
      }
    )

    seq(setup, a1, a2, a3, a4, a5, a6)
  }

  def testOldComposition = {
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

    val q3 = for (u <- users filter (_.id === 42)) yield u.first ~ u.last

    val q4 = (for {
      (u, o) <- users join orders on (_.id === _.userID)
    } yield (u.last, u.first ~ o.orderID)).sortBy(_._1).map(_._2)

    val q6a =
      (for (o <- orders if o.orderID === (for {o2 <- orders if o.userID === o2.userID} yield o2.orderID).max) yield o.orderID).sorted

    val q6b =
      (for (o <- orders if o.orderID === (for {o2 <- orders if o.userID === o2.userID} yield o2.orderID).max) yield o.orderID ~ o.userID).sortBy(_._1)

    val q6c =
      (for (o <- orders if o.orderID === (for {o2 <- orders if o.userID === o2.userID} yield o2.orderID).max) yield o).sortBy(_.orderID).map(o => o.orderID ~ o.userID)

    seq(
      (users.schema ++ orders.schema).create,
      q3.result,
      q4.result,
      q6a.result,
      q6b.result,
      q6c.result,
      (users.schema ++ orders.schema).drop
    )
  }

  def testAdvancedFusion = {
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

    (tableA.schema ++ tableB.schema ++ tableC.schema).create >> queryErr2.result
  }

  def testSubquery = {
    class A(tag: Tag) extends Table[Int](tag, "A_subquery") {
      def id = column[Int]("id")
      def * = id
    }
    val as = TableQuery[A]

    for {
      _ <- as.schema.create
      _ <- as += 42

      q0 = as.filter(_.id === 42.bind).length
      _ <- q0.result.named("q0").map(_ shouldBe 1)

      q1 = Compiled { (n: Rep[Int]) =>
        as.filter(_.id === n).map(a => as.length)
      }
      _ <- q1(42).result.named("q1(42)").map(_ shouldBe List(1))

      q2 = as.filter(_.id in as.sortBy(_.id).map(_.id))
      _ <- q2.result.named("q2").map(_ shouldBe Vector(42))
    } yield ()
  }

  def testExpansion = {
    class A(tag: Tag) extends Table[(Int, String)](tag, "A_refexp") {
      def id = column[Int]("id")
      def a = column[String]("a")
      def b = column[String]("b")
      def * = (id, a)
      override def create_* = collectFieldSymbols((id, a, b).shaped.toNode)
    }
    val as = TableQuery[A]

    for {
      _ <- as.schema.create
      _ <- as.map(a => (a.id, a.a, a.b)) ++= Seq(
        (1, "a1", "b1"),
        (2, "a2", "b2"),
        (3, "a3", "b3")
      )

      q1 = as.map(identity).filter(_.b === "b3")
      _ <- q1.result.named("q1").map(r1 => r1.toSet shouldBe Set((3, "a3")))

      q2a = as.sortBy(_.a) join as on (_.b === _.b)
      q2 = for {
        (c, s) <- q2a
        c2 <- as
      } yield (c.id, c2.a)
      r2 <- q2.result.named("q2").map(_.toSet)
      _ = r2 shouldBe Set((1, "a1"), (1, "a2"), (1, "a3"), (2, "a1"), (2, "a2"), (2, "a3"), (3, "a1"), (3, "a2"), (3, "a3"))
    } yield ()
  }
}
