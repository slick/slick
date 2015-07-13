package slick.test.jdbc

import org.junit.Test
import org.junit.Assert._
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import slick.backend.{DatabaseConfig, StaticDatabaseConfig}
import slick.collection.heterogeneous.HNil
import slick.collection.heterogeneous.syntax._
import slick.driver.JdbcProfile

@StaticDatabaseConfig("file:common-test-resources/application.conf#tsql")
class TypedStaticQueryTest {

  @Test
  def testTypedInterpolation: Unit = {
    val dc = DatabaseConfig.forAnnotation[JdbcProfile]
    import dc.driver.api._
    try {
      val id1 = 150
      val id2 = 1
      val s1 = tsql"select * from SUPPLIERS where SUP_ID = ${id1}"
      val s2 = tsql"select * from COFFEES where SUP_ID = ${id2}"
      assertEquals("select * from SUPPLIERS where SUP_ID = ?", s1.statements.head)
      assertEquals("select * from COFFEES where SUP_ID = ?", s2.statements.head)

      val (total1, sales1) = (5, 4)
      val s3 = tsql"select COF_NAME from COFFEES where SALES = ${sales1} and TOTAL = ${total1}"

      val s4 = tsql"select 1, '2', 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23"

      Await.result(dc.db.run(DBIO.seq(
        s1.map { list1 =>
          val typedList1: Vector[(Int, String, String, String, String, String)] = list1
          assertEquals(Vector((150, "The High Ground", "100 Coffee Lane", "Meadows", "CA", "93966")), typedList1)
        },
        s2.map { list2 =>
          val typedList2: Vector[(String, Int, Double, Int, Int)] = list2
          assertEquals(Vector(("coffee", 1, 2.3, 4, 5)), typedList2)
        },
        s3.map { list3 =>
          val cof1: String = list3.head
          assertEquals("coffee", cof1)
        },
        s4.map { list4 =>
          val hlist1Typed: Int :: String :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: HNil = list4.head
          assertEquals(1 :: "2" :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: 11 :: 12 :: 13 :: 14 :: 15 :: 16 :: 17 :: 18 :: 19 :: 20 :: 21 :: 22 :: 23 :: HNil, hlist1Typed)
        }
      )), Duration.Inf)
    } finally dc.db.close()
  }

  @Test
  def testCustomTypes: Unit = {
    val dc = DatabaseConfig.forAnnotation[JdbcProfile]
    import dc.driver.api._
    try {
      import slick.jdbc.SetParameter

      case class Foo(intVal: Int)
      case class Bar(strVal: String)

      implicit val SetFoo = SetParameter[Foo] { (i, pp) =>
        SetParameter.SetInt(i.intVal, pp)
      }
      implicit val SetBar = SetParameter[Bar] { (s, pp) =>
        SetParameter.SetString(s.strVal, pp)
      }

      val foo = new Foo(150)
      val bar = new Bar("Something")
      val num = 15

      val s1 = tsql"select * from SUPPLIERS where SUP_ID = ${foo}"
      val s2 = tsql"select * from SUPPLIERS where SUP_ID = ${num * 10}"
      val s3 = tsql"select SUP_ID from SUPPLIERS"
      val s4 = tsql"select CITY from SUPPLIERS"

      Await.result(dc.db.run(DBIO.seq(
        s1.map { o1 =>
          val t1: Vector[(Int, String, String, String, String, String)] = o1
          assertEquals(Vector((150, "The High Ground", "100 Coffee Lane", "Meadows", "CA", "93966")), t1)
        },
        s2.map { o2 =>
          val t2: Vector[(Int, String, String, String, String, String)] = o2
          assertEquals(Vector((150, "The High Ground", "100 Coffee Lane", "Meadows", "CA", "93966")), t2)
        },
        s3.map { o3 =>
          val t3: Vector[Foo] = o3.map(Foo(_))
          assertEquals(Set(Foo(101), Foo(150), Foo(49)), t3.toSet)
        },
        s4.map { o4 =>
          val t4: Vector[Bar] = o4.map(Bar(_))
          assertEquals(Set(Bar("Groundsville"), Bar("Meadows"), Bar("Mendocino")), t4.toSet)
        }
      )), Duration.Inf)
    } finally dc.db.close()
  }

  @Test
  def testPreparedQueries: Unit = {
    val dc = DatabaseConfig.forAnnotation[JdbcProfile]
    import dc.driver.api._
    try {
      case class Supplier(id: Int, name: String)
      implicit val supplierGetter = (arg: (Int, String)) => Supplier(arg._1, arg._2)

      def supplierForID(id: Int) =
        tsql"select SUP_ID, SUP_NAME from SUPPLIERS where SUP_ID = $id"
      def supplierForIdAndName(id: Int, name: String) =
        tsql"select SUP_ID, SUP_NAME from SUPPLIERS where SUP_ID = $id and SUP_NAME = $name"

      val s1 = supplierForID(101)
      val s2 = supplierForID(49)
      val s3 = supplierForIdAndName(150, "The High Ground")
      val s4 = supplierForIdAndName(49, "Superior Coffee")

      Await.result(dc.db.run(DBIO.seq(
        s1.map { o1 =>
          val t1: Supplier = o1.map(supplierGetter).head
          assertEquals(Supplier(101, "Acme, Inc."), t1)
        },
        s2.map { o2 =>
          val t2: Supplier = o2.map(supplierGetter).head
          assertEquals(Supplier(49, "Superior Coffee"), t2)
        },
        s3.map { o3 =>
          val t3: Supplier = o3.map(supplierGetter).head
          assertEquals(Supplier(150, "The High Ground"), t3)
        },
        s4.map { o4 =>
          val t4: Supplier = o4.map(supplierGetter).head
          assertEquals(Supplier(49, "Superior Coffee"), t4)
        }
      )), Duration.Inf)
    } finally dc.db.close()
  }

  @Test
  def testAllStatements: Unit = {
    val dc = DatabaseConfig.forAnnotation[JdbcProfile]
    import dc.driver.api._
    try {
      case class Supplier(id: Int, name: String)
      implicit val supplierGetter = (arg: (Int, String)) => Supplier(arg._1, arg._2)

      val testUnitDML = (x: Vector[Int]) => assertEquals(1, x.head)

      val s1 = tsql"select SUP_ID, SUP_NAME from SUPPLIERS where SUP_ID = 102"
      val s2 = tsql"select SUP_ID, SUP_NAME from SUPPLIERS where SUP_ID = 103"

      Await.result(dc.db.run(DBIO.seq(
        tsql"""create table "SUPPLIERS2" ("SUP_ID" INTEGER NOT NULL PRIMARY KEY,"SUP_NAME" VARCHAR NOT NULL);""",
        tsql"""INSERT INTO SUPPLIERS VALUES(102, 'Acme, Inc. Next', '99 Market Street', 'Groundsville', 'CA', '95199');""" map testUnitDML,
        tsql"""INSERT INTO SUPPLIERS VALUES(103, 'Coffee Retailers Corp.', '9 Random Street', 'Ville', 'LA', '63195');""" map testUnitDML,
        s1.map { o1 =>
          val t1: Supplier = o1.map(supplierGetter).head
          assertEquals(Supplier(102, "Acme, Inc. Next"), t1)
        },
        s2.map { o2 =>
          val t2: Supplier = o2.map(supplierGetter).head
          assertEquals(Supplier(103, "Coffee Retailers Corp."), t2)
        },
        tsql"""UPDATE SUPPLIERS SET SUP_NAME = 'Acme, Inc. II' WHERE SUP_ID = '102';""" map testUnitDML,
        tsql"""UPDATE SUPPLIERS SET SUP_NAME = 'Coffee Retailers Corp. II' WHERE SUP_ID = '103';""" map testUnitDML,
        s1.map { o1 =>
          val t1: Supplier = o1.map(supplierGetter).head
          assertEquals(Supplier(102, "Acme, Inc. II"), t1)
        },
        s2.map { o2 =>
          val t2: Supplier = o2.map(supplierGetter).head
          assertEquals(Supplier(103, "Coffee Retailers Corp. II"), t2)
        },
        tsql"""DELETE FROM SUPPLIERS WHERE SUP_ID = '102';""" map testUnitDML,
        tsql"""DELETE FROM SUPPLIERS WHERE SUP_ID = '103';""" map testUnitDML,
        tsql"""drop table "SUPPLIERS2" """
      ).withPinnedSession), Duration.Inf)
    } finally dc.db.close()
  }
}
