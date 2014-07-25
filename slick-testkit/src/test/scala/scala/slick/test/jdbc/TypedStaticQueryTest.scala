package scala.slick.test.jdbc

import org.junit.Test
import org.junit.Assert._
import scala.slick.collection.heterogenous.HNil
import scala.slick.collection.heterogenous.syntax._
import scala.slick.jdbc.StaticQuery.interpolation
import scala.slick.jdbc.TypedStaticQuery._

@TSQLConfig("default")
class TypedStaticQueryTest {

  val config = getConfigHandler()

  @Test
  def testTypedInterpolation = config.connection withSession { implicit session =>
    val id1 = 150
    val id2 = 1
    val s1 = tsql"select * from SUPPLIERS where SUP_ID = ${id1}"
    val s2 = tsql"select * from COFFEES where SUP_ID = ${id2}"
    assertEquals("select * from SUPPLIERS where SUP_ID = ?", s1.getStatement)
    assertEquals("select * from COFFEES where SUP_ID = ?", s2.getStatement)
    val list1 = s1.list
    val typedList1: List[(Int, String, String, String, String, String)] = list1
    assertEquals(List((150, "The High Ground", "100 Coffee Lane", "Meadows", "CA", "93966")), typedList1)
    val list2 = s2.list
    val typedList2: List[(String, Int, Double, Int, Int)] = list2
    assertEquals(List(("coffee", 1, 2.3, 4, 5)), typedList2)

    val (total1, sales1) = (5, 4)
    val s3 = tsql"select COF_NAME from COFFEES where SALES = ${sales1} and TOTAL = ${total1}"
    val cof1: String = s3.first
    assertEquals("coffee", cof1)
    
    val s4 = tsql"select 1, '2', 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23"
    val hlist1 = s4.first
    val hlist1Typed: Int :: String :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: HNil = s4.first
    assertEquals(1 :: "2" :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: 11 :: 12 :: 13 :: 14 :: 15 :: 16 :: 17 :: 18 :: 19 :: 20 :: 21 :: 22 :: 23 :: HNil, hlist1Typed)
  }
  
  @Test
  def testCustomTypes = config.connection withSession { implicit session =>
    import scala.slick.jdbc.SetParameter
    
    case class Foo(intVal: Int)
    case class Bar(strVal: String)
    
    implicit val SetFoo = SetParameter[Foo]{ (i, pp) =>
      SetParameter.SetInt(i.intVal, pp)
    }
    implicit val SetBar = SetParameter[Bar]{ (s, pp) =>
      SetParameter.SetString(s.strVal, pp)
    }
    
    val foo = new Foo(150)
    val bar = new Bar("Something")
    
    val s1 = tsql"select * from SUPPLIERS where SUP_ID = ${foo}"
    val o1 = s1.list
    val t1: List[(Int, String, String, String, String, String)] = o1
    assertEquals(List((150, "The High Ground", "100 Coffee Lane", "Meadows", "CA", "93966")), t1)
    
    val num = 15
    val s2 = tsql"select * from SUPPLIERS where SUP_ID = ${num * 10}"
    val o2 = s2.list
    val t2: List[(Int, String, String, String, String, String)] = o2
    assertEquals(List((150, "The High Ground", "100 Coffee Lane", "Meadows", "CA", "93966")), t2)
    
    val s3 = tsql"select SUP_ID from SUPPLIERS"
    val o3 = s3.mapResult(Foo(_)).list
    val t3: List[Foo] = o3
    assertEquals(List(Foo(101), Foo(150), Foo(49)), t3)
    
    val s4 = tsql"select CITY from SUPPLIERS"
    val o4 = s4.mapResult(Bar(_)).list
    val t4: List[Bar] = o4
    assertEquals(List(Bar("Groundsville"), Bar("Meadows"), Bar("Mendocino")), t4)
  }

  @Test
  def testPreparedQueries = config.connection withSession { implicit session =>
    case class Supplier(id: Int, name: String)
    implicit val supplierGetter = (arg: (Int, String)) => Supplier(arg._1, arg._2)

    def supplierForID(id: Int) =
      tsql"select SUP_ID, SUP_NAME from SUPPLIERS where SUP_ID = $id"
    def supplierForIdAndName(id: Int, name: String) =
      tsql"select SUP_ID, SUP_NAME from SUPPLIERS where SUP_ID = $id and SUP_NAME = $name"

    val s1 = supplierForID(101)
    val o1 = s1.mapResult[Supplier].first
    val t1: Supplier = o1
    assertEquals(Supplier(101, "Acme, Inc."), t1)
    val s2 = supplierForID(49)
    val o2 = s2.mapResult(supplierGetter).first
    val t2: Supplier = o2
    assertEquals(Supplier(49, "Superior Coffee"), t2)

    val s3 = supplierForIdAndName(150, "The High Ground")
    val o3 = s3.mapResult[Supplier].first
    val t3: Supplier = o3
    assertEquals(Supplier(150, "The High Ground"), o3)
    val s4 = supplierForIdAndName(49, "Superior Coffee")
    val o4 = s4.mapResult(supplierGetter).first
    val t4: Supplier = o4
    assertEquals(Supplier(49, "Superior Coffee"), t4)
  }

  @Test
  def testAllStatements = config.connection withSession { implicit session =>
    case class Supplier(id: Int, name: String)
    implicit val supplierGetter = (arg: (Int, String)) => Supplier(arg._1, arg._2)

    tsql"""create table "SUPPLIERS2" ("SUP_ID" INTEGER NOT NULL PRIMARY KEY,"SUP_NAME" VARCHAR NOT NULL);""".execute

    val u1 = tsql"""INSERT INTO SUPPLIERS VALUES(102, 'Acme, Inc. Next', '99 Market Street', 'Groundsville', 'CA', '95199');""".first
    assertEquals(1, u1)
    val u2 = tsql"""INSERT INTO SUPPLIERS VALUES(103, 'Coffee Retailers Corp.', '9 Random Street', 'Ville', 'LA', '63195');""".first
    assertEquals(1, u2)

    val s1 = tsql"select SUP_ID, SUP_NAME from SUPPLIERS where SUP_ID = 102"
    val o1 = s1.mapResult[Supplier].first
    val t1: Supplier = o1
    assertEquals(Supplier(102, "Acme, Inc. Next"), t1)
    val s2 = tsql"select SUP_ID, SUP_NAME from SUPPLIERS where SUP_ID = 103"
    val o2 = s2.mapResult(supplierGetter).first
    val t2: Supplier = o2
    assertEquals(Supplier(103, "Coffee Retailers Corp."), t2)

    val u3 = tsql"""UPDATE SUPPLIERS SET SUP_NAME = 'Acme, Inc. II' WHERE SUP_ID = '102';""".first
    assertEquals(1, u3)
    val u4 = tsql"""UPDATE SUPPLIERS SET SUP_NAME = 'Coffee Retailers Corp. II' WHERE SUP_ID = '103';""".first
    assertEquals(1, u4)

    val s3 = tsql"select SUP_ID, SUP_NAME from SUPPLIERS where SUP_ID = 102"
    val o3 = s3.mapResult[Supplier].first
    val t3: Supplier = o3
    assertEquals(Supplier(102, "Acme, Inc. II"), t3)
    val s4 = tsql"select SUP_ID, SUP_NAME from SUPPLIERS where SUP_ID = 103"
    val o4 = s4.mapResult(supplierGetter).first
    val t4: Supplier = o4
    assertEquals(Supplier(103, "Coffee Retailers Corp. II"), t4)

    val u5 = tsql"""DELETE FROM SUPPLIERS WHERE SUP_ID = '102';""".first
    assertEquals(1, u5)
    val u6 = tsql"""DELETE FROM SUPPLIERS WHERE SUP_ID = '103';""".first
    assertEquals(1, u6)

    tsql"""drop table "SUPPLIERS2" """.execute
  }

  @Test
  @TSQLConfig(url = "jdbc:h2:mem:test1", driver = "org.h2.Driver", slickDriver = "scala.slick.driver.H2Driver")
  def testAllConfigHandlerMacroStyles = {
    testConfigHandlerMacro(getConfigHandler())
    testConfigHandlerMacro(config)
    testConfigHandlerMacro(TypedStaticQueryTest.chVal1)
    testConfigHandlerMacro(TypedStaticQueryTest.chVal2)
    testConfigHandlerMacro(TypedStaticQueryTest.chDef1)
    testConfigHandlerMacro(TypedStaticQueryTest.chDef2)
  }

  def testConfigHandlerMacro(ch: ConfigHandler) = {
    assertEquals("jdbc:h2:mem:test1", ch.url.get.take(17))
    assertEquals(None, ch.user)
    assertEquals(None, ch.password)
    assertEquals("org.h2.Driver", ch.jdbcDriver.get)
    assertEquals("scala.slick.driver.H2Driver", ch.slickDriver.get)
  }
}

object TypedStaticQueryTest extends TypedStaticQuerySupport

@TSQLConfig(url = "jdbc:h2:mem:test1", driver = "org.h2.Driver", slickDriver = "scala.slick.driver.H2Driver")
trait TypedStaticQuerySupport {
  def chDef1 = getConfigHandler()
  @TSQLConfig("default") def chDef2 = getConfigHandler()
  val chVal1 = getConfigHandler()
  @TSQLConfig("default") val chVal2 = getConfigHandler()
}
