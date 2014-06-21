package scala.slick.test.jdbc

import org.junit.Test
import org.junit.Assert._
import scala.slick.collection.heterogenous.HNil
import scala.slick.collection.heterogenous.syntax._
import scala.slick.jdbc.StaticQuery.interpolation
import scala.slick.jdbc.TypedStaticQuery.ConfigHandler

class TypedStaticQueryTest {

  implicit val config = new ConfigHandler {
//    override val databaseName = Some("default")
    override val url = Some("jdbc:h2:mem:test1;INIT=runscript from 'slick-testkit/src/codegen/resources/dbs/h2mem/create.sql'\\;runscript from 'slick-testkit/src/codegen/resources/dbs/h2mem/populate.sql'")
    override val jdbcDriver = Some("org.h2.Driver")
  }
  
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
      println("Yes this was called")
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
    val o3 = s3.as(Foo(_)).list
    val t3: List[Foo] = o3
    assertEquals(List(Foo(101), Foo(150), Foo(49)), t3)
    
    val s4 = tsql"select CITY from SUPPLIERS"
    val o4 = s4.as(Bar(_)).list
    val t4: List[Bar] = o4
    assertEquals(List(Bar("Groundsville"), Bar("Meadows"), Bar("Mendocino")), t4)
  }

//  @Test
  //@TSQLConfig("default")
//  def testNoResult = getConfigHandler().connection withSession { implicit session =>
//
//    StringContext("create table \"COFFEES2\" (\"COF_NAME\" VARCHAR NOT NULL PRIMARY KEY, \"PRICE\" INTEGER NOT NULL)").sqlu().execute
//    sqlu"INSERT INTO COFFEES2 VALUES('coffee', 3);".execute
//
//    val id1 = "coffee"
//    val s1 = tsql"select * from COFFEES2 where COF_NAME = ${id1}"
//    assertEquals("select * from COFFEES2 where COF_NAME = ?", s1.getStatement)
//    val list1 = s1.as[(String, Int)].list
//    //val list1 = s1.list
//    val typedList1: List[(String, Int)] = list1
//    assertEquals(List(("coffee", 3)), typedList1)
//  }
}
