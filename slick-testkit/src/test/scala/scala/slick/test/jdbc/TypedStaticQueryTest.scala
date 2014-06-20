package scala.slick.test.jdbc

import org.junit.Test
import org.junit.Assert._
import scala.slick.collection.heterogenous.HNil
import scala.slick.collection.heterogenous.syntax._
import scala.slick.jdbc.StaticQuery.interpolation
import scala.slick.jdbc.TypedStaticQuery.{CompileTimeConnection, ConfigHandler}

class TypedStaticQueryTest {

  implicit object conn extends CompileTimeConnection {
    val dbName = "default"
  }

  lazy val config = new ConfigHandler {
    val databaseName = conn.dbName
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
    val hlist1: Int :: String :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: HNil = s4.first
    assertEquals(1 :: "2" :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: 11 :: 12 :: 13 :: 14 :: 15 :: 16 :: 17 :: 18 :: 19 :: 20 :: 21 :: 22 :: 23 :: HNil, hlist1)
  }
  
  @Test
  def testCustomTypes = config.connection withSession { implicit session =>
    import scala.slick.jdbc.SetParameter
    
    class Foo(val intVal: Int)
    class Bar(val strVal: String)
    
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
    val t1: List[(Int, String, String, String, String, String)] = s1.list
    assertEquals(List((150, "The High Ground", "100 Coffee Lane", "Meadows", "CA", "93966")), t1)
    
    val num = 15
    val s2 = tsql"select * from SUPPLIERS where SUP_ID = ${num * 10}"
    val t2: List[(Int, String, String, String, String, String)] = s1.list
    assertEquals(List((150, "The High Ground", "100 Coffee Lane", "Meadows", "CA", "93966")), t2)
  }
}
