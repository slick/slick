package scala.slick.test.jdbc

import com.typesafe.config.{ConfigException, ConfigFactory}
import org.junit.Test
import org.junit.Assert._
import scala.slick.collection.heterogenous.HNil
import scala.slick.collection.heterogenous.syntax._
import scala.slick.jdbc.CompileTimeConnection
import scala.slick.jdbc.JdbcBackend
import scala.slick.jdbc.StaticQuery.interpolation

class TypedStaticQueryTest {

  
  
  lazy val db = {
    val config = new TypedStaticQueryConfigHandler {
      val databaseName = "default"
      val configFileName = "reference.conf"
      val configGlobalPrefix = "typedsql."
      def abort(msg: String) = sys.error(msg)
    }
    config.connection
  }
  
  @Test
  def testTypedInterpolation = db withSession { implicit session =>
    implicit object conn extends CompileTimeConnection {
    val dbName = "default"
  }
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
  
  private[this] abstract class TypedStaticQueryConfigHandler {
    
    val databaseName: String
    val configFileName: String
    val configGlobalPrefix: String
    def abort(msg: String): Nothing
    
    lazy val conf = {
      import java.io.File
      val confFile: File = {
        val file = new File(configFileName)
        if (file.isFile() && file.exists())
          file
        else
          abort(s"Configuration file does not exist. Create a file: ${file.getAbsolutePath}")
      }
      ConfigFactory.parseFile(confFile)
    }

    @inline def getFromConfig(key: String): Option[String => String] = try {
      Some{ _key =>
        val c = conf.getConfig(configGlobalPrefix + key)
        try {
          c.getString(_key)
        } catch {
          case _: ConfigException => null
        }
      }
    } catch {
      case _: ConfigException.Missing => None
    }
    
    lazy val databaseConfig = getFromConfig(databaseName)
    
    lazy val connection = databaseConfig match {
      case Some(config) => JdbcBackend.Database.forURL(config("url"),
          user = config("user"),
          password = config("password"),
          driver = config("driver")
        )
      case None => abort(s"Configuration for database $databaseName not found")
    }
  }
}
