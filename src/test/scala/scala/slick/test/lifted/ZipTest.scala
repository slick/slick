package scala.slick.test.lifted

import org.junit.Test
import org.junit.Assert._
import scala.slick.driver.{DerbyDriver, AccessDriver, SQLiteDriver}
import scala.slick.session.Database.threadLocalSession
import scala.slick.testutil._
import scala.slick.testutil.TestDB._

object ZipTest extends DBTestObject(H2Mem, Postgres, MySQL, HsqldbMem, SQLServer)

class ZipTest(val tdb: TestDB) extends DBTest {
  import tdb.profile.simple._

  object Categories extends Table[(Int, String)]("categories") {
    def id = column[Int]("id")
    def name = column[String]("name")
    def * = id ~ name
  }

  object Posts extends Table[(Int, String, Int)]("posts") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def title = column[String]("title")
    def category = column[Int]("category")
    def * = id ~ title ~ category
  }

  @Test def testZip(): Unit = db withSession {
    (Categories.ddl ++ Posts.ddl).create

    Categories insertAll (
      (1, "Scala"),
      (3, "Windows"),
      (2, "ScalaQuery"),
      (4, "Software")
    )
    Posts.title ~ Posts.category insertAll (
      ("Test Post", -1),
      ("Formal Language Processing in Scala, Part 5", 1),
      ("Efficient Parameterized Queries in ScalaQuery", 2),
      ("Removing Libraries and HomeGroup icons from the Windows 7 desktop", 3),
      ("A ScalaQuery Update", 2)
    )

    val q1 = for {
      (c, i) <- Categories.sortBy(_.id).zipWithIndex
    } yield (c.id, i)
    println("ZipWithIndex: "+q1.selectStatement)
    q1.foreach(x => println("  "+x))
    assertEquals(List((1,0), (2,1), (3,2), (4,3)), q1.list)

    val q2 = for {
      (c, p) <- Categories.sortBy(_.id) zip Posts.sortBy(_.category)
    } yield (c.id, p.category)
    println("Zip: "+q2.selectStatement)
    q2.foreach(x => println("  "+x))
    assertEquals(List((1,-1), (2,1), (3,2), (4,2)), q2.list)
  }
}
