package scala.slick.test.ql

import org.junit.After
import org.junit.Test
import org.junit.Assert._
import scala.slick.ql._
import scala.slick.ql.TypeMapper._
import scala.slick.driver.{ExtendedTable => Table, SQLiteDriver}
import scala.slick.jdbc.meta.MTable
import scala.slick.session._
import scala.slick.session.Database.threadLocalSession
import scala.slick.testutil._
import scala.slick.testutil.TestDB._

object PrimaryKeyTest extends DBTestObject(H2Mem, Postgres, MySQL, DerbyMem, HsqldbMem, SQLiteMem, MSAccess, SQLServer)

class PrimaryKeyTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit._

  @Test def test1(): Unit = db withSession {

    object A extends Table[(Int, Int, String)]("a") {
      def k1 = column[Int]("k1")
      def k2 = column[Int]("k2")
      def s = column[String]("s")
      def * = k1 ~ k2 ~ s
      def pk = primaryKey("pk_a", (k1, k2))
    }

    A.primaryKeys.foreach(println)
    assertEquals(Set("pk_a"), A.primaryKeys.map(_.name).toSet)

    A.ddl.createStatements foreach println
    A.ddl create;

    A insertAll (
      (1, 1, "a11"),
      (1, 2, "a12"),
      (2, 1, "a21"),
      (2, 2, "a22")
    )

    assertFail { A.insert(1, 1, "a11-conflict") }
  }
}