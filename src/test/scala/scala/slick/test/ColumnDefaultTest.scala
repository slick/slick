package scala.slick.test

import org.junit.Test
import org.junit.Assert._
import scala.slick.ql._
import scala.slick.ql.TypeMapper._
import scala.slick.driver.{ExtendedTable => Table}
import scala.slick.session._
import scala.slick.session.Database.threadLocalSession
import scala.slick.test.util._
import scala.slick.test.util.TestDB._

object ColumnDefaultTest extends DBTestObject(H2Mem, SQLiteMem, Postgres, MySQL, DerbyMem, HsqldbMem, SQLServer)

class ColumnDefaultTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit._

  case class User(id: Int, first: String, last: String)

  object A extends Table[(Int, String, Option[Boolean])]("a") {
    def id = column[Int]("id")
    def a = column[String]("a", O Default "foo")
    def b = column[Option[Boolean]]("b", O Default Some(true))
    def * = id ~ a ~ b
  }

  @Test def test() {
    db withSession {
      A.ddl.createStatements foreach println
      A.ddl.create
      A.id insert 42
      assertEquals(List((42, "foo", Some(true))), Query(A).list)
    }
  }
}
