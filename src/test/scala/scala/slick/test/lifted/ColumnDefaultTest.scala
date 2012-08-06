package scala.slick.test.lifted

import org.junit.Test
import org.junit.Assert._
import scala.slick.lifted._
import scala.slick.session.Database.threadLocalSession
import scala.slick.testutil._
import scala.slick.testutil.TestDB._

object ColumnDefaultTest extends DBTestObject(H2Mem, SQLiteMem, Postgres, MySQL, DerbyMem, HsqldbMem, SQLServer)

class ColumnDefaultTest(val tdb: TestDB) extends DBTest {
  import tdb.profile.Table
  import tdb.profile.Implicit._

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
