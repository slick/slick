package scala.slick.test.ql.ql

import org.junit.Test
import org.junit.Assert._
import scala.slick.ql._
import scala.slick.session.Database.threadLocalSession
import scala.slick.testutil._
import scala.slick.testutil.TestDB._

object CountTest extends DBTestObject(H2Mem, SQLiteMem, Postgres, MySQL, DerbyMem, HsqldbMem, MSAccess, SQLServer)

class CountTest(val tdb: TestDB) extends DBTest {
  import tdb.profile.Table
  import tdb.profile.Implicit._

  @Test def test() = db withSession {
    object TestTable extends Table[Int]("TEST") {
      def id = column[Int]("ID")
      def * = id
    }
    TestTable.ddl.create
    TestTable.insertAll(1, 2, 3, 4, 5)

    val q1 = Query(TestTable.length)
    q1.dump("q1: ")
    println("q1: "+q1.selectStatement)
    assertEquals(5, q1.first)

    val q2 = Query(Query(TestTable).length)
    q2.dump("q2: ")
    println("q2: "+q2.selectStatement)
    assertEquals(5, q2.first)

    val q3 = Query(TestTable.filter(_.id < 3).length)
    q3.dump("q3: ")
    println("q3: "+q3.selectStatement)
    assertEquals(2, q3.first)

    val q4 = Query(Query(TestTable).take(2).length)
    q4.dump("q4: ")
    println("q4: "+q4.selectStatement)
    assertEquals(2, q4.first)
  }
}
