package scala.slick.test

import org.junit.Test
import org.junit.Assert._
import scala.slick.ql._
import scala.slick.driver.{ExtendedTable => Table}
import scala.slick.session.Database.threadLocalSession
import scala.slick.test.util._
import scala.slick.test.util.TestDB._

object TransactionTest extends DBTestObject(H2Disk, SQLiteDisk, Postgres, MySQL, DerbyDisk, HsqldbDisk, MSAccess, SQLServer)

class TransactionTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit._

  @Test def test() {

    val T = new Table[Int]("t") {
      def a = column[Int]("a")
      def * = a
    }

    db withSession {
      T.ddl.create
    }

    val q = Query(T)

    db withSession {
      threadLocalSession withTransaction {
        T.insert(42)
        assertEquals(Some(42), q.firstOption)
        threadLocalSession.rollback()
      }
      assertEquals(None, q.firstOption)
    }
  }
}
