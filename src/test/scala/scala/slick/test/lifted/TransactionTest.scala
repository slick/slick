package scala.slick.test.lifted

import org.junit.Test
import org.junit.Assert._
import scala.slick.lifted._
import scala.slick.session.Database.threadLocalSession
import scala.slick.testutil._
import scala.slick.testutil.TestDB._

object TransactionTest extends DBTestObject(H2Disk, SQLiteDisk, Postgres, MySQL, DerbyDisk, HsqldbDisk, MSAccess, SQLServer)

class TransactionTest(val tdb: TestDB) extends DBTest {
  import tdb.profile.Table
  import tdb.profile.Implicit._

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

    db withSession {
      T.insert(1)
      threadLocalSession withTransaction {
        Query(T).delete
        assertEquals(None, q.firstOption)
        threadLocalSession.rollback()
      }
      assertEquals(Some(1), q.firstOption)
    }
  }
}
