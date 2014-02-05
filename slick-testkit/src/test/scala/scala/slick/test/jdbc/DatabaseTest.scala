package scala.slick.test.jdbc

import org.junit.Test
import org.junit.Assert._
import scala.slick.testutil._
import scala.slick.testutil.TestDBs._
import com.typesafe.slick.testkit.util.JdbcTestDB

object DatabaseTest extends DBTestObject(H2Mem, SQLiteMem, Postgres, MySQL, DerbyMem, HsqldbMem)

class DatabaseTest(val tdb: JdbcTestDB) extends DBTest {
  import tdb.profile.simple._

  @Test def testNestedSession() {
    db withNestedSession { s1:Session =>
      db withNestedSession{ s2:Session =>
        assertEquals(s1,s2)
      }
      //assert(s1.open) //TODO check if it's opened
    }
  }

  @Test def testDynWithNestedSession() {
    db withDynSession {
      val s1 = Database.dynamicSession
      db withNestedSession{ s2:Session =>
        assertEquals(s1,s2)
      }
      //assert(s1.open) //TODO check if it's opened
    }
  }

  @Test def testDynNestedSession() {
    db withDynSession {
      val s1 = Database.dynamicSession
      db withNestedSession{
        val s2 = Database.dynamicSession
        assertEquals(s1,s2)
      }
      //assert(s1.open) //TODO check if it's opened
    }
  }

  @Test def testNestedTransaction(){
    db.withNestedTransaction{s1:Session =>
      db.withNestedTransaction{s2:Session =>
        assertEquals(s1, s2)
      }
      //assert(s1.open) //TODO check if it's opened
    }
  }

  @Test def testDynWithNestedTransaction(){
    db.withDynTransaction {
      val s1 = Database.dynamicSession
      db.withNestedTransaction{ s2:Session =>
        assertEquals(s1, s2)
      }
      //assert(s1.open) //TODO check if it's opened
    }
  }

  @Test def testDynNestedTransaction(){
    db.withDynTransaction {
      val s1 = Database.dynamicSession
      db.withNestedTransaction{
        val s2 = Database.dynamicSession
        assertEquals(s1, s2)
      }
      //assert(s1.open) //TODO check if it's opened
    }
  }
}