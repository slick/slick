package com.typesafe.slick.testkit.tests

import org.junit.Test
import org.junit.Assert._
import scala.slick.lifted._
import scala.slick.session.Database.threadLocalSession
import scala.slick.testutil.TestDB
import com.typesafe.slick.testkit.util.TestkitTest

//object TransactionTest extends TestkitTestObject(H2Disk, SQLiteDisk, Postgres, MySQL, DerbyDisk, HsqldbDisk, MSAccess, SQLServer)

class TransactionTest(val tdb: TestDB) extends TestkitTest {
  import tdb.profile.Table
  import tdb.profile.Implicit._

  def test {

    val T = new Table[Int]("t") {
      def a = column[Int]("a")
      def * = a
    }

    db withSession {
      T.ddl.create

      val q = Query(T)

      threadLocalSession withTransaction {
        T.insert(42)
        assertEquals(Some(42), q.firstOption)
        threadLocalSession.rollback()
      }
      assertEquals(None, q.firstOption)

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
