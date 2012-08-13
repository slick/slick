package com.typesafe.slick.testkit.tests

import org.junit.{Ignore, Test}
import org.junit.Assert._
import scala.slick.session.Database.threadLocalSession
import scala.slick.testutil.TestDB
import com.typesafe.slick.testkit.util.TestkitTest

//object ExecutorTest extends TestkitTestObject(H2Mem, SQLiteMem, Postgres, MySQL, DerbyMem, HsqldbMem, MSAccess, SQLServer)

class ExecutorTest(val tdb: TestDB) extends TestkitTest {
  import tdb.profile.Table
  import tdb.profile.Implicit._

  def test {

    object T extends Table[Int]("t") {
      def a = column[Int]("a")
      def * = a
    }

    db withSession {
      T.ddl.create
      T.insertAll(2, 3, 1, 5, 4)

      val q = T.sortBy(_.a).map(_.a)

      val r1 = q.list
      val r1t: List[Int] = r1
      assertEquals(List(1, 2, 3, 4, 5), r1t)

      val r2 = q.run
      val r2t: Seq[Int] = r2
      assertEquals(List(1, 2, 3, 4, 5), r2t)

      val r3 = q.length.run
      val r3t: Int = r3
      assertEquals(5, r3t)
    }
  }
}
