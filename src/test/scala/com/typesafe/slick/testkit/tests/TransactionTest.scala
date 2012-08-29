package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import scala.slick.lifted._
import scala.slick.testutil.TestDB
import com.typesafe.slick.testkit.util.TestkitTest

class TransactionTest(val tdb: TestDB) extends TestkitTest {
  import tdb.profile.Table
  import tdb.profile.Implicit._

  def test {

    val T = new Table[Int]("t") {
      def a = column[Int]("a")
      def * = a
    }

    T.ddl.create

    val q = Query(T)

    sharedSession withTransaction {
      T.insert(42)
      assertEquals(Some(42), q.firstOption)
      sharedSession.rollback()
    }
    assertEquals(None, q.firstOption)

    T.insert(1)
    sharedSession withTransaction {
      Query(T).delete
      assertEquals(None, q.firstOption)
      sharedSession.rollback()
    }
    assertEquals(Some(1), q.firstOption)
  }
}
