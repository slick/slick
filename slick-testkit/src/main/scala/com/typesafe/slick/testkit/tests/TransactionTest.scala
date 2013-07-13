package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}

class TransactionTest extends TestkitTest[JdbcTestDB] {
  import tdb.profile.simple._

  def test {

    class T(tag: Tag) extends Table[Int](tag, "t") {
      def a = column[Int]("a")
      def * = a
    }
    val ts = TableQuery[T]

    ts.ddl.create

    sharedSession withTransaction {
      ts.insert(42)
      assertEquals(Some(42), ts.firstOption)
      sharedSession.rollback()
    }
    assertEquals(None, ts.firstOption)

    ts.insert(1)
    sharedSession withTransaction {
      ts.delete
      assertEquals(None, ts.firstOption)
      sharedSession.rollback()
    }
    assertEquals(Some(1), ts.firstOption)
  }
}
