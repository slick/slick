package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}
import org.junit.Assert._

class OldTransactionTest extends TestkitTest[JdbcTestDB] {
  import tdb.profile.simple._

  def test {

    class T(tag: Tag) extends Table[Int](tag, "t") {
      def a = column[Int]("a")
      def * = a
    }
    val ts = TableQuery[T]

    ts.ddl.create

    implicitSession withTransaction {
      ts.insert(42)
      assertEquals(Some(42), ts.firstOption)
      implicitSession.rollback()
    }
    assertEquals(None, ts.firstOption)

    ts.insert(1)
    implicitSession withTransaction {
      ts.delete
      assertEquals(None, ts.firstOption)
      implicitSession.rollback()
    }
    assertEquals(Some(1), ts.firstOption)
  }
}
