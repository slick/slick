package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}

class ExecutorTest extends TestkitTest[JdbcTestDB] {
  import tdb.profile.simple._

  def test {

    class T(tag: Tag) extends Table[Int](tag, "t") {
      def a = column[Int]("a")
      def * = a
    }
    val ts = TableQuery[T]

    ts.ddl.create
    ts.insertAll(2, 3, 1, 5, 4)

    val q = ts.sortBy(_.a).map(_.a)

    val r1 = q.list
    val r1t: List[Int] = r1
    assertEquals(List(1, 2, 3, 4, 5), r1t)

    val r2 = q.run
    val r2t: Seq[Int] = r2
    assertEquals(List(1, 2, 3, 4, 5), r2t)

    assertTrue(q.executor.selectStatement.length > 0)

    val r3 = q.length.run
    val r3t: Int = r3
    assertEquals(5, r3t)
  }
}
