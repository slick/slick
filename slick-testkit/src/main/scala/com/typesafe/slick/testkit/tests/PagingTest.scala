package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{RelationalTestDB, TestkitTest}

class PagingTest extends TestkitTest[RelationalTestDB] {
  import tdb.profile.simple._

  override val reuseInstance = true

  class IDs(tag: Tag, name: String) extends Table[Int](tag, name) {
    def id = column[Int]("id", O.PrimaryKey)
    def * = id
  }

  def testRawPagination {
    lazy val ids = TableQuery(new IDs(_, "ids_raw"))
    ids.ddl.create;
    ids ++= (1 to 10)

    val q1 = ids.sortBy(_.id)
    assertEquals((1 to 10).toList, q1.run)

    val q2 = q1 take 5
    assertEquals((1 to 5).toList, q2.run)

    ifCap(rcap.pagingDrop) {
      val q3 = q1 drop 5
      assertEquals((6 to 10).toList, q3.run)

      val q4 = q1 drop 5 take 3
      assertEquals((6 to 8).toList, q4.run)

      val q5 = q1 take 5 drop 3
      assertEquals((4 to 5).toList, q5.run)
    }

    val q6 = q1 take 0
    assertEquals(List(), q6.run)
  }

  def testCompiledPagination {
    lazy val ids = TableQuery(new IDs(_, "ids_compiled"))
    ids.ddl.create
    ids ++= (1 to 10)
    val q = Compiled { (offset: ConstColumn[Long], fetch: ConstColumn[Long]) =>
      ids.sortBy(_.id).drop(offset).take(fetch)
    }
    assertEquals((1 to 5).toList, q(0, 5).run)
    ifCap(rcap.pagingDrop) {
      assertEquals((6 to 10).toList, q(5, 1000).run)
      assertEquals((6 to 8).toList, q(5, 3).run)
    }
    assertEquals(List(), q(0, 0).run)
  }
}
