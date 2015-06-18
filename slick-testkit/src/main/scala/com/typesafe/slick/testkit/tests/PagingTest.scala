package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{RelationalTestDB, AsyncTest}

class PagingTest extends AsyncTest[RelationalTestDB] {
  import tdb.profile.api._

  class IDs(tag: Tag, name: String) extends Table[Int](tag, name) {
    def id = column[Int]("id", O.PrimaryKey)
    def * = id
  }

  def testRawPagination = {
    lazy val ids = TableQuery(new IDs(_, "ids_raw"))
    val q1 = ids.sortBy(_.id)
    val q2 = q1 take 5
    def q3 = q1 drop 5
    def q4 = q1 drop 5 take 3
    def q5 = q1 take 5 drop 3
    val q6 = q1 take 0

    for {
      _ <- ids.schema.create
      _ <- ids ++= (1 to 10)
      _ <- mark("q1", q1.result).map(_ shouldBe (1 to 10).toList)
      _ <- mark("q2", q2.result).map(_ shouldBe (1 to 5).toList)
      _ <- ifCap(rcap.pagingDrop)(seq(
        q3.result.map(_ shouldBe (6 to 10).toList),
        q4.result.map(_ shouldBe (6 to 8).toList),
        q5.result.map(_ shouldBe (4 to 5).toList)
      ))
      _ <- mark("q6", q6.result).map(_ shouldBe Nil)
    } yield ()
  }

  def testCompiledPagination = {
    lazy val ids = TableQuery(new IDs(_, "ids_compiled"))
    val q = Compiled { (offset: ConstColumn[Long], fetch: ConstColumn[Long]) =>
      ids.sortBy(_.id).drop(offset).take(fetch)
    }
    seq(
      ids.schema.create,
      ids ++= (1 to 10),
      q(0, 5).result.map(_ shouldBe (1 to 5).toList),
      ifCap(rcap.pagingDrop)(seq(
        q(5, 1000).result.map(_ shouldBe (6 to 10).toList),
        q(5, 3).result.map(_ shouldBe (6 to 8).toList)
      )),
      q(0, 0).result.map(_ shouldBe Nil)
    )
  }
}
