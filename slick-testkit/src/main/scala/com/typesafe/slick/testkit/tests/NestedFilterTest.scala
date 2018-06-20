package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{AsyncTest, JdbcTestDB, RelationalTestDB}

class NestedFilterTest extends AsyncTest[JdbcTestDB] {
  import tdb.profile.api._


  case class Data(id: Int, relatesTo: Option[Int], name: String)

  class A(tag: Tag) extends Table[Data](tag, "a") {
    def id = column[Int]("id", O.PrimaryKey)
    def relatesTo = column[Option[Int]]("relatesTo")
    def name = column[String]("name")
    def * = (id, relatesTo, name).mapTo[Data]
  }

  lazy val as = TableQuery[A]

  def test = {
    val filterQuery = as.filter(e => as.filter(r => e.relatesTo === r.id).exists)

    for {
      _ <- as.schema.create
      _ <- as ++= Seq(
        Data(1, None, "Root"),
        Data(2, Some(1), "old")
      )
      _ <- filterQuery.result.map(_.length shouldBe 1)
      _ <- filterQuery.map(_.name).update("new").map(_ shouldBe 1)
    } yield ()
  }
}
