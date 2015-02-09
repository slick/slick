package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{AsyncTest, RelationalTestDB}

class ColumnDefaultTest extends AsyncTest[RelationalTestDB] {
  import tdb.profile.api._

  class A(tag: Tag) extends Table[(Int, String, Option[Boolean])](tag, "a") {
    def id = column[Int]("id")
    def a = column[String]("a", O Default "foo", O Length 254)
    def b = column[Option[Boolean]]("b", O Default Some(true))
    def * = (id, a, b)
  }
  lazy val as = TableQuery[A]

  def test = ifCap(rcap.columnDefaults) {
    for {
      _ <- as.schema.create
      _ <- as.map(_.id) += 42
      _ <- as.result.map(_ shouldBe List((42, "foo", Some(true))))
    } yield ()
  }
}
