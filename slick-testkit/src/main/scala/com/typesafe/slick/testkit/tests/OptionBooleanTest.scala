package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{AsyncTest, JdbcTestDB}
import slick.jdbc.OracleProfile

class OptionBooleanTest extends AsyncTest[JdbcTestDB] {
  lazy val oracleProfile = tdb.profile.asInstanceOf[OracleProfile]
  import oracleProfile.api._

  def testFilterWithOption = {
    class A(tag: Tag) extends Table[(Int, Option[Boolean], Option[Int])](tag, "a") {
      def id = column[Int]("id")
      def b = column[Option[Boolean]]("a")
      def oi = column[Option[Int]]("oi")
      def * = (id, b, oi)
    }
    val as = TableQuery[A]

    DBIO.seq(
      as.schema.create,
      as += (1, Some(true), None),
      as += (2, Some(false), Some(1)),
      as += (3, None, Some(2)),
      as.filter(_.b.getOrElse(false) === false).result.map(_ shouldBe Seq(2, 3)),
      as.filter(_.oi.map(_ === 2).getOrElse(false)).result.map(_ shouldBe Seq(3)),
      as.schema.drop
    )
  }
}
