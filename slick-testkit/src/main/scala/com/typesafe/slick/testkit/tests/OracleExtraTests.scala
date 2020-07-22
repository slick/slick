package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{AsyncTest, JdbcTestDB}
import slick.jdbc.OracleProfile

class OracleExtraTests extends AsyncTest[JdbcTestDB] {
  final lazy val oracleProfile = tdb.profile.asInstanceOf[OracleProfile]
  import oracleProfile.api._

  def testBlobCompare = {
    class A(tag: Tag) extends Table[(Int, Option[Array[Byte]])](tag, "a") {
      def id = column[Int]("id")
      def a = column[Option[Array[Byte]]]("a")
      def * = (id, a)
    }
    val as = TableQuery[A]

    DBIO.seq(
      as.schema.create,
      as += (1, Some(Array[Byte](1, 2, 3))),
      as.filter(_ => LiteralColumn[Option[Int]](None).isDefined).map(_.id).result.map(_ shouldBe Nil),
      as.filter(_ => LiteralColumn[Option[Int]](None).bind.isDefined).map(_.id).result.map(_ shouldBe Nil),
      as.filter(_.a.isEmpty).map(_.id).result.map(_ shouldBe Nil),
      as.filter(_.a === (Some(Array[Byte](1, 2, 3)): Option[Array[Byte]])).map(_.id).result.map(_ shouldBe Seq(1)),
      as.filter(_.a === (None: Option[Array[Byte]])).map(_.id).result.map(_ shouldBe Nil),
      as.filter(_.a === (Some(Array[Byte](1, 2, 3)): Option[Array[Byte]]).bind).map(_.id).result.map(_ shouldBe Seq(1)),
      as.filter(_.a === (None: Option[Array[Byte]]).bind).map(_.id).result.map(_ shouldBe Nil),
      as.filter(_ => LiteralColumn[Option[Int]](None) === (None: Option[Int])).map(_.id).result.map(_ shouldBe Nil)
    )
  }

  def testSequenceAndTriggerName = {
    class A(tag: Tag) extends Table[(Int, Int)](tag, "A_SEQTRG") {
      def id = column[Int]("ID", O.PrimaryKey, O.AutoInc, O.AutoIncSequenceName("SEQ_SEQTRG"), O.AutoIncTriggerName("TRG_SEQTRG"))
      def a = column[Int]("A")
      def * = (id, a)
    }
    val as = TableQuery[A]

    //as.schema.createStatements.foreach(println)
    as.schema.createStatements.should(_.find(_.contains("sequence \"SEQ_SEQTRG\"")).isDefined)
    as.schema.createStatements.should(_.find(_.contains("trigger \"TRG_SEQTRG\"")).isDefined)

    DBIO.seq(
      as.schema.create,
      as.map(_.a) ++= Seq(1, 2, 3),
      as.to[Set].result.map(_ shouldBe Set((1,1), (2,2), (3,3))),
      as.schema.drop
    )
  }
}
