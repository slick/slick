package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{JdbcTestDB, AsyncTest}

class SequenceTest extends AsyncTest[JdbcTestDB] {
  import tdb.profile.api._

  def test1 = ifCap(scap.sequence) {
    case class User(id: Int, first: String, last: String)

    class Users(tag: Tag) extends Table[Int](tag, "users") {
      def id = column[Int]("id", O.PrimaryKey)
      def * = id
    }
    val users = TableQuery[Users]

    val mySequence = Sequence[Int]("mysequence") start 200 inc 10
    val ddl = users.schema ++ mySequence.schema
    val q1 = for(u <- users) yield (mySequence.next, u.id)
    q1.result.statements
    ddl.createStatements

    seq(
      ddl.create,
      users ++= Seq(1, 2, 3),
      q1.result.map(r => r.toSet shouldBe Set((200, 1), (210, 2), (220, 3))),
      ifCap(scap.sequenceCurr)(mySequence.curr.result.map(_ shouldBe 220))
    ).withPinnedSession
  }

  def test2 = ifCap(scap.sequence) {
    val s1 = Sequence[Int]("s1")
    val s2 = Sequence[Int]("s2") start 3
    val s3 = Sequence[Int]("s3") start 3 inc 2
    val s4 = Sequence[Int]("s4").cycle start 3 min 2 max 5
    val s5 = Sequence[Int]("s5").cycle start 3 min 2 max 5 inc -1
    val s6 = Sequence[Int]("s6") start 3 min 2 max 5

    def values(s: Sequence[Int], count: Int = 5, create: Boolean = true) = {
      val q = Query(s.next)
      (if(create) s.schema.create else DBIO.successful(())) >>
        DBIO.sequence((1 to count).toList map (_ => q.result.map(_.head)))
    }

    seq(
      values(s1).map(_ shouldBe List(1, 2, 3, 4, 5)),
      values(s2).map(_ shouldBe List(3, 4, 5, 6, 7)),
      values(s3).map(_ shouldBe List(3, 5, 7, 9, 11)),
      ifCap(scap.sequenceMin, scap.sequenceMax, scap.sequenceCycle)(seq(
        values(s4).map(_ shouldBe List(3, 4, 5, 2, 3)),
        values(s5).map(_ shouldBe List(3, 2, 5, 4, 3))
      )),
      ifCap(scap.sequenceMin, scap.sequenceMax, scap.sequenceLimited)(seq(
        values(s6, 3).map(_ shouldBe List(3, 4, 5)),
        values(s6, 1, false).failed
      ))
    ).withPinnedSession
  }
}
