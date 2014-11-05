package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{RelationalTestDB, AsyncTest}

import scala.concurrent.Future

class ActionTest extends AsyncTest[RelationalTestDB] {
  import tdb.profile.api._

  def testSimpleActionAsFuture = {
    class T(tag: Tag) extends Table[Int](tag, u"t") {
      def a = column[Int]("a")
      def * = a
    }
    val ts = TableQuery[T]

    for {
      _ <- db.run {
        ts.ddl.create >>
        (ts ++= Seq(2, 3, 1, 5, 4))
      }
      q1 = ts.sortBy(_.a).map(_.a)
      f1 = db.run(q1.result)
      r1 <- f1 : Future[Seq[Int]]
      _ = r1 shouldBe List(1, 2, 3, 4, 5)
    } yield ()
  }

  def testSimpleActionAsAction = {
    class T(tag: Tag) extends Table[Int](tag, u"t") {
      def a = column[Int]("a")
      def * = a
    }
    val ts = TableQuery[T]

    for {
      _ <- ts.schema.create
      _ <- ts ++= Seq(2, 3, 1, 5, 4)
      q1 = ts.sortBy(_.a).map(_.a)
      r1 <- q1.result
      _ = (r1 : Seq[Int]) shouldBe List(1, 2, 3, 4, 5)
    } yield ()
  }
}
