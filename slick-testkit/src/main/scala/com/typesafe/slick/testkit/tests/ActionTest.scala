package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{StandardTestDBs, RelationalTestDB, AsyncTest}

import scala.collection.mutable.ArrayBuffer
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
        ts.schema.create >>
        (ts ++= Seq(2, 3, 1, 5, 4))
      }
      q1 = ts.sortBy(_.a).map(_.a)
      f1 = db.run(q1.result)
      r1 <- f1 : Future[Seq[Int]]
      _ = r1 shouldBe List(1, 2, 3, 4, 5)
    } yield ()
  }

  def testSessionPinning = {
    class T(tag: Tag) extends Table[Int](tag, u"t") {
      def a = column[Int]("a")
      def * = a
    }
    val ts = TableQuery[T]

    val aSetup = ts.schema.create andThen (ts ++= Seq(2, 3, 1, 5, 4))

    val aNotPinned = for {
      p1 <- IsPinned
      s1 <- GetSession
      l <- ts.length.result
      p2 <- IsPinned
      s2 <- GetSession
      _ = p1 shouldBe false
      _ = p2 shouldBe false
      _ = s1 shouldNotBe s2
    } yield ()

    val aFused = for {
      ((s1, l), s2) <- GetSession zip ts.length.result zip GetSession
      _ = s1 shouldBe s2
    } yield ()

    val aPinned = for {
      _ <- (for {
        p1 <- IsPinned
        s1 <- GetSession
        l <- ts.length.result
        p2 <- IsPinned
        s2 <- GetSession
        _ = p1 shouldBe true
        _ = p2 shouldBe true
        _ = s1 shouldBe s2
      } yield ()).withPinnedSession
      p3 <- IsPinned
      _ = p3 shouldBe false
    } yield ()

    aSetup andThen aNotPinned andThen aFused andThen aPinned
  }

  def testStreaming = {
    class T(tag: Tag) extends Table[Int](tag, u"t") {
      def a = column[Int]("a")
      def * = a
    }
    val ts = TableQuery[T]
    val q1 = ts.sortBy(_.a).map(_.a)

    val p1 = db.stream {
      ts.schema.create >>
      (ts ++= Seq(2, 3, 1, 5, 4)) >>
      q1.result
    }

    for {
      r1 <- materialize(p1)
      _ = r1 shouldBe Vector(1, 2, 3, 4, 5)
      r2 <- db.run(q1.result.head)
      _ = r2 shouldBe 1
      r3 <- db.run(q1.result.headOption)
      _ = r3 shouldBe Some(1)
    } yield ()
  }

  def testDeepRecursion = if(tdb == StandardTestDBs.H2Disk) {
    val a1 = DBIO.sequence((1 to 5000).toSeq.map(i => LiteralColumn(i).result))
    val a2 = DBIO.sequence((1 to 20).toSeq.map(i => if(i%2 == 0) LiteralColumn(i).result else DBIO.from(Future.successful(i))))
    val a3 = DBIO.sequence((1 to 20).toSeq.map(i => if((i/4)%2 == 0) LiteralColumn(i).result else DBIO.from(Future.successful(i))))
    val a4 = DBIO.seq((1 to 50000).toSeq.map(i => DBIO.successful("a4")): _*)
    val a5 = (1 to 50000).toSeq.map(i => DBIO.successful("a5")).reduceLeft(_ andThen _)

    DBIO.seq(
      a1.map(_ shouldBe (1 to 5000).toSeq),
      a2.map(_ shouldBe (1 to 20).toSeq),
      a3.map(_ shouldBe (1 to 20).toSeq),
      a4.map(_ shouldBe (())),
      a5.map(_ shouldBe "a5")
    )
  } else DBIO.successful(())
}
