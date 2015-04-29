package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{RelationalTestDB, AsyncTest}

class CountTest extends AsyncTest[RelationalTestDB] {
  import tdb.profile.api._

  def testSimple = {
    class TestTable(tag: Tag) extends Table[Int](tag, "TEST") {
      def id = column[Int]("ID")
      def * = id
    }
    val testTable = TableQuery(new TestTable(_))
    for {
      _ <- testTable.schema.create
      _ <- testTable ++= Seq(1, 2, 3, 4, 5)
      q1 = Query(testTable.length)
      _ <- q1.result.map(_ shouldBe Vector(5))
      q2 = testTable.length
      _ <- q2.result.map(_ shouldBe 5)
      q3 = testTable.filter(_.id < 3).length
      _ <- q3.result.map(_ shouldBe 2)
      q4 = testTable.take(2).length
      _ <- q4.result.map(_ shouldBe 2)
    } yield ()
  }

  def testJoin = {
    class Categories(tag: Tag) extends Table[(Int, String)](tag, "cat_j") {
      def id = column[Int]("id")
      def name = column[String]("name")
      def * = (id, name)
    }
    val categories = TableQuery[Categories]

    class Posts(tag: Tag) extends Table[(Int, String, Int)](tag, "posts_j") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def title = column[String]("title")
      def category = column[Int]("category")
      def * = (id, title, category)
    }
    val posts = TableQuery[Posts]

    for {
      _ <- (categories.schema ++ posts.schema).create
      _ <- categories ++= Seq((1, "Scala"), (2, "JVM"), (3, "Java"), (4, "Erlang"), (5, "Haskell"))
      _ <- posts ++= Seq((1, "Shiny features", 1), (2, "HotSpot", 2))
      joinedQuery = for {
        c <- categories
        p <- posts
        if p.category === c.id
      } yield (c, p)
      q1 = joinedQuery.length
      _ <- q1.result.map(_ shouldBe 2)
      q2 = Query(joinedQuery.length)
      _ <- q2.result.map(_ shouldBe Vector(2))
    } yield ()
  }

  def testJoinCount = {
    class A(tag: Tag) extends Table[Long](tag, "a_j") {
      def id = column[Long]("id", O.PrimaryKey)
      def * = id
    }
    lazy val as = TableQuery[A]
    class B(tag: Tag) extends Table[(Long, String)](tag, "b_j") {
      def aId = column[Long]("a_id")
      def data = column[String]("data")
      def * = (aId, data)
    }
    lazy val bs = TableQuery[B]
    DBIO.seq(
      (as.schema ++ bs.schema).create,
      as ++= Seq(1L, 2L),
      bs ++= Seq((1L, "1a"), (1L, "1b"), (2L, "2")),
      (for {
        a <- as if a.id === 1L
      } yield (a, (for {
          b <- bs if b.aId === a.id
        } yield b).length)).result.named("directLength").map(_ shouldBe Seq((1L, 2))),
      (for {
        a <- as if a.id === 1L
        l <- Query((for {
          b <- bs if b.aId === a.id
        } yield b).length)
      } yield (a, l)).result.named("joinLength").map(_ shouldBe Seq((1L, 2))),
      (for {
        (a, b) <- as joinLeft bs on (_.id === _.aId)
      } yield (a.id, b.map(_.data))).length.result.named("outerJoinLength").map(_ shouldBe 3)
    )
  }
}
