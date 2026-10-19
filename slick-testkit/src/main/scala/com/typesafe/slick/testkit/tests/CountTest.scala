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
      _ <- mark("q1", q1.result).map(_ shouldBe Vector(5))
      q2 = testTable.length
      _ <- mark("q2", q2.result).map(_ shouldBe 5)
      q3 = testTable.filter(_.id < 3).length
      _ <- mark("q3", q3.result).map(_ shouldBe 2)
      q4 = testTable.take(2).length
      _ <- mark("q4", q4.result).map(_ shouldBe 2)
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
        (a, b) <- as.joinLeft(bs).on(_.id === _.aId)
      } yield (a.id, b.map(_.data))).length.result.named("outerJoinLength").map(_ shouldBe 3)
    )
  }

  def testTableCount = {
    class T(tag: Tag) extends Table[(Long, String, Long, Option[Long], Option[Long])](tag, "TABLECOUNT_T") {
      def a = column[Long]("ID")
      def b = column[String]("B")
      def c = column[Long]("C")
      def d = column[Option[Long]]("DISCONTINUED")
      def e = column[Option[Long]]("E")
      def * = (a, b, c, d, e)
    }
    val ts = TableQuery[T]

    DBIO.seq(
      ts.schema.create,
      ts += (1L, "a", 1L, None, None),
      ts.length.result.map(_ shouldBe 1)
    ).withPinnedSession
  }

  def testLong = {
    class T(tag: Tag) extends Table[(Int, String)](tag, "COUNTLONG_T") {
      def id = column[Int]("ID")
      def name = column[String]("NAME")
      def * = (id, name)
    }
    val ts = TableQuery[T]

    val total: Rep[Long] = ts.lengthLong
    val distinctNames: Rep[Long] = ts.map(_.name).countDistinctLong
    val grouped: Query[(Rep[String], Rep[Long], Rep[Int], Rep[Long]), (String, Long, Int, Long), Seq] =
      ts.groupBy(_.name).map { case (name, g) => (name, g.lengthLong, g.length, g.map(_.id).countDistinctLong) }

    for {
      _ <- ts.schema.create
      _ <- ts ++= Seq((1, "a"), (2, "a"), (3, "b"), (4, "c"), (5, "c"), (5, "c"))
      _ <- total.result.map(_ shouldBe 6L)
      _ <- ts.length.result.map(_ shouldBe 6)
      _ <- ts.size.result.map(_ shouldBe 6)
      _ <- ts.filter(_.id > 2).lengthLong.result.map(_ shouldBe 4L)
      _ <- Query(ts.lengthLong).result.map(_ shouldBe Vector(6L))
      _ <- distinctNames.result.map(_ shouldBe 3L)
      _ <- ts.map(_.name).countDistinct.result.map(_ shouldBe 3)
      _ <- grouped.sortBy(_._1).result.map { r =>
        r shouldBe Vector(("a", 2L, 2, 2L), ("b", 1L, 1, 1L), ("c", 3L, 3, 2L))
        // Tuple equality treats 2 and 2L as equal, so check the boxed types too
        r.map(t => (t._2: Any, t._3: Any, t._4: Any)).foreach { case (l, i, d) =>
          l.getClass shouldBe classOf[java.lang.Long]
          i.getClass shouldBe classOf[java.lang.Integer]
          d.getClass shouldBe classOf[java.lang.Long]
        }
      }
    } yield ()
  }
}
