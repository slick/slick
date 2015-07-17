package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{RelationalTestDB, AsyncTest}

class ForeignKeyTest extends AsyncTest[RelationalTestDB] {
  import tdb.profile.api._

  def testSimple = {
    class Categories(tag: Tag) extends Table[(Int, String)](tag, "categories") {
      def id = column[Int]("id", O.PrimaryKey)
      def name = column[String]("name")
      def * = (id, name)
    }
    val categories = TableQuery[Categories]

    class Posts(tag: Tag) extends Table[(Int, String, Option[Int])](tag, "posts") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def title = column[String]("title")
      def category = column[Option[Int]]("category")
      def * = (id, title, category)
      def categoryFK = foreignKey("category_fk", category, categories)(_.id.?)
      def categoryJoin = categories.filter(_.id === category)
    }
    val posts = TableQuery[Posts]

    for {
      _ <- tdb.assertNotTablesExist("categories", "posts")
      _ <- (posts.schema ++ categories.schema).create
      _ <- tdb.assertTablesExist("categories", "posts")
      _ <- categories ++= Seq(
        (1, "Scala"),
        (2, "ScalaQuery"),
        (3, "Windows"),
        (4, "Software")
      )
      _ <- posts.map(p => (p.title, p.category)) ++= Seq(
        ("Test Post", None),
        ("Formal Language Processing in Scala, Part 5", Some(1)),
        ("Efficient Parameterized Queries in ScalaQuery", Some(2)),
        ("Removing Libraries and HomeGroup icons from the Windows 7 desktop", Some(3)),
        ("A ScalaQuery Update", Some(2))
      )
      q1 = (for {
        p <- posts
        c <- p.categoryJoin
      } yield (p.id, c.id, c.name, p.title)).sortBy(_._1)
      _ <- q1.map(p => (p._1, p._2)).result.map(_ shouldBe List((2,1), (3,2), (4,3), (5,2)))
      q2 = (for {
        p <- posts
        c <- p.categoryFK
      } yield (p.id, c.id, c.name, p.title)).sortBy(_._1)
      _ <- q2.map(p => (p._1, p._2)).result.map(_ shouldBe List((2,1), (3,2), (4,3), (5,2)))
      _ <- (categories.schema ++ posts.schema).drop
      _ <- tdb.assertNotTablesExist("categories", "posts")
    } yield ()
  }

  def testMultiColumn = {
    class A(tag: Tag) extends Table[(Int, Int, String)](tag, "a") {
      def k1 = column[Int]("k1")
      def k2 = column[Int]("k2")
      def s = column[String]("s")
      def * = (k1, k2, s)
      def bFK = foreignKey("b_fk", (k1, k2), bs)(b => (b.f1, b.f2), onDelete = ForeignKeyAction.Cascade)
    }
    lazy val as = TableQuery[A]

    class B(tag: Tag) extends Table[(Int, Int, String)](tag, "b") {
      def f1 = column[Int]("f1")
      def f2 = column[Int]("f2")
      def s = column[String]("s")
      def * = (f1, f2, s)
      def bIdx1 = index("b_idx1", (f1, f2), unique = true)
    }
    lazy val bs = TableQuery[B]

    as.baseTableRow.foreignKeys.map(_.name).toSet shouldBe Set("b_fk")

    for {
      _ <- (as.schema ++ bs.schema).create
      _ <- bs ++= Seq(
        (1, 2, "b12"),
        (3, 4, "b34"),
        (5, 6, "b56")
      )
      _ <- as ++= Seq(
        (1, 2, "a12"),
        (3, 4, "a34")
      )
      q1 = (for {
        a <- as
        b <- a.bFK
      } yield (a.s, b.s)).to[Set]
      _ <- q1.result.map(_ shouldBe Set(("a12","b12"), ("a34","b34")))
    } yield ()
  }

  def testCombinedJoin = {
    class A(tag: Tag) extends Table[(Int, String)](tag, "a2") {
      def id = column[Int]("id", O.PrimaryKey)
      def s = column[String]("s")
      def * = (id, s)
    }
    val as = TableQuery[A]
    class Dep(tag: Tag, n: String) extends Table[(Int, Int)](tag, n) {
      def id = column[Int]("id", O.PrimaryKey)
      def aRef = column[Int]("aRef")
      def * = (id, aRef)
      def a = foreignKey(n+"_a2_fk", aRef, as)(_.id)
    }
    val bs = TableQuery(new Dep(_, "b2"))
    val cs = TableQuery(new Dep(_, "c2"))

    for {
      _ <- (as.schema ++ bs.schema ++ cs.schema).create
      _ <- as ++= Seq((1, "a"), (2, "b"), (3, "c"), (4, "d"))
      _ <- bs ++= Seq((1, 1), (2, 1), (3, 2))
      _ <- cs ++= Seq((1, 1), (2, 3))
      q1 = (for {
        b <- bs
        a <- b.a
      } yield a.s).sorted
      _ <- q1.result.map(_ shouldBe List("a", "a", "b"))
      q2 = (for {
        c <- cs
        a <- c.a
      } yield a.s).sorted
      _ <- q2.result.map(_ shouldBe List("a", "c"))
      q3 = (for {
        b <- bs
        c <- cs
        a <- b.a & c.a
      } yield a.s).sorted
      _ <- q3.result.map(_ shouldBe List("a", "a"))
    } yield ()
  }

  def testManyToMany = {
    class A(tag: Tag) extends Table[(Int, String)](tag, "a3") {
      def id = column[Int]("id", O.PrimaryKey)
      def s = column[String]("s")
      def * = (id, s)
      def bs = aToB.filter(_.aId === id).flatMap(_.bFK)
    }
    lazy val as = TableQuery[A]

    class B(tag: Tag) extends Table[(Int, String)](tag, "b3") {
      def id = column[Int]("id", O.PrimaryKey)
      def s = column[String]("s")
      def * = (id, s)
      def as = aToB.filter(_.bId === id).flatMap(_.aFK)
    }
    lazy val bs = TableQuery[B]

    class AToB(tag: Tag) extends Table[(Int, Int)](tag, "a3_to_b3") {
      def aId = column[Int]("a")
      def bId = column[Int]("b")
      def * = (aId, bId)
      def aFK = foreignKey("a3_fk", aId, as)(a => a.id)
      def bFK = foreignKey("b3_fk", bId, bs)(b => b.id)
    }
    lazy val aToB = TableQuery[AToB]

    seq(
      (as.schema ++ bs.schema ++ aToB.schema).create,
      as ++= Seq(1 -> "a", 2 -> "b", 3 -> "c"),
      bs ++= Seq(1 -> "x", 2 -> "y", 3 -> "z"),
      aToB ++= Seq(1 -> 1, 1 -> 2, 2 -> 2, 2 -> 3),
      { val q1 = (for {
          a <- as if a.id >= 2
          b <- a.bs
        } yield (a.s, b.s)).to[Set]
        q1.result.map(_ shouldBe Set(("b","y"), ("b","z"))) }
    )
  }
}
