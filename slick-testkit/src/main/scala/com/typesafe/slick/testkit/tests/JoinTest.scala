package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{RelationalTestDB, AsyncTest}

class JoinTest extends AsyncTest[RelationalTestDB] {
  import tdb.profile.api._

  @deprecated("Using deprecated join operators", "2.2")
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
      _ <- categories ++= Seq(
        (1, "Scala"),
        (2, "ScalaQuery"),
        (3, "Windows"),
        (4, "Software")
      )
      _ <- posts.map(p => (p.title, p.category)) ++= Seq(
        ("Test Post", -1),
        ("Formal Language Processing in Scala, Part 5", 1),
        ("Efficient Parameterized Queries in ScalaQuery", 2),
        ("Removing Libraries and HomeGroup icons from the Windows 7 desktop", 3),
        ("A ScalaQuery Update", 2)
      )
      // Implicit join
      q1 = (for {
        c <- categories
        p <- posts if c.id === p.category
      } yield (p.id, c.id, c.name, p.title)).sortBy(_._1)
      _ <- q1.map(p => (p._1, p._2)).result.map(_ shouldBe List((2,1), (3,2), (4,3), (5,2)))
      // Explicit inner join
      q2 = (for {
        (c,p) <- categories innerJoin posts on (_.id === _.category)
      } yield (p.id, c.id, c.name, p.title)).sortBy(_._1)
      _ <- q2.map(p => (p._1, p._2)).result.map(_ shouldBe List((2,1), (3,2), (4,3), (5,2)))
      // Left outer join (nulls first)
      q3 = (for {
        (c,p) <- categories leftJoin posts on (_.id === _.category)
      } yield (p.id, (p.id.?.getOrElse(0), c.id, c.name, p.title.?.getOrElse("")))).sortBy(_._1.nullsFirst).map(_._2)
      _ <- q3.map(p => (p._1, p._2)).result.map(_ shouldBe List((0,4), (2,1), (3,2), (4,3), (5,2)))
      // Read NULL from non-nullable column
      q3a = (for {
        (c,p) <- categories leftJoin posts on (_.id === _.category)
      } yield (p.id, c.id, c.name, p.title)).sortBy(_._1.nullsFirst)
      _ <- q3a.result.failed.map(_.shouldBeA[SlickException])
      // Left outer join (nulls last)
      q3b = (for {
        (c,p) <- categories leftJoin posts on (_.id === _.category)
      } yield (p.id, (p.id.?.getOrElse(0), c.id, c.name, p.title.?.getOrElse("")))).sortBy(_._1.nullsLast).map(_._2)
      _ <- q3b.map(p => (p._1, p._2)).result.map(_ shouldBe List((2,1), (3,2), (4,3), (5,2), (0,4)))
      // Right outer join
      q4 = (for {
        (c,p) <- categories rightJoin posts on (_.id === _.category)
      } yield (p.id, c.id.?.getOrElse(0), c.name.?.getOrElse(""), p.title)).sortBy(_._1)
      _ <- q4.map(p => (p._1, p._2)).result.map(_ shouldBe List((1,0), (2,1), (3,2), (4,3), (5,2)))
      // Full outer join
      q5 = (for {
        (c,p) <- categories outerJoin posts on (_.id === _.category)
      } yield (p.id.?.getOrElse(0), c.id.?.getOrElse(0), c.name.?.getOrElse(""), p.title.?.getOrElse(""))).sortBy(_._1)
      _ <- q5.map(p => (p._1, p._2)).result.map(_ shouldBe Vector((0,4), (1,0), (2,1), (3,2), (4,3), (5,2)))
    } yield ()
  }

  def testOptionExtendedJoin = {
    class Data(name: String)(tag: Tag) extends Table[(Int, String)](tag, name) {
      def a = column[Int]("a")
      def b = column[String]("b")
      def * = (a, b)
    }
    val xs = TableQuery(new Data("xs_jo")(_))
    val ys = TableQuery(new Data("ys_jo")(_))

    for {
      _ <- (xs.schema ++ ys.schema).create
      _ <- xs ++= Seq((1, "a"), (2, "b"), (3, "b"), (4, "c"), (5, "c"))
      _ <- ys ++= Seq((1, "a"), (2, "b"), (3, "b"), (4, "d"), (5, "d"))
      // Left outer, lift primitive value
      q1 = (xs.map(_.b) joinLeft ys.map(_.b) on (_ === _)).to[Set]
      r1 <- q1.result
      r1t: Set[(String, Option[String])] = r1
      _ = r1 shouldBe Set(("a",Some("a")), ("b",Some("b")), ("c",None))
      // Nested left outer, lift primitive value
      q2 = ((xs.map(_.b) joinLeft ys.map(_.b) on (_ === _)) joinLeft ys.map(_.b) on (_._1 === _)).to[Set]
      r2 <- q2.result
      r2t: Set[((String, Option[String]), Option[String])] = r2
      _ = r2 shouldBe Set((("a",Some("a")),Some("a")), (("b",Some("b")),Some("b")), (("c",None),None))
      // Left outer, lift non-primitive value
      q3 = (xs joinLeft ys on (_.b === _.b)).to[Set]
      r3 <- q3.result
      r3t: Set[((Int, String), Option[(Int, String)])] = r3
      _ = r3 shouldBe Set(((3,"b"),Some((3,"b"))), ((3,"b"),Some((2,"b"))), ((5,"c"),None), ((1,"a"),Some((1,"a"))), ((4,"c"),None), ((2,"b"),Some((3,"b"))), ((2,"b"),Some((2,"b"))))
      // Left outer, lift non-primitive value, then map to primitive
      q4 = (xs joinLeft ys on (_.b === _.b)).map { case (x, yo) => (x.a, yo.map(_.a)) }.to[Set]
      r4 <- q4.result
      r4t: Set[(Int, Option[Int])] = r4
      _ = r4 shouldBe Set((4,None), (3,Some(2)), (2,Some(3)), (2,Some(2)), (3,Some(3)), (1,Some(1)), (5,None))
      // Nested left outer, lift non-primitive value
      q5 = ((xs joinLeft ys on (_.b === _.b)) joinLeft ys on (_._1.b === _.b)).to[Set]
      r5 <- q5.result
      r5t: Set[(((Int, String), Option[(Int, String)]), Option[(Int, String)])] = r5
      _ = r5 shouldBe Set(
        (((1,"a"),Some((1,"a"))),Some((1,"a"))),
        (((2,"b"),Some((2,"b"))),Some((2,"b"))),
        (((2,"b"),Some((2,"b"))),Some((3,"b"))),
        (((2,"b"),Some((3,"b"))),Some((2,"b"))),
        (((2,"b"),Some((3,"b"))),Some((3,"b"))),
        (((3,"b"),Some((2,"b"))),Some((2,"b"))),
        (((3,"b"),Some((2,"b"))),Some((3,"b"))),
        (((3,"b"),Some((3,"b"))),Some((2,"b"))),
        (((3,"b"),Some((3,"b"))),Some((3,"b"))),
        (((4,"c"),None),None),
        (((5,"c"),None),None)
      )
      // Right outer, lift primitive value
      q6 = (ys.map(_.b) joinRight xs.map(_.b) on (_ === _)).to[Set]
      r6 <- q6.result
      r6t: Set[(Option[String], String)] = r6
      _ = r6 shouldBe Set((Some("a"),"a"), (Some("b"),"b"), (None,"c"))
      // Nested right outer, lift primitive value
      // (left-associative; not symmetrical to the nested left outer case)
      q7 = ((ys.map(_.b) joinRight xs.map(_.b) on (_ === _)) joinRight xs.map(_.b) on (_._2 === _)).to[Set]
      r7 <- q7.result
      rt: Set[(Option[(Option[String], String)], String)] = r7
      _ = r7 shouldBe Set((Some((Some("a"),"a")),"a"), (Some((Some("b"),"b")),"b"), (Some((None,"c")),"c"))
      // Right outer, lift non-primitive value
      q8 = (ys joinRight xs on (_.b === _.b)).to[Set]
      r8 <- q8.result
      r8t: Set[(Option[(Int, String)], (Int, String))] = r8
      _ = r8 shouldBe Set(
        (Some((1,"a")), (1,"a")),
        (Some((2,"b")), (2,"b")),
        (Some((3,"b")), (2,"b")),
        (Some((2,"b")), (3,"b")),
        (Some((3,"b")), (3,"b")),
        (None, (4,"c")),
        (None, (5,"c"))
      )
      // Right outer, lift non-primitive value, then map to primitive
      q9 = (ys joinRight xs on (_.b === _.b)).map { case (yo, x) => (yo.map(_.a), x.a) }.to[Set]
      r9 <- q9.result
      r9t: Set[(Option[Int], Int)] = r9
      _ = r9 shouldBe Set((None,4), (Some(2),3), (Some(3),2), (Some(2),2), (Some(3),3), (Some(1),1), (None,5))
      // Nested right outer, lift non-primitive value
      // (left-associative; not symmetrical to the nested left outer case)
      q10 = ((ys joinRight xs on (_.b === _.b)) joinRight xs on (_._1.map(_.b) === _.b)).to[Set]
      r10 <- q10.result
      r10t: Set[(Option[(Option[(Int, String)], (Int, String))], (Int, String))] = r10
      _ = r10 shouldBe Set(
        (Some((Some((1,"a")),(1,"a"))),(1,"a")),
        (Some((Some((2,"b")),(2,"b"))),(2,"b")),
        (Some((Some((2,"b")),(2,"b"))),(3,"b")),
        (Some((Some((2,"b")),(3,"b"))),(2,"b")),
        (Some((Some((2,"b")),(3,"b"))),(3,"b")),
        (Some((Some((3,"b")),(2,"b"))),(2,"b")),
        (Some((Some((3,"b")),(2,"b"))),(3,"b")),
        (Some((Some((3,"b")),(3,"b"))),(2,"b")),
        (Some((Some((3,"b")),(3,"b"))),(3,"b")),
        (None,(4,"c")),
        (None,(5,"c"))
      )
      // Full outer, lift primitive values
      q11 = (xs.map(_.b) joinFull ys.map(_.b) on (_ === _)).to[Set]
      r11 <- q11.result
      r11t: Set[(Option[String], Option[String])] = r11
      _ = r11 shouldBe Set((Some("a"),Some("a")), (Some("b"),Some("b")), (Some("c"),None), (None,Some("d")))
      // Full outer, lift non-primitive values
      q12 = (xs joinFull ys on (_.b === _.b)).to[Set]
      r12 <- q12.result
      r12t: Set[(Option[(Int, String)], Option[(Int, String)])] = r12
      _ = r12 shouldBe Set(
        (Some((1,"a")),Some((1,"a"))),
        (Some((2,"b")),Some((2,"b"))),
        (Some((2,"b")),Some((3,"b"))),
        (Some((3,"b")),Some((2,"b"))),
        (Some((3,"b")),Some((3,"b"))),
        (Some((4,"c")),None),
        (Some((5,"c")),None),
        (None,Some((4,"d"))),
        (None,Some((5,"d")))
      )
    } yield ()
  }

  def testComputedStarProjection = {
    class X(tag: Tag) extends Table[(Int, Int)](tag, "x_star") {
      def a = column[Int]("a")
      def b = column[Int]("b", O.Default(2))
      def * = (a, b * 10)
    }
    val xs = TableQuery[X]
    for {
      _ <- xs.schema.create
      _ <- xs.map(_.a) ++= Seq(1)
      q1 = xs joinLeft xs
      _ <- q1.result.map(_ shouldBe Vector(((1, 20), Some((1, 20)))))
    } yield ()
  }

  def testZip = ifCap(rcap.zip) {
    class Categories(tag: Tag) extends Table[(Int, String)](tag, "cat_z") {
      def id = column[Int]("id")
      def name = column[String]("name")
      def * = (id, name)
    }
    val categories = TableQuery[Categories]

    class Posts(tag: Tag) extends Table[(Int, String, Int)](tag, "posts_z") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def title = column[String]("title")
      def category = column[Int]("category")
      def * = (id, title, category)
    }
    val posts = TableQuery[Posts]

    for {
      _ <- (categories.schema ++ posts.schema).create
      _ <- categories ++= Seq(
        (1, "Scala"),
        (3, "Windows"),
        (2, "ScalaQuery"),
        (4, "Software")
      )
      _ <- posts.map(p => (p.title, p.category)) ++= Seq(
        ("Test Post", -1),
        ("Formal Language Processing in Scala, Part 5", 1),
        ("Efficient Parameterized Queries in ScalaQuery", 2),
        ("Removing Libraries and HomeGroup icons from the Windows 7 desktop", 3),
        ("A ScalaQuery Update", 2)
      )
      q1 = for {
        (c, i) <- categories.sortBy(_.id).zipWithIndex
      } yield (c.id, i)
      _ <- q1.result.map(_ shouldBe List((1,0), (2,1), (3,2), (4,3)))
      q2 = for {
        (c, p) <- categories.sortBy(_.id) zip posts.sortBy(_.category)
      } yield (c.id, p.category)
      _ <- q2.result.map(_ shouldBe List((1,-1), (2,1), (3,2), (4,2)))
      q3 = for {
        (c, p) <- categories zip posts
      } yield (c.id, p.category)
      _ <- q3.result.map(_ shouldBe List((1, -1), (3, 1), (2, 2), (4, 3)))
      q4 = for {
        res <- categories.zipWith(posts, (c: Categories, p: Posts) => (c.id, p.category))
      } yield res
      _ <- q4.result.map(_ shouldBe List((1, -1), (3, 1), (2, 2), (4, 3)))
      q5 = for {
        (c, i) <- categories.zipWithIndex
      } yield (c.id, i)
      _ <- q5.result.map(_ shouldBe List((1,0), (3,1), (2,2), (4,3)))
      q6 = for {
        ((c, p), i) <- (categories zip posts).zipWithIndex
      } yield (c.id, p.category, i)
      _ <- q6.result.map(_ shouldBe List((1, -1, 0), (3, 1, 1), (2, 2, 2), (4, 3, 3)))
    } yield ()
  }

  def testNoJoinCondition = {
    class T(tag: Tag) extends Table[Int](tag, "t_nojoincondition") {
      def id = column[Int]("id")
      def * = id
    }
    lazy val ts = TableQuery[T]
    for {
      _ <- ts.schema.create
      q1 = ts joinLeft ts
      _ <- q1.result
      q2 = ts joinRight ts
      _ <- q2.result
      q3 = ts join ts
      _ <- q3.result
    } yield ()
  }
}
