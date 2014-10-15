package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{RelationalTestDB, TestkitTest}

class JoinTest extends TestkitTest[RelationalTestDB] {
  import tdb.profile.simple._

  override val reuseInstance = true

  @deprecated("Using deprecated join operators", "2.2")
  def testJoin {
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

    (categories.ddl ++ posts.ddl).create

    categories ++= Seq(
      (1, "Scala"),
      (2, "ScalaQuery"),
      (3, "Windows"),
      (4, "Software")
    )
    posts.map(p => (p.title, p.category)) ++= Seq(
      ("Test Post", -1),
      ("Formal Language Processing in Scala, Part 5", 1),
      ("Efficient Parameterized Queries in ScalaQuery", 2),
      ("Removing Libraries and HomeGroup icons from the Windows 7 desktop", 3),
      ("A ScalaQuery Update", 2)
    )

    val q1 = (for {
      c <- categories
      p <- posts if c.id === p.category
    } yield (p.id, c.id, c.name, p.title)).sortBy(_._1)
    println("Implicit join")
    q1.run.foreach(x => println("  "+x))
    assertEquals(List((2,1), (3,2), (4,3), (5,2)), q1.map(p => (p._1, p._2)).run)

    val q2 = (for {
      (c,p) <- categories innerJoin posts on (_.id === _.category)
    } yield (p.id, c.id, c.name, p.title)).sortBy(_._1)
    println("Explicit inner join")
    q2.run.foreach(x => println("  "+x))
    assertEquals(List((2,1), (3,2), (4,3), (5,2)), q2.map(p => (p._1, p._2)).run)

    val q3 = (for {
      (c,p) <- categories leftJoin posts on (_.id === _.category)
    } yield (p.id, (p.id.?.getOrElse(0), c.id, c.name, p.title.?.getOrElse("")))).sortBy(_._1.nullsFirst).map(_._2)
    println("Left outer join (nulls first)")
    q3.run.foreach(x => println("  "+x))
    assertEquals(List((0,4), (2,1), (3,2), (4,3), (5,2)), q3.map(p => (p._1, p._2)).run)

    val q3a = (for {
      (c,p) <- categories leftJoin posts on (_.id === _.category)
    } yield (p.id, c.id, c.name, p.title)).sortBy(_._1.nullsFirst)
    assertFail(println("q3a result: " + q3a.run)) // reads NULL from non-nullable column

    val q3b = (for {
      (c,p) <- categories leftJoin posts on (_.id === _.category)
    } yield (p.id, (p.id.?.getOrElse(0), c.id, c.name, p.title.?.getOrElse("")))).sortBy(_._1.nullsLast).map(_._2)
    println("Left outer join (nulls last)")
    q3b.run.foreach(x => println("  "+x))
    assertEquals(List((2,1), (3,2), (4,3), (5,2), (0,4)), q3b.map(p => (p._1, p._2)).run)

    val q4 = (for {
      (c,p) <- categories rightJoin posts on (_.id === _.category)
    } yield (p.id, c.id.?.getOrElse(0), c.name.?.getOrElse(""), p.title)).sortBy(_._1)
    println("Right outer join")
    q4.run.foreach(x => println("  "+x))
    assertEquals(List((1,0), (2,1), (3,2), (4,3), (5,2)), q4.map(p => (p._1, p._2)).run)

    val q5 = (for {
      (c,p) <- categories outerJoin posts on (_.id === _.category)
    } yield (p.id.?.getOrElse(0), c.id.?.getOrElse(0), c.name.?.getOrElse(""), p.title.?.getOrElse(""))).sortBy(_._1)
    println("Full outer join")
    q5.run.foreach(x => println("  "+x))
    assertEquals(Vector((0,4), (1,0), (2,1), (3,2), (4,3), (5,2)), q5.map(p => (p._1, p._2)).run)
  }

  def testOptionExtendedJoin {
    class Data(name: String)(tag: Tag) extends Table[(Int, String)](tag, name) {
      def a = column[Int]("a")
      def b = column[String]("b")
      def * = (a, b)
    }
    val xs = TableQuery(new Data("xs_jo")(_))
    val ys = TableQuery(new Data("ys_jo")(_))

    (xs.ddl ++ ys.ddl).create

    xs ++= Seq((1, "a"), (2, "b"), (3, "b"), (4, "c"), (5, "c"))
    ys ++= Seq((1, "a"), (2, "b"), (3, "b"), (4, "d"), (5, "d"))

    // Left outer, lift primitive value
    val q1 = (xs.map(_.b) joinLeft ys.map(_.b) on (_ === _)).to[Set]
    val r1 = q1.run
    val r1t: Set[(String, Option[String])] = r1
    assertEquals(Set(("a",Some("a")), ("b",Some("b")), ("c",None)), r1)

    // Nested left outer, lift primitive value
    val q2 = ((xs.map(_.b) joinLeft ys.map(_.b) on (_ === _)) joinLeft ys.map(_.b) on (_._1 === _)).to[Set]
    val r2 = q2.run
    val r2t: Set[((String, Option[String]), Option[String])] = r2
    assertEquals(Set((("a",Some("a")),Some("a")), (("b",Some("b")),Some("b")), (("c",None),None)), r2)

    // Left outer, lift non-primitive value
    val q3 = (xs joinLeft ys on (_.b === _.b)).to[Set]
    val r3 = q3.run
    val r3t: Set[((Int, String), Option[(Int, String)])] = r3
    assertEquals(Set(((3,"b"),Some((3,"b"))), ((3,"b"),Some((2,"b"))), ((5,"c"),None), ((1,"a"),Some((1,"a"))), ((4,"c"),None), ((2,"b"),Some((3,"b"))), ((2,"b"),Some((2,"b")))), r3)

    // Left outer, lift non-primitive value, then map to primitive
    val q4 = (xs joinLeft ys on (_.b === _.b)).map { case (x, yo) => (x.a, yo.map(_.a)) }.to[Set]
    val r4 = q4.run
    val r4t: Set[(Int, Option[Int])] = r4
    assertEquals(Set((4,None), (3,Some(2)), (2,Some(3)), (2,Some(2)), (3,Some(3)), (1,Some(1)), (5,None)), r4)

    // Nested left outer, lift non-primitive value
    val q5 = ((xs joinLeft ys on (_.b === _.b)) joinLeft ys on (_._1.b === _.b)).to[Set]
    val r5 = q5.run
    val r5t: Set[(((Int, String), Option[(Int, String)]), Option[(Int, String)])] = r5
    assertEquals(Set(
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
    ), r5)

    // Right outer, lift primitive value
    val q6 = (ys.map(_.b) joinRight xs.map(_.b) on (_ === _)).to[Set]
    val r6 = q6.run
    val r6t: Set[(Option[String], String)] = r6
    assertEquals(Set((Some("a"),"a"), (Some("b"),"b"), (None,"c")), r6)

    // Nested right outer, lift primitive value
    // (left-associative; not symmetrical to the nested left outer case)
    val q7 = ((ys.map(_.b) joinRight xs.map(_.b) on (_ === _)) joinRight xs.map(_.b) on (_._2 === _)).to[Set]
    val r7 = q7.run
    val rt: Set[(Option[(Option[String], String)], String)] = r7
    assertEquals(Set((Some((Some("a"),"a")),"a"), (Some((Some("b"),"b")),"b"), (Some((None,"c")),"c")), r7)

    // Right outer, lift non-primitive value
    val q8 = (ys joinRight xs on (_.b === _.b)).to[Set]
    val r8 = q8.run
    val r8t: Set[(Option[(Int, String)], (Int, String))] = r8
    assertEquals(Set(
      (Some((1,"a")), (1,"a")),
      (Some((2,"b")), (2,"b")),
      (Some((3,"b")), (2,"b")),
      (Some((2,"b")), (3,"b")),
      (Some((3,"b")), (3,"b")),
      (None, (4,"c")),
      (None, (5,"c"))
    ), r8)

    // Right outer, lift non-primitive value, then map to primitive
    val q9 = (ys joinRight xs on (_.b === _.b)).map { case (yo, x) => (yo.map(_.a), x.a) }.to[Set]
    val r9 = q9.run
    val r9t: Set[(Option[Int], Int)] = r9
    assertEquals(Set((None,4), (Some(2),3), (Some(3),2), (Some(2),2), (Some(3),3), (Some(1),1), (None,5)), r9)

    // Nested right outer, lift non-primitive value
    // (left-associative; not symmetrical to the nested left outer case)
    val q10 = ((ys joinRight xs on (_.b === _.b)) joinRight xs on (_._1.map(_.b) === _.b)).to[Set]
    val r10 = q10.run
    val r10t: Set[(Option[(Option[(Int, String)], (Int, String))], (Int, String))] = r10
    assertEquals(Set(
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
    ), r10)

    // Full outer, lift primitive values
    val q11 = (xs.map(_.b) joinFull ys.map(_.b) on (_ === _)).to[Set]
    val r11 = q11.run
    val r11t: Set[(Option[String], Option[String])] = r11
    assertEquals(Set((Some("a"),Some("a")), (Some("b"),Some("b")), (Some("c"),None), (None,Some("d"))), r11)

    // Full outer, lift non-primitive values
    val q12 = (xs joinFull ys on (_.b === _.b)).to[Set]
    val r12 = q12.run
    val r12t: Set[(Option[(Int, String)], Option[(Int, String)])] = r12
    assertEquals(Set(
      (Some((1,"a")),Some((1,"a"))),
      (Some((2,"b")),Some((2,"b"))),
      (Some((2,"b")),Some((3,"b"))),
      (Some((3,"b")),Some((2,"b"))),
      (Some((3,"b")),Some((3,"b"))),
      (Some((4,"c")),None),
      (Some((5,"c")),None),
      (None,Some((4,"d"))),
      (None,Some((5,"d")))
    ), r12)
  }

  def testComputedStarProjection {
    class X(tag: Tag) extends Table[(Int, Int)](tag, "x_star") {
      def a = column[Int]("a")
      def b = column[Int]("b", O.Default(2))
      def * = (a, b * 10)
    }
    val xs = TableQuery[X]
    xs.ddl.create
    xs.map(_.a) ++= Seq(1)

    val q1 = xs joinLeft xs
    assertEquals(Vector(((1, 20), Some((1, 20)))), q1.run)
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

    (categories.ddl ++ posts.ddl).create

    categories ++= Seq(
      (1, "Scala"),
      (3, "Windows"),
      (2, "ScalaQuery"),
      (4, "Software")
    )
    posts.map(p => (p.title, p.category)) ++= Seq(
      ("Test Post", -1),
      ("Formal Language Processing in Scala, Part 5", 1),
      ("Efficient Parameterized Queries in ScalaQuery", 2),
      ("Removing Libraries and HomeGroup icons from the Windows 7 desktop", 3),
      ("A ScalaQuery Update", 2)
    )

    val q1 = for {
      (c, i) <- categories.sortBy(_.id).zipWithIndex
    } yield (c.id, i)
    assertEquals(List((1,0), (2,1), (3,2), (4,3)), q1.run)

    val q2 = for {
      (c, p) <- categories.sortBy(_.id) zip posts.sortBy(_.category)
    } yield (c.id, p.category)
    assertEquals(List((1,-1), (2,1), (3,2), (4,2)), q2.run)

    val q3 = for {
      (c, p) <- categories zip posts
    } yield (c.id, p.category)
    assertEquals(List((1, -1), (3, 1), (2, 2), (4, 3)), q3.run)

    val q4 = for {
      res <- categories.zipWith(posts, (c: Categories, p: Posts) => (c.id, p.category))
    } yield res
    assertEquals(List((1, -1), (3, 1), (2, 2), (4, 3)), q4.run)

    val q5 = for {
      (c, i) <- categories.zipWithIndex
    } yield (c.id, i)
    assertEquals(List((1,0), (3,1), (2,2), (4,3)), q5.run)

    val q6 = for {
      ((c, p), i) <- (categories zip posts).zipWithIndex
    } yield (c.id, p.category, i)
    assertEquals(List((1, -1, 0), (3, 1, 1), (2, 2, 2), (4, 3, 3)), q6.run)
  }

  def testNoJoinCondition {
    class T(tag: Tag) extends Table[Int](tag, "t_nojoincondition") {
      def id = column[Int]("id")
      def * = id
    }
    lazy val ts = TableQuery[T]
    ts.ddl.create
    val q1 = ts joinLeft ts
    q1.run
    val q2 = ts joinRight ts
    q2.run
    val q3 = ts join ts
    q3.run
  }
}
