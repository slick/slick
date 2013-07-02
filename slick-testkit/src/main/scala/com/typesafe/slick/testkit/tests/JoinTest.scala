package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{RelationalTestDB, TestkitTest}

class JoinTest extends TestkitTest[RelationalTestDB] {
  import tdb.profile.simple._

  override val reuseInstance = true

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
      p <- posts if c.id is p.category
    } yield (p.id, c.id, c.name, p.title)).sortBy(_._1)
    println("Implicit join")
    q1.run.foreach(x => println("  "+x))
    assertEquals(List((2,1), (3,2), (4,3), (5,2)), q1.map(p => (p._1, p._2)).run)

    val q2 = (for {
      (c,p) <- categories innerJoin posts on (_.id is _.category)
    } yield (p.id, c.id, c.name, p.title)).sortBy(_._1)
    println("Explicit inner join")
    q2.run.foreach(x => println("  "+x))
    assertEquals(List((2,1), (3,2), (4,3), (5,2)), q2.map(p => (p._1, p._2)).run)

    val q3 = (for {
      (c,p) <- categories leftJoin posts on (_.id is _.category)
    } yield (p.id, (p.id.?.getOrElse(0), c.id, c.name, p.title.?.getOrElse("")))).sortBy(_._1.nullsFirst).map(_._2)
    println("Left outer join (nulls first)")
    q3.run.foreach(x => println("  "+x))
    assertEquals(List((0,4), (2,1), (3,2), (4,3), (5,2)), q3.map(p => (p._1, p._2)).run)

    val q3a = (for {
      (c,p) <- categories leftJoin posts on (_.id is _.category)
    } yield (p.id, c.id, c.name, p.title)).sortBy(_._1.nullsFirst)
    assertFail(println("q3a result: " + q3a.run)) // reads NULL from non-nullable column

    val q3b = (for {
      (c,p) <- categories leftJoin posts on (_.id is _.category)
    } yield (p.id, (p.id.?.getOrElse(0), c.id, c.name, p.title.?.getOrElse("")))).sortBy(_._1.nullsLast).map(_._2)
    println("Left outer join (nulls last)")
    q3b.run.foreach(x => println("  "+x))
    assertEquals(List((2,1), (3,2), (4,3), (5,2), (0,4)), q3b.map(p => (p._1, p._2)).run)

    ifCap(rcap.joinRight) {
      val q4 = (for {
        (c,p) <- categories rightJoin posts on (_.id is _.category)
      } yield (p.id, c.id.?.getOrElse(0), c.name.?.getOrElse(""), p.title)).sortBy(_._1)
      println("Right outer join")
      q4.run.foreach(x => println("  "+x))
      assertEquals(List((1,0), (2,1), (3,2), (4,3), (5,2)), q4.map(p => (p._1, p._2)).run)
    }
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
    q1.run.foreach(x => println("  "+x))
    assertEquals(List((1,0), (2,1), (3,2), (4,3)), q1.run)

    val q2 = for {
      (c, p) <- categories.sortBy(_.id) zip posts.sortBy(_.category)
    } yield (c.id, p.category)
    q2.run.foreach(x => println("  "+x))
    assertEquals(List((1,-1), (2,1), (3,2), (4,2)), q2.run)
  }
}
