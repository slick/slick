package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{JdbcTestDB, RelationalTestDB, TestkitTest}

class JoinTest extends TestkitTest[JdbcTestDB] {
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

    val q3 = for {
      (c, p) <- categories zip posts
    } yield (c.id, p.category)
    q3.run.foreach(x => println("  "+x))
    assertEquals(List((1, -1), (3, 1), (2, 2), (4, 3)), q3.run)

    val q4 = for {
      res <- categories.zipWith(posts, (c: Categories, p: Posts) => (c.id, p.category))
    } yield res
    q4.run.foreach(x => println("  "+x))
    assertEquals(List((1, -1), (3, 1), (2, 2), (4, 3)), q4.run)

    val q5 = for {
      (c, i) <- categories.zipWithIndex
    } yield (c.id, i)
    q5.run.foreach(x => println("  "+x))
    assertEquals(List((1,0), (3,1), (2,2), (4,3)), q5.run)

    val q6 = for {
      ((c, p), i) <- (categories zip posts).zipWithIndex
    } yield (c.id, p.category, i)
    q6.run.foreach(x => println("  "+x))
    assertEquals(List((1, -1, 0), (3, 1, 1), (2, 2, 2), (4, 3, 3)), q6.run)
  }

  def testNoJoinCondition {
    class T(tag: Tag) extends Table[Int](tag, "t_nojoincondition") {
      def id = column[Int]("id")
      def * = id
    }
    lazy val ts = TableQuery[T]
    ts.ddl.create
    val q1 = ts leftJoin ts
    q1.run
    val q2 = ts rightJoin ts
    q2.run
    val q3 = ts innerJoin ts
    q3.run
  }

  def testNoParenthesesJoin {
    class Employees(tag: Tag) extends Table[(Int, String, Int)](tag, "emp_noparenthesesjoin") {
      def id = column[Int]("id")
      def name = column[String]("name2")
      def manager = column[Int]("manager")
      def * = (id, name, manager)
    }

    lazy val employees = TableQuery[Employees]
    (employees.ddl).create
    val resultString = employees.
      join(employees).on(_.id === _.id).
      join(employees).on(_._2.id === _.manager).
      map(t => (t._1._1.id, t._1._2.id, t._2.id)).
      filter(t => t._1 < 10).selectStatement

    assertEquals(-1, resultString.indexOf('('))
    assertEquals(-1, resultString.indexOf(')'))

    (employees.ddl).drop
  }
}
