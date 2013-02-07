package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{TestkitTest, TestDB}

class JoinTest(val tdb: TestDB) extends TestkitTest {
  import tdb.profile.simple._

  override val reuseInstance = true

  def testJoin {
    object Categories extends Table[(Int, String)]("cat_j") {
      def id = column[Int]("id")
      def name = column[String]("name")
      def * = id ~ name
    }

    object Posts extends Table[(Int, String, Int)]("posts_j") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def title = column[String]("title")
      def category = column[Int]("category")
      def * = id ~ title ~ category
    }

    (Categories.ddl ++ Posts.ddl).create

    Categories insertAll (
      (1, "Scala"),
      (2, "ScalaQuery"),
      (3, "Windows"),
      (4, "Software")
    )
    Posts.title ~ Posts.category insertAll (
      ("Test Post", -1),
      ("Formal Language Processing in Scala, Part 5", 1),
      ("Efficient Parameterized Queries in ScalaQuery", 2),
      ("Removing Libraries and HomeGroup icons from the Windows 7 desktop", 3),
      ("A ScalaQuery Update", 2)
    )

    val q1 = (for {
      c <- Categories
      p <- Posts if c.id is p.category
    } yield p.id ~ c.id ~ c.name ~ p.title).sortBy(_._1)
    println("Implicit join: "+q1.selectStatement)
    q1.foreach(x => println("  "+x))
    assertEquals(List((2,1), (3,2), (4,3), (5,2)), q1.map(p => p._1 ~ p._2).list)

    val q2 = (for {
      (c,p) <- Categories innerJoin Posts on (_.id is _.category)
    } yield p.id ~ c.id ~ c.name ~ p.title).sortBy(_._1)
    println("Explicit inner join: "+q2.selectStatement)
    q2.foreach(x => println("  "+x))
    assertEquals(List((2,1), (3,2), (4,3), (5,2)), q2.map(p => p._1 ~ p._2).list)

    val q3 = (for {
      (c,p) <- Categories leftJoin Posts on (_.id is _.category)
    } yield (p.id, p.id.?.getOrElse(0) ~ c.id ~ c.name ~ p.title.?.getOrElse(""))).sortBy(_._1.nullsFirst).map(_._2)
    println("Left outer join (nulls first): "+q3.selectStatement)
    q3.foreach(x => println("  "+x))
    assertEquals(List((0,4), (2,1), (3,2), (4,3), (5,2)), q3.map(p => p._1 ~ p._2).list)

    val q3a = (for {
      (c,p) <- Categories leftJoin Posts on (_.id is _.category)
    } yield p.id ~ c.id ~ c.name ~ p.title).sortBy(_._1.nullsFirst)
    assertFail(println("q3a result: " + q3a.list)) // reads NULL from non-nullable column

    val q3b = (for {
      (c,p) <- Categories leftJoin Posts on (_.id is _.category)
    } yield (p.id, p.id.?.getOrElse(0) ~ c.id ~ c.name ~ p.title.?.getOrElse(""))).sortBy(_._1.nullsLast).map(_._2)
    println("Left outer join (nulls last): "+q3b.selectStatement)
    q3b.foreach(x => println("  "+x))
    assertEquals(List((2,1), (3,2), (4,3), (5,2), (0,4)), q3b.map(p => p._1 ~ p._2).list)

    ifCap(rcap.joinRight) {
      val q4 = (for {
        (c,p) <- Categories rightJoin Posts on (_.id is _.category)
      } yield p.id ~ c.id.?.getOrElse(0) ~ c.name.?.getOrElse("") ~ p.title).sortBy(_._1)
      println("Right outer join: "+q4.selectStatement)
      q4.foreach(x => println("  "+x))
      assertEquals(List((1,0), (2,1), (3,2), (4,3), (5,2)), q4.map(p => p._1 ~ p._2).list)
    }
  }

  def testZip = ifCap(rcap.zip) {
    object Categories extends Table[(Int, String)]("cat_z") {
      def id = column[Int]("id")
      def name = column[String]("name")
      def * = id ~ name
    }

    object Posts extends Table[(Int, String, Int)]("posts_z") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def title = column[String]("title")
      def category = column[Int]("category")
      def * = id ~ title ~ category
    }

    (Categories.ddl ++ Posts.ddl).create

    Categories insertAll (
      (1, "Scala"),
      (3, "Windows"),
      (2, "ScalaQuery"),
      (4, "Software")
    )
    Posts.title ~ Posts.category insertAll (
      ("Test Post", -1),
      ("Formal Language Processing in Scala, Part 5", 1),
      ("Efficient Parameterized Queries in ScalaQuery", 2),
      ("Removing Libraries and HomeGroup icons from the Windows 7 desktop", 3),
      ("A ScalaQuery Update", 2)
    )

    val q1 = for {
      (c, i) <- Categories.sortBy(_.id).zipWithIndex
    } yield (c.id, i)
    println("ZipWithIndex: "+q1.selectStatement)
    q1.foreach(x => println("  "+x))
    assertEquals(List((1,0), (2,1), (3,2), (4,3)), q1.list)

    val q2 = for {
      (c, p) <- Categories.sortBy(_.id) zip Posts.sortBy(_.category)
    } yield (c.id, p.category)
    println("Zip: "+q2.selectStatement)
    q2.foreach(x => println("  "+x))
    assertEquals(List((1,-1), (2,1), (3,2), (4,2)), q2.list)
  }
}
