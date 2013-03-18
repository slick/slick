package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import scala.slick.lifted._
import scala.slick.ast.Dump
import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}

class ForeignKeyTest extends TestkitTest[JdbcTestDB] {
  import tdb.profile.Table
  import tdb.profile.Implicit._

  override val reuseInstance = true

  def testSimple {

    object Categories extends Table[(Int, String)]("categories") {
      def id = column[Int]("id", O.PrimaryKey)
      def name = column[String]("name")
      def * = id ~ name
    }

    object Posts extends Table[(Int, String, Int)]("posts") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def title = column[String]("title")
      def category = column[Int]("category", O.Nullable)
      def * = id ~ title ~ category
      def categoryFK = foreignKey("category_fk", category, Categories)(_.id)
      def categoryJoin = Categories.where(_.id === category)
    }

    tdb.assertNotTablesExist("categories", "posts")
    val ddl = Posts.ddl ++ Categories.ddl
    ddl.createStatements foreach println
    ddl.create
    tdb.assertTablesExist("categories", "posts")

    Categories insertAll (
      (1, "Scala"),
      (2, "ScalaQuery"),
      (3, "Windows"),
      (4, "Software")
    )
    Posts.title ~ Posts.category.? insertAll (
      ("Test Post", None),
      ("Formal Language Processing in Scala, Part 5", Some(1)),
      ("Efficient Parameterized Queries in ScalaQuery", Some(2)),
      ("Removing Libraries and HomeGroup icons from the Windows 7 desktop", Some(3)),
      ("A ScalaQuery Update", Some(2))
    )

    val q1 = (for {
      p <- Posts
      c <- p.categoryJoin
    } yield (p.id, c.id, c.name, p.title)).sortBy(_._1)
    Dump(q1, "Manual join: ")
    println("Manual join: "+q1.selectStatement)
    q1.foreach(x => println("  "+x))
    assertEquals(List((2,1), (3,2), (4,3), (5,2)), q1.map(p => (p._1, p._2)).list)

    val q2 = (for {
      p <- Posts
      c <- p.categoryFK
    } yield (p.id, c.id, c.name, p.title)).sortBy(_._1)
    Dump(q2, "Foreign-key join: ")
    println("Foreign-key join: "+q2.selectStatement)
    q2.foreach(x => println("  "+x))
    assertEquals(List((2,1), (3,2), (4,3), (5,2)), q2.map(p => (p._1, p._2)).list)

    val ddl2 = Categories.ddl ++ Posts.ddl
    ddl2.dropStatements foreach println
    ddl2.drop
    tdb.assertNotTablesExist("categories", "posts")
  }

  def testMultiColumn {

    object A extends Table[(Int, Int, String)]("a") {
      def k1 = column[Int]("k1")
      def k2 = column[Int]("k2")
      def s = column[String]("s")
      def * = k1 ~ k2 ~ s
      def bFK = foreignKey("b_fk", (k1, k2), B)(b => (b.f1, b.f2), onDelete = ForeignKeyAction.Cascade)
    }

    object B extends Table[(Int, Int, String)]("b") {
      def f1 = column[Int]("f1")
      def f2 = column[Int]("f2")
      def s = column[String]("s")
      def * = f1 ~ f2 ~ s
      def bIdx1 = index("b_idx1", (f1, f2), unique = true)
    }

    A.foreignKeys.foreach(println)
    assertEquals(Set("b_fk"), A.foreignKeys.map(_.name).toSet)

    val ddl = A.ddl ++ B.ddl
    ddl.createStatements foreach println
    ddl.create

    B insertAll (
      (1, 2, "b12"),
      (3, 4, "b34"),
      (5, 6, "b56")
    )
    A insertAll (
      (1, 2, "a12"),
      (3, 4, "a34")
    )

    val q1 = for {
      a <- A
      b <- a.bFK
    } yield (a.s, b.s)
    println("Multiple rows: "+q1.selectStatement)
    q1.foreach(x => println("  "+x))
    assertEquals(Set(("a12","b12"), ("a34","b34")), q1.list.toSet)
  }

  def testCombinedJoin {

    object A extends Table[(Int, String)]("a2") {
      def id = column[Int]("id", O.PrimaryKey)
      def s = column[String]("s")
      def * = id ~ s
    }

    class Dep(n: String) extends Table[(Int, Int)](n) {
      def id = column[Int]("id", O.PrimaryKey)
      def aRef = column[Int]("aRef")
      def * = id ~ aRef
      def a = foreignKey(n+"_a2_fk", aRef, A)(_.id)
    }

    val B = new Dep("b2")
    val C = new Dep("c2")

    (A.ddl ++ B.ddl ++ C.ddl).create
    A.insertAll((1, "a"), (2, "b"), (3, "c"), (4, "d"))
    B.insertAll((1, 1), (2, 1), (3, 2))
    C.insertAll((1, 1), (2, 3))

    val q1 = (for {
      b <- B
      a <- b.a
    } yield a.s).sorted
    println("q1: " + q1.selectStatement)
    println("    " + q1.list)
    assertEquals(List("a", "a", "b"), q1.list)

    val q2 = (for {
      c <- C
      a <- c.a
    } yield a.s).sorted
    println("q2: " + q2.selectStatement)
    println("    " + q2.list)
    assertEquals(List("a", "c"), q2.list)

    val q3 = (for {
      b <- B
      c <- C
      a <- b.a & c.a
    } yield a.s).sorted
    println("q3: " + q3.selectStatement)
    println("    " + q3.list)
    assertEquals(List("a", "a"), q3.list)
  }

  def testManyToMany {

    object A extends Table[(Int, String)]("a3") {
      def id = column[Int]("id", O.PrimaryKey)
      def s = column[String]("s")
      def * = id ~ s
      def bs = AToB.filter(_.aId === id).flatMap(_.bFK)
    }

    object B extends Table[(Int, String)]("b3") {
      def id = column[Int]("id", O.PrimaryKey)
      def s = column[String]("s")
      def * = id ~ s
      def as = AToB.filter(_.bId === id).flatMap(_.aFK)
    }

    object AToB extends Table[(Int, Int)]("a3_to_b3") {
      def aId = column[Int]("a")
      def bId = column[Int]("b")
      def * = aId ~ bId
      def aFK = foreignKey("a3_fk", aId, A)(a => a.id)
      def bFK = foreignKey("b3_fk", bId, B)(b => b.id)
    }

    (A.ddl ++ B.ddl ++ AToB.ddl).create
    A.insertAll(1 -> "a", 2 -> "b", 3 -> "c")
    B.insertAll(1 -> "x", 2 -> "y", 3 -> "z")
    AToB.insertAll(1 -> 1, 1 -> 2, 2 -> 2, 2 -> 3)

    /*val q1 = for {
      a <- A if a.id >= 2
      aToB <- AToB if aToB.aId === a.id
      b <- B if b.id === aToB.bId
    } yield (a.s, b.s)*/
    val q1 = for {
      a <- A if a.id >= 2
      b <- a.bs
    } yield (a.s, b.s)
    q1.foreach(x => println("  "+x))
    assertEquals(Set(("b","y"), ("b","z")), q1.list.toSet)
  }
}
