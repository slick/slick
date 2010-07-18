package com.novocode.squery.test

import org.junit.Test
import org.junit.Assert._
import com.novocode.squery.combinator._
import com.novocode.squery.combinator.TypeMapper._
import com.novocode.squery.combinator.extended.H2Driver.Implicit._
import com.novocode.squery.combinator.extended.{ExtendedTable => Table}
import com.novocode.squery.session._
import com.novocode.squery.session.Database.threadLocalSession

object JoinTest { def main(args: Array[String]) = new JoinTest().test() }

class JoinTest {

  object Categories extends Table[(Int, String)]("categories") {
    def id = column[Int]("id")
    def name = column[String]("name")
    def * = id ~ name
  }

  object Posts extends Table[(Int, String, Int)]("posts") {
    def id = column[Int]("id", O AutoInc)
    def title = column[String]("title")
    def category = column[Int]("category")
    def * = id ~ title ~ category
  }

  @Test def test(): Unit = Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver") withSession {

    (Categories.ddl ++ Posts.ddl) create

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

    val q1 = for {
      c <- Categories
      p <- Posts if c.id is p.category
      _ <- Query orderBy p.id
    } yield p.id ~ c.id ~ c.name ~ p.title
    println("Implicit join: "+q1.selectStatement)
    q1.foreach(x => println("  "+x))
    assertEquals(List((2,1), (3,2), (4,3), (5,2)), q1.map(p => p._1 ~ p._2).list)

    val q2 = for {
      Join(c,p) <- Categories innerJoin Posts on (_.id is _.category)
      _ <- Query orderBy p.id
    } yield p.id ~ c.id ~ c.name ~ p.title
    println("Explicit inner join: "+q2.selectStatement)
    q2.foreach(x => println("  "+x))
    assertEquals(List((2,1), (3,2), (4,3), (5,2)), q2.map(p => p._1 ~ p._2).list)

    val q3 = for {
      Join(c,p) <- Categories leftJoin Posts on (_.id is _.category)
      _ <- Query orderBy p.id
    } yield p.id ~ c.id ~ c.name ~ p.title
    println("Left outer join: "+q3.selectStatement)
    q3.foreach(x => println("  "+x))
    assertEquals(List((0,4), (2,1), (3,2), (4,3), (5,2)), q3.map(p => p._1 ~ p._2).list)

    val q4 = for {
      Join(c,p) <- Categories rightJoin Posts on (_.id is _.category)
      _ <- Query orderBy p.id
    } yield p.id ~ c.id ~ c.name ~ p.title
    println("Right outer join: "+q4.selectStatement)
    q4.foreach(x => println("  "+x))
    assertEquals(List((1,0), (2,1), (3,2), (4,3), (5,2)), q4.map(p => p._1 ~ p._2).list)
  }
}
