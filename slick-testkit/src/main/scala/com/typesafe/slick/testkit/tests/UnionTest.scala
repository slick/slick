package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{RelationalTestDB, TestkitTest}

class UnionTest extends TestkitTest[RelationalTestDB] {
  import tdb.profile.simple._
  override val reuseInstance = true

  object Managers extends Table[(Int, String, String)]("managers") {
    def id = column[Int]("id")
    def name = column[String]("name")
    def department = column[String]("department")
    def * = (id, name, department)
  }

  object Employees extends Table[(Int, String, Int)]("employees") {
    def id = column[Int]("id")
    def name = column[String]("name2")
    def manager = column[Int]("manager")
    def * = (id, name, manager)

    // A convenience method for selecting employees by department
    def departmentIs(dept: String) = manager in Managers.where(_.department is dept).map(_.id)
  }

  def testBasic {
    (Managers.ddl ++ Employees.ddl).create

    Managers ++= Seq(
      (1, "Peter", "HR"),
      (2, "Amy", "IT"),
      (3, "Steve", "IT")
    )

    Employees ++= Seq(
      (4, "Jennifer", 1),
      (5, "Tom", 1),
      (6, "Leonard", 2),
      (7, "Ben", 2),
      (8, "Greg", 3)
    )

    val q1 = for(m <- Managers where { _.department is "IT" }) yield (m.id, m.name)
    println("Managers in IT")
    q1.run.foreach(o => println("  "+o))

    val q2 = for(e <- Employees where { _.departmentIs("IT") }) yield (e.id, e.name)
    println("Employees in IT")
    q2.run.foreach(o => println("  "+o))

    val q3 = (q1 union q2).sortBy(_._2.asc)
    println("Combined and sorted")
    q3.run.foreach(o => println("  "+o))

    assertEquals(List((2,"Amy"), (7,"Ben"), (8,"Greg"), (6,"Leonard"), (3,"Steve")), q3.run)

    (Managers.ddl ++ Employees.ddl).drop
  }

  def testUnionWithoutProjection {
    Managers.ddl.create
    Managers ++= Seq(
      (1, "Peter", "HR"),
      (2, "Amy", "IT"),
      (3, "Steve", "IT")
    )

    def f (s: String) = Managers where { _.name === s}
    val q = f("Peter") union f("Amy")
    assertEquals(Set((1, "Peter", "HR"), (2, "Amy", "IT")), q.run.toSet)

    Managers.ddl.drop
  }

  def testUnionOfJoins {
    abstract class Drinks(tableName: String) extends Table[(Long, Long)](tableName) {
      def pk = column[Long]("pk")
      def pkCup = column[Long]("pkCup")
      def * = (pk, pkCup)
    }
    object Coffees extends Drinks("Coffee")
    object Teas extends Drinks("Tea")

    (Coffees.ddl ++ Teas.ddl).create
    Coffees ++= Seq(
      (10L, 1L),
      (20L, 2L),
      (30L, 3L)
    )
    Teas ++= Seq(
      (100L, 1L),
      (200L, 2L),
      (300L, 3L)
    )

    val q1 = for {
      coffee <- Coffees
      tea <- Teas if coffee.pkCup === tea.pkCup
    } yield (coffee.pk, coffee.pkCup)
    val q2 = for {
      coffee <- Coffees
      tea <- Teas if coffee.pkCup === tea.pkCup
    } yield (tea.pk, tea.pkCup)
    val q3 = q1 union q2

    val r1 = q1.run.toSet
    assertEquals(Set((10L, 1L), (20L, 2L), (30L, 3L)), r1)
    val r2 = q2.run.toSet
    assertEquals(Set((100L, 1L), (200L, 2L), (300L, 3L)), r2)
    val r3 = q3.run.toSet
    assertEquals(Set((10L, 1L), (20L, 2L), (30L, 3L), (100L, 1L), (200L, 2L), (300L, 3L)), r3)
  }
}
