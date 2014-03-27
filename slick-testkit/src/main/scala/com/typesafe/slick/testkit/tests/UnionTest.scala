package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{RelationalTestDB, TestkitTest}

class UnionTest extends TestkitTest[RelationalTestDB] {
  import tdb.profile.simple._
  override val reuseInstance = true

  class Managers(tag: Tag) extends Table[(Int, String, String)](tag, "managers") {
    def id = column[Int]("id")
    def name = column[String]("name")
    def department = column[String]("department")
    def * = (id, name, department)
  }
  lazy val managers = TableQuery[Managers]

  class Employees(tag: Tag) extends Table[(Int, String, Int)](tag, "employees") {
    def id = column[Int]("id")
    def name = column[String]("name2")
    def manager = column[Int]("manager")
    def * = (id, name, manager)

    // A convenience method for selecting employees by department
    def departmentIs(dept: String) = manager in managers.filter(_.department is dept).map(_.id)
  }
  lazy val employees = TableQuery[Employees]

  def testBasic {
    (managers.ddl ++ employees.ddl).create

    managers ++= Seq(
      (1, "Peter", "HR"),
      (2, "Amy", "IT"),
      (3, "Steve", "IT")
    )

    employees ++= Seq(
      (4, "Jennifer", 1),
      (5, "Tom", 1),
      (6, "Leonard", 2),
      (7, "Ben", 2),
      (8, "Greg", 3)
    )

    val q1 = for(m <- managers filter { _.department is "IT" }) yield (m.id, m.name)
    println("Managers in IT")
    q1.run.foreach(o => println("  "+o))
    assertEquals(Set((2,"Amy"), (3,"Steve")), q1.run.toSet)

    val q2 = for(e <- employees filter { _.departmentIs("IT") }) yield (e.id, e.name)
    println("Employees in IT")
    q2.run.foreach(o => println("  "+o))
    assertEquals(Set((7,"Ben"), (8,"Greg"), (6,"Leonard")), q2.run.toSet)

    val q3 = (q1 union q2).sortBy(_._2.asc)
    println("Combined and sorted")
    q3.run.foreach(o => println("  "+o))
    assertEquals(List((2,"Amy"), (7,"Ben"), (8,"Greg"), (6,"Leonard"), (3,"Steve")), q3.run)

    (managers.ddl ++ employees.ddl).drop
  }

  def testUnionWithoutProjection {
    managers.ddl.create
    managers ++= Seq(
      (1, "Peter", "HR"),
      (2, "Amy", "IT"),
      (3, "Steve", "IT")
    )

    def f (s: String) = managers filter { _.name === s}
    val q = f("Peter") union f("Amy")
    assertEquals(Set((1, "Peter", "HR"), (2, "Amy", "IT")), q.run.toSet)

    managers.ddl.drop
  }

  def testUnionOfJoins {
    class Drinks(tag: Tag, tableName: String) extends Table[(Long, Long)](tag, tableName) {
      def pk = column[Long]("pk")
      def pkCup = column[Long]("pkCup")
      def * = (pk, pkCup)
    }
    val coffees = TableQuery(new Drinks(_, "Coffee"))
    val teas = TableQuery(new Drinks(_, "Tea"))

    (coffees.ddl ++ teas.ddl).create
    coffees ++= Seq(
      (10L, 1L),
      (20L, 2L),
      (30L, 3L)
    )
    teas ++= Seq(
      (100L, 1L),
      (200L, 2L),
      (300L, 3L)
    )

    val q1 = for {
      coffee <- coffees
      tea <- teas if coffee.pkCup === tea.pkCup
    } yield (coffee.pk, coffee.pkCup)
    val q2 = for {
      coffee <- coffees
      tea <- teas if coffee.pkCup === tea.pkCup
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
