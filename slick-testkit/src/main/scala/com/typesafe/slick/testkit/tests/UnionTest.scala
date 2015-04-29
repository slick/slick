package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{RelationalTestDB, AsyncTest}

class UnionTest extends AsyncTest[RelationalTestDB] {
  import tdb.profile.api._

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
    def departmentIs(dept: String) = manager in managers.filter(_.department === dept).map(_.id)
  }
  lazy val employees = TableQuery[Employees]

  def testBasic = {
    val q1 = for(m <- managers filter { _.department === "IT" }) yield (m.id, m.name)
    val q2 = for(e <- employees filter { _.departmentIs("IT") }) yield (e.id, e.name)
    val q3 = (q1 union q2).sortBy(_._2.asc)
    val q4 = managers.map(_.id)
    val q4b = q4 union q4
    val q4c = q4 union q4 union q4

    (for {
      _ <- (managers.schema ++ employees.schema).create
      _ <- managers ++= Seq(
        (1, "Peter", "HR"),
        (2, "Amy", "IT"),
        (3, "Steve", "IT")
      )
      _ <- employees ++= Seq(
        (4, "Jennifer", 1),
        (5, "Tom", 1),
        (6, "Leonard", 2),
        (7, "Ben", 2),
        (8, "Greg", 3)
      )
      _ <- mark("q1", q1.result).map(r => r.toSet shouldBe Set((2,"Amy"), (3,"Steve")))
      _ <- mark("q2", q2.result).map(r => r.toSet shouldBe Set((7,"Ben"), (8,"Greg"), (6,"Leonard")))
      _ <- mark("q3", q3.result).map(_ shouldBe List((2,"Amy"), (7,"Ben"), (8,"Greg"), (6,"Leonard"), (3,"Steve")))
      _ <- mark("q4b", q4b.result).map(r => r.toSet shouldBe Set(1, 2, 3))
      _ <- mark("q4c", q4c.result).map(r => r.toSet shouldBe Set(1, 2, 3))
    } yield ()) andFinally (managers.schema ++ employees.schema).drop
  }

  def testUnionWithoutProjection = {
    def f (s: String) = managers filter { _.name === s}
    val q = f("Peter") union f("Amy")

    seq(
      managers.schema.create,
      managers ++= Seq(
        (1, "Peter", "HR"),
        (2, "Amy", "IT"),
        (3, "Steve", "IT")
      ),
      q.result.map(r => r.toSet shouldBe Set((1, "Peter", "HR"), (2, "Amy", "IT")))
    ) andFinally managers.schema.drop
  }

  def testUnionOfJoins = {
    class Drinks(tag: Tag, tableName: String) extends Table[(Long, Long)](tag, tableName) {
      def pk = column[Long]("pk")
      def pkCup = column[Long]("pkCup")
      def * = (pk, pkCup)
    }
    val coffees = TableQuery(new Drinks(_, "Coffee"))
    val teas = TableQuery(new Drinks(_, "Tea"))

    val q1 = for {
      coffee <- coffees
      tea <- teas if coffee.pkCup === tea.pkCup
    } yield (coffee.pk, coffee.pkCup)
    val q2 = for {
      coffee <- coffees
      tea <- teas if coffee.pkCup === tea.pkCup
    } yield (tea.pk, tea.pkCup)
    val q3 = q1 union q2

    seq(
      (coffees.schema ++ teas.schema).create,
      coffees ++= Seq(
        (10L, 1L),
        (20L, 2L),
        (30L, 3L)
      ),
      teas ++= Seq(
        (100L, 1L),
        (200L, 2L),
        (300L, 3L)
      ),
      q1.result.map(r => r.toSet shouldBe Set((10L, 1L), (20L, 2L), (30L, 3L))),
      q2.result.map(r => r.toSet shouldBe Set((100L, 1L), (200L, 2L), (300L, 3L))),
      q3.result.map(r => r.toSet shouldBe Set((10L, 1L), (20L, 2L), (30L, 3L), (100L, 1L), (200L, 2L), (300L, 3L)))
    )
  }
}
