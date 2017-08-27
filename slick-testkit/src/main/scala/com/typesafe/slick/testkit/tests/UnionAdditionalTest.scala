package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{AsyncTest, JdbcTestDB}

class UnionAdditionalTest extends AsyncTest[JdbcTestDB] {

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

  def testLimitWithUnion = {
    val q1 = for (m <- managers drop 1 take 2) yield (m.id, m.name)
    val q2 = for (e <- employees drop 1 take 3) yield (e.id, e.name)
    val q3 = (q1 union q2) sortBy (_._2)
    (for {
      _ <- (managers.schema ++ employees.schema).create
      _ <- managers ++= Seq((1, "Peter", "HR"), (2, "Amy", "IT"), (3, "Steve", "IT"))
      _ <- employees ++= Seq((4, "Leonard", 2), (5, "Jennifer", 1), (6, "Tom", 1), (7, "Ben", 1), (8, "Greg", 3))
      _ <- mark("q1", q1.result).map(r => r.toSet shouldBe Set((2, "Amy"), (3, "Steve")))
      _ <- mark("q2", q2.result).map(r => r.toSet shouldBe Set((5, "Jennifer"), (6, "Tom"), (7, "Ben")))
      _ <- mark("q3", q3.result).map(_ shouldBe Vector((2, "Amy"), (7, "Ben"), (5, "Jennifer"), (3, "Steve"), (6, "Tom")))
    } yield ()) andFinally (managers.schema ++ employees.schema).drop
  }

  def testOrderByWithUnion = {
    val q1 = for (m <- managers sortBy (_.name)) yield (m.id, m.name)
    val q2 = for (e <- employees sortBy (_.name)) yield (e.id, e.name)
    val q3 = (q1 ++ q2) sortBy (_._2)
    (for {
      _ <- (managers.schema ++ employees.schema).create
      _ <- managers ++= Seq((1, "Peter", "HR"), (2, "Amy", "IT"), (3, "Steve", "IT"))
      _ <- employees ++= Seq((4, "Leonard", 2), (5, "Jennifer", 1), (6, "Tom", 1), (7, "Ben", 1), (8, "Greg", 3))
      _ <- mark("q1", q1.result).map(r => r.toSet shouldBe Set((2, "Amy"), (1, "Peter"), (3, "Steve")))
      _ <- mark("q2", q2.result).map(r => r.toSet shouldBe Set((7, "Ben"), (8, "Greg"), (5, "Jennifer"), (4, "Leonard"), (6, "Tom")))
      _ <- mark("q3", q3.result).map(_ shouldBe Vector((2, "Amy"), (7, "Ben"), (8, "Greg"),
        (5, "Jennifer"), (4, "Leonard"), (1, "Peter"), (3, "Steve"), (6, "Tom")
      ))
    } yield ()) andFinally (managers.schema ++ employees.schema).drop
  }
}
