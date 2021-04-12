package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{AsyncTest, RelationalTestDB}

class UnionTest extends AsyncTest[RelationalTestDB] {
  import tdb.profile.api.*

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

  def managersQuery = for(m <- managers filter { _.department === "IT" }) yield (m.id, m.name)

  def employeesQuery = for(e <- employees filter { _.departmentIs("IT") }) yield (e.id, e.name)

  val managersData = Seq(
    (1, "Peter", "HR"),
    (2, "Amy", "IT"),
    (3, "Steve", "IT")
  )

  val employeesData = Seq(
    (4, "Jennifer", 1),
    (5, "Tom", 1),
    (6, "Leonard", 2),
    (7, "Ben", 2),
    (8, "Greg", 3)
  )

  def testBasicUnions = {

    val q1 = managersQuery
    val q2 = employeesQuery
    val q3 = (q1 union q2).sortBy(_._2.asc)
    val q4 = managers.map(_.id)
    val q4b = q4 union q4
    val q4c = q4 union q4 union q4
    val q5 = managers.map(m => (m.id, 0)) union employees.map(e => (e.id, e.id))

    (for {
      _ <- (managers.schema ++ employees.schema).create
      _ <- managers ++= managersData
      _ <- employees ++= employeesData
      _ <- mark("q1", q1.result).map(r => r.toSet shouldBe Set((2,"Amy"), (3,"Steve")))
      _ <- mark("q2", q2.result).map(r => r.toSet shouldBe Set((7,"Ben"), (8,"Greg"), (6,"Leonard")))
      _ <- mark("q3", q3.result).map(_ shouldBe List((2,"Amy"), (7,"Ben"), (8,"Greg"), (6,"Leonard"), (3,"Steve")))
      _ <- mark("q4b", q4b.result).map(r => r.toSet shouldBe Set(1, 2, 3))
      _ <- mark("q4c", q4c.result).map(r => r.toSet shouldBe Set(1, 2, 3))
      _ <- mark("q5", q5.result).map(r => r.toSet shouldBe Set((7,7), (6,6), (2,0), (4,4), (3,0), (8,8), (5,5), (1,0)))
    } yield ()) andFinally (managers.schema ++ employees.schema).drop
  }

  def testUnionWithLimit = {
    val q1 = managersQuery
    val q2 = employeesQuery.sortBy(_._2.asc)
    val union = (q1 ++ q2.take(1)).sortBy(_._2.asc)

    (for {
      _ <- (managers.schema ++ employees.schema).create
      _ <- managers ++= managersData
      _ <- employees ++= employeesData
      _ <- mark("union", union.result).map(_ shouldBe List((2,"Amy"),(7,"Ben"),(3,"Steve")))
    } yield ()) andFinally (managers.schema ++ employees.schema).drop
  }

  def testUnionWithoutProjection = {
    def f (s: String) = managers filter { _.name === s}
    val q = f("Peter") union f("Amy")

    seq(
      managers.schema.create,
      managers ++= managersData,
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

  def testCountWithUnion = {
    case class Delivery(id: Long, dname: String, messageId: Long, sentAt: Long)

    case class Message(id: Long, mname: String, mbody: String)

    class Deliveries(tag: Tag) extends Table[Delivery](tag, "d2") {
      val id = column[Long]("delivery_id")
      val dname = column[String]("dname")
      val messageId = column[Long]("message_id")
      val sentAt = column[Long]("sent_at")

      def * = (id, dname, messageId, sentAt).mapTo[Delivery]
    }

    class Messages(tag: Tag) extends Table[Message](tag, "m2") {
      val id = column[Long]("message_id")
      val mname = column[String]("mname")
      val mbody = column[String]("mbody")

      def * = (id, mname, mbody).mapTo[Message]
    }


    def leftSide = {
      (for {
        d <- TableQuery[Deliveries]
        m <- TableQuery[Messages] if d.messageId === m.id
      } yield (d, m))
        .filter { case (d, m) => d.sentAt >= 1400000000L }
    }

    def rightSide = {
      (for {
        d <- TableQuery[Deliveries]
        m <- TableQuery[Messages] if d.messageId === m.id
      } yield (d, m))
        .filter { case (d, m) => d.sentAt < 1400000000L }
    }

    val query =
      leftSide.union(rightSide).length

    DBIO.seq(
      TableQuery[Deliveries].schema.create,
      TableQuery[Messages].schema.create,
      mark("q", query.result).map(_ shouldBe 0)
    )
  }

  def testCountWithUnionAndSort = {
    case class Delivery(id: Long, dname: String, sentAt: Long)

    class Deliveries(tag: Tag) extends Table[Delivery](tag, "d") {
      val id = column[Long]("delivery_id")
      val dname = column[String]("dname")
      val sentAt = column[Long]("sent_at")

      def * = (id, dname, sentAt).mapTo[Delivery]
    }

    def leftSide = {
      TableQuery[Deliveries].filter(_.sentAt >= 1400000000L)
    }

    def rightSide = {
      TableQuery[Deliveries].filter(_.sentAt < 1400000000L)
    }

    val query =
      leftSide.union(rightSide).sortBy(_.id.desc).length

    DBIO.seq(
      TableQuery[Deliveries].schema.create,
      mark("q", query.result).map(_ shouldBe 0)
    )
  }

  def testMappedUnion = {
    class T(tag: Tag) extends Table[(String, Int, String, Int)](tag, "t".withUniquePostFix) {
      def a = column[String]("a")
      def b = column[Int]("b")
      def c = column[String]("c")
      def d = column[Int]("d")
      def e = column[String]("e")
      def * = (a, b, c, d)
      override def create_* = collectFieldSymbols((*, e).shaped.toNode)
    }
    val ts = TableQuery[T]
    val q1 = ts.filter(_.a === "a") ++ ts.filter(_.e === "e")
    DBIO.seq(
      ts.schema.create,
      ts.map(f => (f.a, f.b, f.c, f.d, f.e)) += (("a", 1, "c", 3, "e")),
      q1.result.map(_ shouldBe Vector(("a",1,"c",3), ("a",1,"c",3)))
    )
  }

  def testInUnion = {
    class T(tag: Tag) extends Table[(String, Int)](tag, "t") {
      def id = column[String]("id")
      def data = column[Int]("data")
      def * = (id, data)
    }

    val ts = TableQuery[T]
    val q1 = ts.filter(_.data.in((ts.filter(_.id === "a") ++ ts.filter(_.id === "c")).map(_.data)))
    DBIO.seq(
      ts.schema.create,
      ts.map(f => (f.id, f.data)) ++= Seq(("a", 1), ("b", 2), ("c", 1), ("d", 1)),
      q1.result.map(_ shouldBe Vector(("a", 1), ("c", 1), ("d", 1)))
    )
  }

}
