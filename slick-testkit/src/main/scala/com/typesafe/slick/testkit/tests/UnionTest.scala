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
    def departmentIs(dept: String) = manager.in(managers.filter(_.department === dept).map(_.id))
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
    val q3 = (q1.union(q2)).sortBy(_._2.asc)
    val q4 = managers.map(_.id)
    val q4b = q4.union(q4)
    val q4c = q4.union(q4).union(q4)
    val q5 = managers.map(m => (m.id, 0)).union(employees.map(e => (e.id, e.id)))

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
    } yield ()).andFinally((managers.schema ++ employees.schema).drop)
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
    } yield ()).andFinally((managers.schema ++ employees.schema).drop)
  }

  def testUnionWithoutProjection = {
    def f (s: String) = managers filter { _.name === s}
    val q = f("Peter").union(f("Amy"))

    seq(
      managers.schema.create,
      managers ++= managersData,
      q.result.map(r => r.toSet shouldBe Set((1, "Peter", "HR"), (2, "Amy", "IT")))
    ).andFinally(managers.schema.drop)
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
    val q3 = q1.union(q2)

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

  def testSameTableUnion = {
    case class T(table: String)(tag: Tag) extends Table[(String, String, Int, Int, Int, Int)](tag, table) {
      def from = column[String]("from")
      def to = column[String]("to")
      def data = column[Int]("data")
      def data1 = column[Int]("data1")
      def data2 = column[Int]("data2")
      def data3 = column[Int]("data3")
      def * = (from, to, data, data1, data2, data3)
    }

    abstract class TT(table: String)(tag: Tag) extends Table[(String, Int, Int, Int, Int)](tag, table) {
      def party: Rep[String]
      def data = column[Int]("data")
      def data1 = column[Int]("data1")
      def data2 = column[Int]("data2")
      def data3 = column[Int]("data3")
      def * = (party, data, data1, data2, data3)
    }
    case class T1(table: String)(tag: Tag) extends TT(table)(tag) {
      def party = column[String]("from")
    }
    case class T2(table: String)(tag: Tag) extends TT(table)(tag) {
      def party = column[String]("to")
    }

    val ts = TableQuery(T("t1"))
    val ts1 = TableQuery(T1("t1"))
    val ts2 = TableQuery(T2("t1"))
    val q1 = (ts1 ++ ts2).distinctOn(_.party).sortBy(t => (t.party, t.data))
    DBIO.seq(
      ts.schema.createIfNotExists,
      ts ++= Seq(("from1", "to1", 1, 2, 3, 4), ("from1", "to2", 11, 2, 3, 4)),
      q1.result.map(_ shouldBe Vector(("from1", 1, 2, 3, 4), ("to1", 1, 2, 3, 4), ("to2", 11, 2, 3, 4)))
    )
  }

  def testUnionOfTablesWithNonColumnProjections = {
    abstract class AB(tag: Tag, name: String) extends Table[(Int, Boolean)](tag, name) {
      def a = column[Int]("a")
    }
    class TF(tag: Tag) extends AB(tag, "u1559_tf") {
      def * = (a, LiteralColumn(false))
    }
    class TT(tag: Tag) extends AB(tag, "u1559_tt") {
      def * = (a, LiteralColumn(true))
    }
    class CF(tag: Tag) extends Table[(Int, Int)](tag, "u1559_cf") {
      def a = column[Int]("a")
      def * = (a, a + 10)
    }
    class CT(tag: Tag) extends Table[(Int, Int)](tag, "u1559_ct") {
      def b = column[Int]("b")
      def * = (b, b * 100)
    }
    case class R(a: Int, s: String)
    class MF(tag: Tag) extends Table[R](tag, "u1559_mf") {
      def a = column[Int]("a")
      def * = (a, LiteralColumn("f")).mapTo[R]
    }
    class MT(tag: Tag) extends Table[R](tag, "u1559_mt") {
      def a = column[Int]("a")
      def * = (a, LiteralColumn("t")).mapTo[R]
    }
    val tfs = TableQuery[TF]
    val tts = TableQuery[TT]
    val cfs = TableQuery[CF]
    val cts = TableQuery[CT]
    val mfs = TableQuery[MF]
    val mts = TableQuery[MT]
    val schema = tfs.schema ++ tts.schema ++ cfs.schema ++ cts.schema ++ mfs.schema ++ mts.schema

    seq(
      schema.create,
      tfs.map(_.a) ++= Seq(1, 3),
      tts.map(_.a) += 2,
      cfs.map(_.a) += 1,
      cts.map(_.b) += 2,
      mfs.map(_.a) += 1,
      mts.map(_.a) += 2,
      mark("plain", tfs.result).map(_.toSet shouldBe Set((1, false), (3, false))),
      mark("union", tfs.union(tts).result).map(_.toSet shouldBe Set((1, false), (3, false), (2, true))),
      mark("unionAll", (tfs ++ tts).result).map(_.toSet shouldBe Set((1, false), (3, false), (2, true))),
      mark("reversed", tts.union(tfs).result).map(_.toSet shouldBe Set((1, false), (3, false), (2, true))),
      mark("filtered sides", tfs.filter(_.a < 3).union(tts.filter(_.a > 0)).result)
        .map(_.toSet shouldBe Set((1, false), (2, true))),
      mark("filtered union", tfs.union(tts).filter(_.a > 1).result).map(_.toSet shouldBe Set((3, false), (2, true))),
      mark("sorted union", (tfs ++ tts).sortBy(_.a).result).map(_ shouldBe Seq((1, false), (2, true), (3, false))),
      mark("mapped union", tfs.union(tts).map(t => (t.a, t)).result)
        .map(_.toSet shouldBe Set((1, (1, false)), (3, (3, false)), (2, (2, true)))),
      mark("nested union", (tfs ++ tts ++ tfs.filter(_.a === 1)).result)
        .map(_.sorted shouldBe Seq((1, false), (1, false), (2, true), (3, false))),
      mark("union in subquery", tfs.union(tts).sortBy(_.a).take(2).result).map(_ shouldBe Seq((1, false), (2, true))),
      mark("distinct", (tfs ++ tts ++ tts).distinct.result).map(_.toSet shouldBe Set((1, false), (3, false), (2, true))),
      mark("length", tfs.union(tts).length.result).map(_ shouldBe 3),
      mark("join", tfs.join(tts).on(_.a < _.a).result).map(_.toSet shouldBe Set(((1, false), (2, true)))),
      mark("computed", cfs.union(cts).result).map(_.toSet shouldBe Set((1, 11), (2, 200))),
      mark("computed, reversed", (cts ++ cfs).result).map(_.toSet shouldBe Set((1, 11), (2, 200))),
      mark("mapTo", mfs.union(mts).result).map(_.toSet shouldBe Set(R(1, "f"), R(2, "t")))
    ).andFinally(schema.drop)
  }

  def testUnionOfRowsContainingTablesWithNonColumnProjections = {
    abstract class AB(tag: Tag, name: String) extends Table[(Int, Boolean)](tag, name) {
      def a = column[Int]("a")
    }
    class TF(tag: Tag) extends AB(tag, "u1559_rf") {
      def * = (a, LiteralColumn(false))
    }
    class TT(tag: Tag) extends AB(tag, "u1559_rt") {
      def * = (a, LiteralColumn(true))
    }
    class TC(tag: Tag) extends AB(tag, "u1559_rc") {
      def b = column[Boolean]("b")
      def * = (a, b)
    }
    class TP(tag: Tag, name: String) extends Table[(Int, Int)](tag, name) {
      def a = column[Int]("a")
      def b = column[Int]("b")
      def * = (a, b)
    }
    case class R(a: Int, s: String)
    class MF(tag: Tag) extends Table[R](tag, "u1559_rmf") {
      def a = column[Int]("a")
      def * = (a, LiteralColumn("f")).mapTo[R]
    }
    class MT(tag: Tag) extends Table[R](tag, "u1559_rmt") {
      def a = column[Int]("a")
      def * = (a, LiteralColumn("t")).<>({ (t: (Int, String)) => R(t._1, t._2) }, (r: R) => Some((r.a, r.s)))
    }
    val tfs = TableQuery[TF]
    val tts = TableQuery[TT]
    val tcs = TableQuery[TC]
    val p1 = TableQuery(new TP(_, "u1559_rp1"))
    val p2 = TableQuery(new TP(_, "u1559_rp2"))
    val mfs = TableQuery[MF]
    val mts = TableQuery[MT]
    val schema = tfs.schema ++ tts.schema ++ tcs.schema ++ p1.schema ++ p2.schema ++ mfs.schema ++ mts.schema

    val joins = tfs.join(tts).on(_.a < _.a) ++ tts.join(tfs).on(_.a < _.a)
    val joinRows = Set(((1, false), (2, true)), ((2, true), (3, false)))

    seq(
      schema.create,
      tfs.map(_.a) ++= Seq(1, 3),
      tts.map(_.a) += 2,
      tcs ++= Seq((5, true), (6, false)),
      p1 ++= Seq((1, 10), (3, 30)),
      p2 += ((2, 20)),
      mfs.map(_.a) += 1,
      mts.map(_.a) += 2,
      mark("joins of column-only tables", p1.join(p2).on(_.a < _.a).union(p2.join(p1).on(_.a < _.a)).result)
        .map(_.toSet shouldBe Set(((1, 10), (2, 20)), ((2, 20), (3, 30)))),
      mark("joins", joins.result).map(_.toSet shouldBe joinRows),
      mark("filtered joins", joins.filter(_._1.a > 1).map(_._1).result).map(_.toSet shouldBe Set((2, true))),
      mark("swapped joins", joins.map { case (x, y) => (y, x) }.result).map(_.toSet shouldBe joinRows.map(_.swap)),
      mark("nested joins", (joins ++ tcs.join(tcs).on(_.a < _.a)).result)
        .map(_.toSet shouldBe joinRows + (((5, true), (6, false)))),
      mark("joinLeft of column-only tables", p1.joinLeft(p2).on(_.a < _.a).union(p2.joinLeft(p1).on(_.a < _.a)).result)
        .map(_.toSet shouldBe Set(((1, 10), Some((2, 20))), ((3, 30), None), ((2, 20), Some((3, 30))))),
      mark("joinLeft", tfs.joinLeft(tts).on(_.a < _.a).union(tts.joinLeft(tfs).on(_.a < _.a)).result)
        .map(_.toSet shouldBe Set(((1, false), Some((2, true))), ((3, false), None), ((2, true), Some((3, false))))),
      mark("tuple", tfs.map(t => (t, t.a)).union(tts.map(t => (t, t.a))).result)
        .map(_.toSet shouldBe Set(((1, false), 1), ((3, false), 3), ((2, true), 2))),
      mark("nested tuple", tfs.map(t => ((t, t.a), t.a)).union(tts.map(t => ((t, t.a), t.a))).result)
        .map(_.toSet shouldBe Set((((1, false), 1), 1), (((3, false), 3), 3), (((2, true), 2), 2))),
      mark("mapTo and <>", mfs.map(m => (m, m.a)).union(mts.map(m => (m, m.a))).result)
        .map(_.toSet shouldBe Set((R(1, "f"), 1), (R(2, "t"), 2))),
      mark("table and mapped table", tfs.union(tts.map(t => t)).result)
        .map(_.toSet shouldBe Set((1, false), (3, false), (2, true))),
      mark("mapped table and table", tfs.map(t => t).union(tts).result)
        .map(_.toSet shouldBe Set((1, false), (3, false), (2, true))),
      mark("columns and literal", tcs.union(tts).filter(_.a > 1).result)
        .map(_.toSet shouldBe Set((5, true), (6, false), (2, true))),
      mark("literal and columns", tfs.union(tcs).result)
        .map(_.toSet shouldBe Set((1, false), (3, false), (5, true), (6, false))),
      mark("right-nested", (tfs ++ (tts ++ tfs.filter(_.a === 1))).result)
        .map(_.sorted shouldBe Seq((1, false), (1, false), (2, true), (3, false)))
    ).andFinally(schema.drop)
  }
}
