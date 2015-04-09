package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{AsyncTest, RelationalTestDB}

class AggregateTest extends AsyncTest[RelationalTestDB] {
  import tdb.profile.api._

  def testAggregates = {
    class T(tag: Tag) extends Table[(Int, Option[Int])](tag, "t2") {
      def a = column[Int]("a")
      def b = column[Option[Int]]("b")
      def * = (a, b)
    }
    val ts = TableQuery[T]
    def q1(i: Int) = for { t <- ts if t.a === i } yield t
    def q2(i: Int) = (q1(i).length, q1(i).map(_.a).sum, q1(i).map(_.b).sum, q1(i).map(_.b).avg)
    val q2_0 = q2(0).shaped
    val q2_1 = q2(1).shaped
    ts.schema.create >>
      (ts ++= Seq((1, Some(1)), (1, Some(2)), (1, Some(3)))) >>
      q2_0.result.map(_ shouldBe (0, None, None, None)) >>
      q2_1.result.map(_ shouldBe (3, Some(3), Some(6), Some(2)))
  }

  def testGroupBy = {
    class T(tag: Tag) extends Table[(Int, Option[Int])](tag, "t3") {
      def a = column[Int]("a")
      def b = column[Option[Int]]("b")
      def * = (a, b)
    }
    class U(tag: Tag) extends Table[Int](tag, "u") {
      def id = column[Int]("id")
      def * = id
    }
    val ts = TableQuery[T]
    val us = TableQuery[U]

    db.run {
      ts.schema.create >>
        (ts ++= Seq((1, Some(1)), (1, Some(2)), (1, Some(3)))) >>
        (ts ++= Seq((2, Some(1)), (2, Some(2)), (2, Some(5)))) >>
        (ts ++= Seq((3, Some(1)), (3, Some(9))))
    }.flatMap { _ =>
      val q0 = ts.groupBy(_.a)
      val q1 = q0.map(_._2.length).sortBy(identity)
      db.run(q1.result).map { r0t: Seq[Int] => r0t shouldBe Vector(2, 3, 3) }
    }.flatMap { _ =>
      val q = (for {
        (k, v) <- ts.groupBy(t => t.a)
      } yield (k, v.length, v.map(_.a).sum, v.map(_.b).sum)).sortBy(_._1)
      db.run(q.result).map { rt: Seq[(Int, Int, Option[Int], Option[Int])] =>
        rt shouldBe Vector((1, 3, Some(3), Some(6)), (2, 3, Some(6), Some(8)), (3, 2, Some(6), Some(10)))
      }
    }.flatMap { _ =>
      db.run(us.schema.create >> (us ++= Seq(1, 2, 3)))
    }.flatMap { _ =>
      val q2 = (for {
        u <- us
        t <- ts if t.a === u.id
      } yield (u, t)).groupBy(_._1.id).map {
        case (id, q) => (id, q.length, q.map(_._2.a).sum, q.map(_._2.b).sum)
      }
      db.run(q2.result).map { r2t: Seq[(Int, Int, Option[Int], Option[Int])] =>
        r2t.toSet shouldBe Set((1, 3, Some(3), Some(6)), (2, 3, Some(6), Some(8)), (3, 2, Some(6), Some(10)))
      }
    }.flatMap { _ =>
      val q3 = (for {
        (x, q) <- ts.map(t => (t.a + 10, t.b)).groupBy(_._1)
      } yield (x, q.map(_._2).sum)).sortBy(_._1)
      db.run(q3.result).map { r3t: Seq[(Int, Option[Int])] =>
        r3t shouldBe Vector((11, Some(6)), (12, Some(8)), (13, Some(10)))
      }
    }.flatMap { _ =>
      val q4 = (for {
        (x, q) <- ts.groupBy(t => (t.a, t.b))
      } yield (x, q.length)).sortBy(_._1)
      db.run(q4.result).map { r4t: Seq[((Int, Option[Int]), Int)] =>
        r4t shouldBe Vector(
          ((1,Some(1)),1), ((1,Some(2)),1), ((1,Some(3)),1),
          ((2,Some(1)),1), ((2,Some(2)),1), ((2,Some(5)),1),
          ((3,Some(1)),1), ((3,Some(9)),1)
        )
      }
    }.flatMap { _ =>
      val q5 = ts
        .filter(_.a === 1)
        .map(t => (t.a, t.b))
        .sortBy(_._2)
        .groupBy(x => (x._1, x._2))
        .map { case (a, _) => (a._1, a._2) }
        .to[Set]
      db.run(q5.result).map(_ shouldBe Set((1, Some(1)), (1, Some(2)), (1, Some(3))))
    }.flatMap { _ =>
      db.run(us += 4)
    }.flatMap { _ =>
      val q6 = ((for {
        (u, t) <- us joinLeft ts on (_.id === _.a)
      } yield (u, t)).groupBy(_._1.id).map {
        case (id, q) => (id, q.length, q.map(_._1).length, q.map(_._2).length)
      }).to[Set]
      db.run(q6.result).map(_ shouldBe Set((1, 3, 3, 3), (2, 3, 3, 3), (3, 2, 2, 2), (4, 1, 1, 0)))
    }.flatMap { _ =>
      val q7 = ts.groupBy(_.a).map { case (a, ts) =>
        (a, ts.map(_.b).sum, ts.map(_.b).min, ts.map(_.b).max, ts.map(_.b).avg)
      }.to[Set]
      db.run(q7.result).map(_ shouldBe Set(
        (1, Some(6), Some(1), Some(3), Some(2)),
        (2, Some(8), Some(1), Some(5), Some(2)),
        (3, Some(10), Some(1), Some(9), Some(5))))
    }.flatMap { _ =>
      val q8 = us.map( _ => "test").groupBy(x => x).map(_._2.max)
      val q8b = for( (key, group) <- us.map(_ => "x").groupBy(co => co) ) yield (key, group.map(co => co).max )
      val q8c = for( (key, group) <- us.map(_ => 5).groupBy(co => co) ) yield (key, group.map(co => co + co).sum )
      db.run(for {
        _ <- q8.result.map(_ shouldBe Seq(Some("test")))
        _ <- q8b.result.map(_ shouldBe Seq(("x", Some("x"))))
        _ <- q8c.result.map(_ shouldBe Seq((5, Some(40))))
      } yield ())
    }.flatMap { _ =>
      val res9 = Set(
        (1, Some(1)), (1, Some(2)), (1, Some(3)),
        (2, Some(1)), (2, Some(2)), (2, Some(5)),
        (3, Some(1)), (3, Some(9))
      )
      val q9 = ts.groupBy(x => x).map(_._1).to[Set]
      val q9b = ts.map(x => x).groupBy(_.*).map(_._1).to[Set]
      val q9c = ts.map(x => x).groupBy(x => x).map(_._1).to[Set]
      db.run(for {
        _ <- q9.result.map(_ shouldBe res9)
        _ <- q9b.result.map(_ shouldBe res9)
        _ <- q9c.result.map(_ shouldBe res9)
      } yield ())
    }.flatMap { _ =>
      val q10 = ((for {
        m <- ts
      } yield m) groupBy (_.a) map {
        case (id, data) => (id, data.map(_.b.asColumnOf[Option[Double]]).max)
      }).to[Set]
      db.run(q10.result).map(_ shouldBe Set((2,Some(5.0)), (1,Some(3.0)), (3,Some(9.0))))
    }.flatMap { _ =>
      case class Pair(a:Int,b:Option[Int])
      class T4(tag: Tag) extends Table[Pair](tag, "t4") {
        def a = column[Int]("a")
        def b = column[Option[Int]]("b")
        def * = (a, b) <> (Pair.tupled,Pair.unapply)
      }
      val t4s = TableQuery[T4]
      db.run(t4s.schema.create >>
        (t4s ++= Seq(Pair(1, Some(1)), Pair(1, Some(2)))) >>
        (t4s ++= Seq(Pair(1, Some(1)), Pair(1, Some(2)))) >>
        (t4s ++= Seq(Pair(1, Some(1)), Pair(1, Some(2))))).flatMap { _ =>

        val expected11 = Set( Pair(1, Some(1)), Pair(1, Some(2)) )
        val q12 = t4s
        val q13 = t4s.map(identity)
        val q11 = t4s.groupBy(identity).map(_._1)
        db.run(for {
          res12 <- q12.result
          _ = res12.size shouldBe 6
          _ = res12.toSet shouldBe expected11
          res13 <- q13.result
          _ = res13.size shouldBe 6
          _ = res13.toSet shouldBe expected11
          res11 <- q11.result
          _ = res11.size shouldBe 2
          _ = res11.toSet shouldBe expected11
        } yield ())
      }
    }
  }

  def testIntLength = {
    class A(tag: Tag) extends Table[Int](tag, "A_testIntLength") {
      def id = column[Int]("ID")
      def * = id
    }
    val as = TableQuery[A]
    val q1 = as.groupBy(_.id).map { case (_, q) => (q.map(_.id).min, q.length) }
    DBIO.seq(
      as.schema.create,
      as += 1,
      q1.result
    )
  }

  def testGroup3 = {
    case class Tab(col1: String, col2: String, col3: String, col4: Int, col5: Int)

    class Tabs(tag: Tag) extends Table[Tab](tag, "TAB_group3") {
      def col1 = column[String]("COL1")
      def col2 = column[String]("COL2")
      def col3 = column[String]("COL3")
      def col4 = column[Int]("COL4")
      def col5 = column[Int]("COL5")

      def * = (col1, col2, col3, col4, col5) <> (Tab.tupled, Tab.unapply)
    }
    val Tabs = TableQuery[Tabs]

    for {
      _ <- Tabs.schema.create
      _ <- Tabs ++= Seq(
        Tab("foo", "bar",  "bat", 1, 5),
        Tab("foo", "bar",  "bat", 2, 6),
        Tab("foo", "quux", "bat", 3, 7),
        Tab("baz", "quux", "bat", 4, 8)
      )
      q1 = Tabs.groupBy(t => (t.col1, t.col2, t.col3)).map {
        case (grp, t) => (grp._1, grp._2, t.map(_.col4).sum)
      }.to[Set]
      _ <- q1.result.map(_ shouldBe Set(("baz","quux",Some(4)), ("foo","quux",Some(3)), ("foo","bar",Some(3))))
      q2 = Tabs.groupBy(t => ((t.col1, t.col2), t.col3)).map {
        case (grp, t) => (grp._1._1, grp._1._2, t.map(_.col4).sum)
      }.to[Set]
      _ <- q2.result.map(_ shouldBe Set(("baz","quux",Some(4)), ("foo","quux",Some(3)), ("foo","bar",Some(3))))
      q3 = Tabs.groupBy(_.col1).map {
        case (grp, t) => (grp, t.map(x => x.col4 + x.col5).sum)
      }.to[Set]
      _ <- q3.result.map(_ shouldBe Set(("baz",Some(12)), ("foo",Some(24))))
    } yield ()
  }

  def testMultiMapAggregates = {
    class B(tag: Tag) extends Table[(Long, String, String)](tag, "b_multimap") {
      def id = column[Long]("id", O.PrimaryKey)
      def b = column[String]("b")
      def d = column[String]("d")
      def * = (id, b, d)
    }
    class A(tag: Tag) extends Table[(Long, String, Long, Long)](tag, "a_multimap") {
      def id = column[Long]("id", O.PrimaryKey)
      def a = column[String]("a")
      def c = column[Long]("c")
      def fkId = column[Long]("fkId")
      def * = (id, a, c, fkId)
    }
    val bs = TableQuery[B]
    val as = TableQuery[A]
    for {
      _ <- (as.schema ++ bs.schema).create
      q1 = as.groupBy(_.id).map(_._2.map(x => x).map(x => x.a).min)
      _ <- q1.result.map(v => v.toList shouldBe Nil)
      q2 =
        (as joinLeft bs on (_.id === _.id)).map { case (c, so) =>
          val nameo = so.map(_.b)
          (c, so, nameo)
        }.groupBy { prop =>
          val c = prop._1
          val so = prop._2
          val nameo = prop._3
          so.map(_.id)
        }.map { prop =>
          val supId = prop._1
          val c = prop._2.map(x => x._1)
          val s = prop._2.map(x => x._2)
          val name = prop._2.map(x => x._3)
          (name.min, s.map(_.map(_.b)).min, supId, c.length)
        }
      _ <- q2.result.map(_ shouldBe Nil)
      q4 = as.flatMap { t1 =>
        bs.withFilter { t2 =>
          t1.fkId === t2.id && t2.d === ""
        }.map(t2 => (t1, t2))
      }.groupBy { prop =>
        val t1 = prop._1
        val t2 = prop._2
        (t1.a, t2.b)
      }.map { prop =>
        val a = prop._1._1
        val b = prop._1._2
        val t1 = prop._2.map(_._1)
        val t2 = prop._2.map(_._2)
        val c3 = t1.map(_.c).max
        scala.Tuple3(a, b, c3)
      }
      _ <- q4.result.map(_ shouldBe Nil)
    } yield ()
  }
}
