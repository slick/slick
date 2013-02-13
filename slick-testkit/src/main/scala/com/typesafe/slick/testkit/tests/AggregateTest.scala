package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{TestkitTest, TestDB}

class AggregateTest(val tdb: TestDB) extends TestkitTest {
  import tdb.profile.simple._

  override val reuseInstance = true

  def testAggregates {
    object T extends Table[(Int, Option[Int])]("t2") {
      def a = column[Int]("a")
      def b = column[Option[Int]]("b")
      def * = a ~ b
    }
    T.ddl.create
    T.insertAll((1, Some(1)), (1, Some(2)), (1, Some(3)))
    def q1(i: Int) = for { t <- T if t.a === i } yield t
    def q2(i: Int) = (q1(i).length, q1(i).map(_.a).sum, q1(i).map(_.b).sum, q1(i).map(_.b).avg)
    val q2_0 = q2(0).toQueryExecutor
    val q2_1 = q2(1).toQueryExecutor
    println("q2_0: "+q2_0.selectStatement)
    println("q2_1: "+q2_1.selectStatement)
    println(q2_0.run)
    println(q2_1.run)
    assertEquals((0, None, None, None), q2_0.run)
    assertEquals((3, Some(3), Some(6), Some(2)), q2_1.run)
  }

  def testGroupBy = {
    object T extends Table[(Int, Option[Int])]("t3") {
      def a = column[Int]("a")
      def b = column[Option[Int]]("b")
      def * = a ~ b
    }
    T.ddl.create
    T.insertAll((1, Some(1)), (1, Some(2)), (1, Some(3)))
    T.insertAll((2, Some(1)), (2, Some(2)), (2, Some(5)))
    T.insertAll((3, Some(1)), (3, Some(9)))

    println("=========================================================== q0")
    val q0 = T.groupBy(_.a)
    val q1 = q0.map(_._2.length).sortBy(identity)
    val r0 = q1.list
    val r0t: List[Int] = r0
    println(q1.selectStatement)
    assertEquals(List(2, 3, 3), r0t)

    println("=========================================================== q")
    val q = (for {
      (k, v) <- T.groupBy(t => t.a)
    } yield (k, v.length, v.map(_.a).sum, v.map(_.b).sum)).sortBy(_._1)
    println(q.selectStatement)
    val r = q.list
    val rt = r: List[(Int, Int, Option[Int], Option[Int])]
    println(r)
    assertEquals(List((1, 3, Some(3), Some(6)), (2, 3, Some(6), Some(8)), (3, 2, Some(6), Some(10))), rt)

    object U extends Table[Int]("u") {
      def id = column[Int]("id")
      def * = id
    }
    U.ddl.create
    U.insertAll(1, 2, 3)

    println("=========================================================== q2")
    val q2 = (for {
      u <- U
      t <- T if t.a === u.id
    } yield (u, t)).groupBy(_._1.id).map {
      case (id, q) => (id, q.length, q.map(_._2.a).sum, q.map(_._2.b).sum)
    }
    println(q2.selectStatement)
    val r2 = q2.list
    val r2t = r2: List[(Int, Int, Option[Int], Option[Int])]
    println(r2)
    assertEquals(List((1, 3, Some(3), Some(6)), (2, 3, Some(6), Some(8)), (3, 2, Some(6), Some(10))), r2)

    println("=========================================================== q3")
    val q3 = (for {
      (x, q) <- T.map(t => (t.a + 10, t.b)).groupBy(_._1)
    } yield (x, q.map(_._2).sum)).sortBy(_._1)
    println(q3.selectStatement)
    val r3 = q3.list
    val r3t = r3: List[(Int, Option[Int])]
    println(r3)
    assertEquals(List((11, Some(6)), (12, Some(8)), (13, Some(10))), r3)
  }
}
