package org.scalaquery.test

import org.junit.Test
import org.junit.Assert._
import org.scalaquery.ql._
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql.basic.{BasicTable => Table}
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.test.util._
import org.scalaquery.test.util.TestDB._

object NestingTest extends DBTestObject(H2Mem /*, SQLiteMem, Postgres, MySQL, DerbyMem, HsqldbMem, MSAccess, SQLServer*/)

class NestingTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit._

  @Test def testUnpackTypes() {
    val q1: Query[Column[Int]] = null
    val q2: Query[Projection2[Int, String]] = null
    val q3: Query[Projection3[Int, String, Int]] = null
    val q4: Query[(NamedColumn[Int], Column[String])] = null
    val q5: Query[(Column[Int], (Column[String], Column[Int]))] = null
    val q6: Query[((Column[Int], Column[String], Column[Int]), (Column[String], Column[Int]))] = null
    val q7: Query[(Column[Int], (Column[String], (Column[String], Int)))] = null

    def call[T, U](q: Query[T])(implicit u: T =>> U): U = null.asInstanceOf[U]

    val c1 = call(q1)
    val c2 = call(q2)
    val c3 = call(q3)
    val c4 = call(q4)
    val c5 = call(q5)
    val c6 = call(q6)
    val c7 = call(q7)

    val c1t: Int = c1
    val c2t: (Int, String) = c2
    val c3t: (Int, String, Int) = c3
    val c4t: (Int, String) = c4
    val c5t: (Int, (String, Int)) = c5
    val c6t: ((Int, String, Int), (String, Int)) = c6
    val c7t: (Int, (String, (String, Int))) = c7
  }

  @Test def testNestedTuples() {

    object T extends Table[(Int, String, String)]("T") {
      def a = column[Int]("A")
      def b = column[String]("B")
      def c = column[String]("C")
      def * = a ~ b ~ c
    }

    db withSession {
      T.ddl.create
      T.insertAll((1, "1", "a"), (2, "2", "b"), (3, "3", "c"))

      val res1 = List(
        (1, "1", "a", 5), (2, "2", "a", 5), (3, "3", "a", 5),
        (1, "1", "b", 5), (2, "2", "b", 5), (3, "3", "b", 5),
        (1, "1", "c", 5), (2, "2", "c", 5), (3, "3", "c", 5)
      )
      val res1b = res1.map { case (a, b, c, d) => ((a, b), (c, d)) }

      val q1a = for {
        a ~ b <- T.map(t => t.a ~ t.b)
        c <- T.map(t => t.c)
        _ <- Query.orderBy(c, a)
      } yield a ~ b ~ c ~ 5
      println("q1a: "+q1a.selectStatement)
      assertEquals(q1a.to[List](), res1)

      val q1b = for {
        (a, b) <- T.map(t => (t.a, t.b))
        c <- T.map(t => t.c)
        _ <- Query.orderBy(c, a)
      } yield a ~ b ~ c ~ 5
      println("q1b: "+q1b.selectStatement)
      println(q1b.list)
      assertEquals(q1b.to[List](), res1)

      val q1c = for {
        (a, b) <- T.map(t => (t.a, t.b))
        c <- T.map(t => t.c)
        _ <- Query.orderBy(c, a)
      } yield (a, b, c, ConstColumn(5))
      println("q1c: "+q1c.unpack.selectStatement)
      println(q1c.unpack.list)
      assertEquals(q1c.unpack.to[List](), res1)

      val q1d = for {
        (a, b) <- T.map(t => (t.a, t.b))
        c <- T.map(t => t.c)
        _ <- Query.orderBy(c, a)
      } yield ((a, b), (c, 5))
      println("q1d: "+q1d.unpack.selectStatement)
      println(q1d.unpack.list)
      assertEquals(q1d.unpack.to[List](), res1b)

      val res2 = Set((1, "1"), (2, "2"))

      val q2a = for {
        a ~ b <- T.where(_.a === 1).map(t => t.a ~ t.b) unionAll T.where(_.a === 2).map(t => t.a ~ t.b)
      } yield a ~ b
      //q2a.dump("q2a: ")
      println("q2a: "+q2a.selectStatement)
      assertEquals(q2a.to[Set](), res2)

      val q2b = for {
        (a, b) <- T.where(_.a === 1).map(t => (t.a, t.b)) unionAll T.where(_.a === 2).map(t => (t.a, t.b))
      } yield a ~ b
      //q2b.dump("q2b: ")
      println("q2b: "+q2b.selectStatement)
      println(q2b.list)
      assertEquals(q2b.to[Set](), res2)
    }
  }
}
