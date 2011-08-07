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
      assertEquals(res1, q1a.to[List]())

      val q1b = for {
        (a, b) <- T.map(t => (t.a, t.b))
        c <- T.map(t => t.c)
        _ <- Query.orderBy(c, a)
      } yield a ~ b ~ c ~ 5
      println("q1b: "+q1b.selectStatement)
      println(q1b.list)
      assertEquals(res1, q1b.to[List]())

      val q1c = for {
        (a, b) <- T.map(t => (t.a, t.b))
        c <- T.map(t => t.c)
        _ <- Query.orderBy(c, a)
      } yield (a, b, c, ConstColumn(5))
      println("q1c: "+q1c.selectStatement)
      println(q1c.list)
      assertEquals(res1, q1c.to[List]())

      val q1d = for {
        (a, b) <- T.map(t => (t.a, t.b))
        c <- T.map(t => t.c)
        _ <- Query.orderBy(c, a)
      } yield ((a, b), (c, 5))
      println("q1d: "+q1d.selectStatement)
      println(q1d.list)
      assertEquals(res1b, q1d.to[List]())

      val res2 = Set((1, "1", 8), (2, "2", 10))

      val q2a = for {
        a ~ b ~ c <- T.where(_.a === 1).map(t => t.a ~ t.b ~ 4) unionAll T.where(_.a === 2).map(t => t.a ~ t.b ~ 5)
      } yield a ~ b ~ (c*2)
      //q2a.dump("q2a: ")
      println("q2a: "+q2a.selectStatement)
      assertEquals(res2, q2a.to[Set]())

      val q2b = for {
        (a, b, c) <- T.where(_.a === 1).map(t => (t.a, t.b, ConstColumn(4))) unionAll T.where(_.a === 2).map(t => (t.a, t.b, ConstColumn(5)))
      } yield a ~ b ~ (c*2)
      q2b.dump("q2b: ")
      println("q2b: "+q2b.selectStatement)
      println(q2b.list)
      assertEquals(res2, q2b.to[Set]())

      val q2c = for {
        (a, b, c) <- T.where(_.a === 1).map(t => (t.a, t.b, 4)) unionAll T.where(_.a === 2).map(t => (t.a, t.b, 5))
      } yield a ~ b ~ (c*2)
      q2c.dump("q2c: ")
      println("q2c: "+q2c.selectStatement)
      println(q2c.list)
      assertEquals(res2, q2c.to[Set]())
    }
  }
}
