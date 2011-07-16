package org.scalaquery.test

import org.junit.Test
import org.junit.Assert._
import org.scalaquery.ql._
import extended.AccessDriver
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql.basic.{BasicTable => Table}
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.test.util._
import org.scalaquery.test.util.TestDB._

object MiscTest extends DBTestObject(H2Mem, SQLiteMem, Postgres, MySQL, DerbyMem, HsqldbMem, MSAccess, SQLServer)

class MiscTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit._

  @Test def isNotAndOrTest() {

    object T extends Table[(String, String)]("users") {
      def a = column[String]("a")
      def b = column[String]("b")
      def * = a ~ b
    }

    db withSession {
      T.ddl.create
      T.insertAll(("1", "a"), ("2", "a"), ("3", "b"))

      val q1 = for(t <- T if t.a === "1" || t.a === "2") yield t
      println("q1: "+q1.selectStatement)
      q1.foreach(println _)
      assertEquals(q1.to[Set](), Set(("1", "a"), ("2", "a")))

      val q2 = for(t <- T if (t.a isNot "1") || (t.b isNot "a")) yield t
      println("q2: "+q2.selectStatement)
      q2.foreach(println _)
      assertEquals(q2.to[Set](), Set(("2", "a"), ("3", "b")))

      val q3 = for(t <- T if (t.a != "1") || (t.b != "a")) yield t
      println("q3: "+q3.selectStatement) // Hah, not what you expect!
      q3.foreach(println _)
      assertEquals(q3.to[Set](), Set(("1", "a"), ("2", "a"), ("3", "b")))

      val q4 = for(t <- T if t.a =!= "1" || t.b =!= "a") yield t
      println("q4: "+q4.selectStatement)
      q4.foreach(println _)
      assertEquals(q4.to[Set](), Set(("2", "a"), ("3", "b")))
    }
  }

  @Test def testNullability() {

    object T1 extends Table[String]("t1") {
      def a = column[String]("a")
      def * = a
    }

    object T2 extends Table[String]("t2") {
      def a = column[String]("a", O Nullable)
      def * = a
    }

    object T3 extends Table[Option[String]]("t3") {
      def a = column[Option[String]]("a")
      def * = a
    }

    object T4 extends Table[Option[String]]("t4") {
      def a = column[Option[String]]("a", O NotNull)
      def * = a
    }

    db withSession {
      (T1.ddl ++ T2.ddl ++ T3.ddl ++ T4.ddl) create

      T1.insert("a")
      T2.insert("a")
      T3.insert(Some("a"))
      T4.insert(Some("a"))

      T2.insert(null.asInstanceOf[String])
      T3.insert(None)

      assertFail { T1.insert(null.asInstanceOf[String]) }
      assertFail { T4.insert(None) }
    }
  }

  @Test def testLike() {

    object T1 extends Table[String]("t1") {
      def a = column[String]("a")
      def * = a
    }

    db withSession {
      T1.ddl.create
      T1.insertAll("foo", "bar", "foobar", "foo%")

      val q1 = for { t1 <- T1 if t1.a like "foo" } yield t1.a
      println("q1: " + q1.selectStatement)
      assertEquals(List("foo"), q1.list)

      val q2 = for { t1 <- T1 if t1.a like "foo%" } yield t1.a
      println("q2: " + q2.selectStatement)
      assertEquals(Set("foo", "foobar", "foo%"), q2.to[Set]())

      if(tdb.driver != AccessDriver) { // Access does not support ESCAPE
        val q3 = for { t1 <- T1 if t1.a.like("foo^%", '^') } yield t1.a
        println("q3: " + q3.selectStatement)
        assertEquals(Set("foo%"), q3.to[Set]())
      }
    }
  }
}
