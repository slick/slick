package com.novocode.squery.test

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.JUnitCore
import com.novocode.squery.combinator._
import com.novocode.squery.combinator.TypeMapper._
import com.novocode.squery.combinator.basic.SQLiteDriver.Implicit._
import com.novocode.squery.combinator.basic.{BasicTable => Table}
import com.novocode.squery.session._
import com.novocode.squery.session.Database.threadLocalSession

object MiscTest { def main(args: Array[String]) = JUnitCore.main(Array(classOf[MiscTest].getName):_*); }

class MiscTest {

  @Test def isNotAndOrTest() {

    object T extends Table[(String, String)]("users") {
      def a = column[String]("a", O NotNull)
      def b = column[String]("b", O NotNull)
      def * = a ~ b
    }

    Database.forURL("jdbc:sqlite:sample.db", driver = "org.sqlite.JDBC") withSession {
      T.ddl.create
      T.insertAll(("1", "a"), ("2", "a"), ("3", "b"))

      val q1 = for(t <- T if t.a === "1" || t.a === "2") yield t
      println("q1: "+q1.selectStatement)
      q1.foreach(println _)
      assertEquals(q1.list.toSet, Set(("1", "a"), ("2", "a")))

      val q2 = for(t <- T if (t.a isNot "1") || (t.b isNot "a")) yield t
      println("q2: "+q2.selectStatement)
      q2.foreach(println _)
      assertEquals(q2.list.toSet, Set(("2", "a"), ("3", "b")))

      val q3 = for(t <- T if (t.a != "1") || (t.b != "a")) yield t
      println("q3: "+q3.selectStatement) // Hah, not what you expect!
      q3.foreach(println _)
      assertEquals(q3.list.toSet, Set(("1", "a"), ("2", "a"), ("3", "b")))

      val q4 = for(t <- T if t.a =!= "1" || t.b =!= "a") yield t
      println("q4: "+q4.selectStatement)
      q4.foreach(println _)
      assertEquals(q4.list.toSet, Set(("2", "a"), ("3", "b")))
    }
  }
}
