package com.novocode.squery.test

import org.junit.After
import org.junit.Test
import org.junit.Assert._
import com.novocode.squery.combinator._
import com.novocode.squery.combinator.TypeMapper._
import com.novocode.squery.combinator.extended.SQLiteDriver.Implicit._
import com.novocode.squery.combinator.extended.{ExtendedTable => Table}
import com.novocode.squery.meta.MTable
import com.novocode.squery.session._
import com.novocode.squery.session.Database.threadLocalSession
import com.novocode.squery.simple.StaticQueryBase

object InsertTest { def main(args: Array[String]) = new InsertTest().testSimple() }

class InsertTest {
  @After def cleanUp() = Database.forURL("jdbc:sqlite:sample.db", driver = "org.sqlite.JDBC") withSession {
    MTable.getTables.list.foreach(t => StaticQueryBase.updateNA("drop table " + t.name.name).first)
  }

  class TestTable(name: String) extends Table[(Int, String)](name) {
    def id = column[Int]("id")
    def name = column[String]("name")
    def * = id ~ name
  }

  object Src1 extends TestTable("src1")
  object Dst1 extends TestTable("dst1")
  object Dst2 extends TestTable("dst2")

  @Test def testSimple(): Unit = Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver") withSession {

    (Src1.ddl ++ Dst1.ddl ++ Dst2.ddl) create

    Src1.insert(1, "A")
    Src1.insertAll((2, "B"), (3, "C"))

    Dst1.insert(Src1)
    assertEquals(Set((1,"A"), (2,"B"), (3,"C")), Query(Dst1).list.toSet)

    val q2 = for(s <- Src1 if s.id <= 2) yield s
    println("Insert 2: "+Dst2.insertStatementFor(q2))
    Dst2.insert(q2)
    assertEquals(Set((1,"A"), (2,"B")), Query(Dst2).list.toSet)

    val q3 = 42 ~ "X".bind
    println("Insert 3: "+Dst2.insertStatementFor(q3))
    Dst2.insert(q3)
    assertEquals(Set((1,"A"), (2,"B"), (42,"X")), Query(Dst2).list.toSet)
  }
}
