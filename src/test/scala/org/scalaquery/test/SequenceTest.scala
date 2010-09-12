package org.scalaquery.test

import org.junit.Test
import org.junit.Assert._
import org.scalaquery.ql._
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql.extended.{ExtendedTable => Table, H2Driver, MySQLDriver, DerbyDriver}
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.test.util._
import org.scalaquery.test.util.TestDB._

object SequenceTest extends DBTestObject(H2Mem, Postgres, MySQL, DerbyMem, HsqldbMem)

class SequenceTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit._

  @Test def test1(): Unit = db withSession {
    case class User(id: Int, first: String, last: String)

    object Users extends Table[Int]("users") {
      def id = column[Int]("id", O PrimaryKey)
      def * = id
    }

    val mySequence = Sequence[Int]("mysequence") start 200 inc 10

    val ddl = Users.ddl ++ mySequence.ddl
    ddl.createStatements.foreach(println)
    ddl.create
    Users.insertAll(1, 2, 3)

    val q1 = for(u <- Users) yield mySequence.next ~ u.id
    println("q1: " + q1.selectStatement)
    assertEquals(Set((200, 1), (210, 2), (220, 3)), q1.list.toSet)
  }

  @Test def test2(): Unit = db withSession {
    val s1 = Sequence[Int]("s1")
    val s2 = Sequence[Int]("s2") start 3
    val s3 = Sequence[Int]("s3") start 3 inc 2
    val s4 = Sequence[Int]("s4") start 3 min 2 max 5 cycle
    val s5 = Sequence[Int]("s5") start 3 min 2 max 5 inc -1 cycle
    val s6 = Sequence[Int]("s6") start 3 min 2 max 5

    def values(s: Sequence[Int], count: Int = 5, create: Boolean = true) = {
      if(create) {
        val ddl = s.ddl
        ddl.createStatements.foreach(println)
        ddl.create
      }
      val q = Query(s.next)
      println(q.selectStatement)
      1 to count map (_ => q.first)
    }

    assertEquals(List(1, 2, 3, 4, 5), values(s1))
    assertEquals(List(3, 4, 5, 6, 7), values(s2))
    assertEquals(List(3, 5, 7, 9, 11), values(s3))
    if(tdb.driver != H2Driver) {
      // H2 does not support MINVALUE, MAXVALUE and CYCLE
      if(tdb.driver != DerbyDriver) {
        // Cycling is broken in Derby. It cycles to the start value instead of min or max
        assertEquals(List(3, 4, 5, 2, 3), values(s4))
        assertEquals(List(3, 2, 5, 4, 3), values(s5))
      }
      if(tdb.driver != MySQLDriver) {
        // MySQL sequence emulation does not support non-cycling limited sequences
        assertEquals(List(3, 4, 5), values(s6, 3))
        assertFail(values(s6, 1, false))
      }
    }
  }
}
