package com.novocode.squery.test

import org.junit.Test
import org.junit.Assert._
import com.novocode.squery.combinator._
import com.novocode.squery.combinator.TypeMapper._
import com.novocode.squery.combinator.extended.{ExtendedTable => Table}
import com.novocode.squery.session._
import com.novocode.squery.session.Database.threadLocalSession
import com.novocode.squery.test.util._
import com.novocode.squery.test.util.TestDB._

object SequenceTest extends DBTestObject(H2Mem)

class SequenceTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit._

  case class User(id: Int, first: String, last: String)

  object Users extends Table[Int]("users") {
    def id = column[Int]("id", O PrimaryKey, O NotNull)
    def * = id
  }

  val mySequence = Sequence[Int]("mysequence") start 200 inc 10

  @Test def test() {
    db withSession {

      val ddl = Users.ddl ++ mySequence.ddl
      ddl.createStatements.foreach(println)
      ddl.create
      Users.insertAll(1, 2, 3)

      val q1 = for(u <- Users) yield mySequence.next ~ u.id
      println("q1: " + q1.selectStatement)
      assertEquals(Set((200, 1), (210, 2), (220, 3)), q1.list.toSet)
    }
  }
}
