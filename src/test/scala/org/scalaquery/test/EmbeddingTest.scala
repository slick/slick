package org.scalaquery.test

import org.junit.Test
import org.junit.Assert._
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.test.util._
import org.scalaquery.test.util.TestDB._
import collection.mutable.ListBuffer

object EmbeddingTest extends DBTestObject(H2Mem)

class EmbeddingTest(tdb: TestDB) extends DBTest(tdb) {
  //import tdb.driver.Implicit._

  @Test def testRaw(): Unit = db withSession {
    import org.scalaquery.simple._
    import org.scalaquery.simple.StaticQuery._

    updateNA("create table USERS(ID int not null primary key, NAME varchar(255))").execute
    updateNA("create table POSTS(ID int not null primary key, NAME varchar(255), UID int not null)").execute
    List(
      (1, "u1"),
      (2, "u2"),
      (3, "u3")
    ).foreach(t => update[(Int, String)]("insert into USERS values (?, ?)").execute(t))
    List(
      (1, "p1u1", 1),
      (2, "p2u1", 1),
      (3, "p3u1", 1),
      (4, "p4u2", 2)
    ).foreach(t => update[(Int, String, Int)]("insert into POSTS values (?, ?, ?)").execute(t))

    val l1 = queryNA("""
      select u.NAME, p.NAME
      from USERS u left join POSTS p on u.ID = p.UID
      order by u.NAME, p.NAME
    """)(GetResult { r =>
      (r.nextString, r.nextString)
    }).list
    l1 foreach println
    assertEquals(List(
      ("u1", "p1u1"),
      ("u1", "p2u1"),
      ("u1", "p3u1"),
      ("u2", "p4u2"),
      ("u3", null)
    ), l1)

    val l2 = queryNA("""
      select u.NAME, (u.r0 + p.r0), p.NAME
      from (select *, rownum as r0 from USERS order by NAME) u
        left join (select *, 0 as r0 from POSTS order by NAME) p
        on u.ID = p.UID
      order by u.r0
    """)(GetResult { r =>
      (r.nextString, r.view1.to[List](GetResult(_.nextString)))
    }).list
    l2 foreach println
    assertEquals(List(
      ("u1", List("p1u1", "p2u1", "p3u1")),
      ("u2", List("p4u2")),
      ("u3", List())
    ), l2)
  }
}
