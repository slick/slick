package scala.slick.test.jdbc

import org.junit.Test
import org.junit.Assert._
import scala.slick.testutil._
import scala.slick.testutil.TestDBs._
import com.typesafe.slick.testkit.util.JdbcTestDB

object EmbeddingTest extends DBTestObject(H2Mem)

class EmbeddingTest(val tdb: JdbcTestDB) extends DBTest {
  import tdb.profile.backend.Database.dynamicSession

  @Test def testRaw(): Unit = db withDynSession {
    import scala.slick.jdbc.{StaticQuery => Q, GetResult}

    (Q.u + "create table USERS(ID int not null primary key, NAME varchar(255))").execute
    (Q.u + "create table POSTS(ID int not null primary key, NAME varchar(255), UID int not null)").execute
    List(
      (1, "u1"),
      (2, "u2"),
      (3, "u3")
    ).map(Q.u1[(Int, String)] + "insert into USERS values (?, ?)").foreach(_.execute)
    List(
      (1, "p1u1", 1),
      (2, "p2u1", 1),
      (3, "p3u1", 1),
      (4, "p4u2", 2)
    ).map(Q.u1[(Int, String, Int)] + "insert into POSTS values (?, ?, ?)").foreach(_.execute)

    val l1 = (Q(GetResult { r => (r.nextString, r.nextString) }) + """
      select u.NAME, p.NAME
      from USERS u left join POSTS p on u.ID = p.UID
      order by u.NAME, p.NAME
    """).list
    l1 foreach println
    assertEquals(List(
      ("u1", "p1u1"),
      ("u1", "p2u1"),
      ("u1", "p3u1"),
      ("u2", "p4u2"),
      ("u3", null)
    ), l1)

    val l2 = (Q(GetResult { r => (r.nextString, r.view1.to[List](GetResult(_.nextString))) }) + """
      select u.NAME, (u.r0 + p.r0), p.NAME
      from (select *, rownum as r0 from USERS order by NAME) u
        left join (select *, 0 as r0 from POSTS order by NAME) p
        on u.ID = p.UID
      order by u.r0
    """).list
    l2 foreach println
    assertEquals(List(
      ("u1", List("p1u1", "p2u1", "p3u1")),
      ("u2", List("p4u2")),
      ("u3", List())
    ), l2)
  }
}
