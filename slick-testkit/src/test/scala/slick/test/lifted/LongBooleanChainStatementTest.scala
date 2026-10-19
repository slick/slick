package slick.test.lifted

import org.junit.Assert._
import org.junit.Test

import slick.jdbc._

/** SQL generation for long `||` / `&&` chains (https://github.com/slick/slick/issues/1606) on
  * every JDBC profile, including the ones that rewrite booleans (Derby, DB2, Oracle, SQL Server). */
class LongBooleanChainStatementTest {

  val profiles: Seq[JdbcProfile] = Seq(
    H2Profile, HsqldbProfile, DerbyProfile, SQLiteProfile, PostgresProfile, MySQLProfile,
    OracleProfile, DB2Profile, SQLServerProfile)

  class Statements(val profile: JdbcProfile, n: Int) {
    import profile.api._
    class T(tag: Tag) extends Table[(Int, Int, Option[Int])](tag, "t") {
      def id = column[Int]("id")
      def a = column[Int]("a")
      def o = column[Option[Int]]("o")
      def * = (id, a, o)
    }
    val ts = TableQuery[T]
    val us = TableQuery[T]
    val all = Seq(
      ts.filter(t => (1 to n).map(x => t.id === x.bind).reduce(_ || _)).result.statements.head,
      ts.filter(t => (1 to n).map(x => t.id =!= x).reduce(_ && _)).result.statements.head,
      ts.filter(t => (1 to n).map(x => t.o === x).reduce(_ || _)).result.statements.head,
      ts.filter(t => (1 to n).map(x => t.id === x && t.a =!= 1).reduce(_ || _)).result.statements.head,
      // a long conjunction in a join condition is split and recombined by the rewriteJoins phase
      ts.join(us).on((t, u) => (1 to n).map(x => t.id =!= x && u.id =!= x).reduce(_ && _))
        .filter { case (t, u) => (1 to n).map(x => t.a =!= x).reduce(_ && _) }
        .result.statements.head
    )
  }

  @Test def testLongChains(): Unit = {
    for (p <- profiles) {
      val ss = new Statements(p, 5000).all
      assertEquals(p.toString, 4999, ss.head.split(" or ", -1).length - 1)
      ss.foreach(s => assertTrue(p.toString, s.length > 5000))
    }
  }

  /** Nested `or` and `and` are rendered without the redundant inner parentheses. */
  @Test def testShortChainSql(): Unit = {
    import H2Profile.api._
    class T(tag: Tag) extends Table[(Int, Int)](tag, "t") {
      def id = column[Int]("id")
      def a = column[Int]("a")
      def * = (id, a)
    }
    val ts = TableQuery[T]
    val or = ts.filter(t => t.id === 1 || t.id === 2 || t.id === 3).result.statements.head.replace('\n', ' ')
    assertEquals("""select "id", "a" from "t" where ("id" = 1) or ("id" = 2) or ("id" = 3)""", or)
    val and = ts.filter(t => t.id === 1 && t.a === 2 && t.a =!= 3).result.statements.head.replace('\n', ' ')
    assertEquals("""select "id", "a" from "t" where ("id" = 1) and ("a" = 2) and (not ("a" = 3))""", and)
    val mixed = ts.filter(t => (t.id === 1 && t.a === 2) || (t.id === 3 && t.a === 4) || t.a === 5).result.statements.head.replace('\n', ' ')
    assertEquals("""select "id", "a" from "t" where (("id" = 1) and ("a" = 2)) or (("id" = 3) and ("a" = 4)) or ("a" = 5)""", mixed)
  }
}
