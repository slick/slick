package slick.test.lifted

import org.junit.Assert.*
import org.junit.Test

import slick.jdbc.{H2Profile, JdbcProfile, SQLiteProfile}

/** Pins where single-table SELECTs get a table alias (https://github.com/slick/slick/issues/1374,
  * https://github.com/slick/slick/issues/1402). */
class TableAliasStatementTest {
  private class Statements(val profile: JdbcProfile) {
    import profile.api.*
    class T(tag: Tag) extends Table[(Int, String)](tag, "t") {
      def id = column[Int]("id")
      def content = column[String]("content")
      def * = (id, content)
    }
    val ts = TableQuery[T]
    class U(tag: Tag) extends Table[(Int, String)](tag, "u") {
      def id = column[Int]("id")
      def content = column[String]("content")
      def * = (id, content)
    }
    val us = TableQuery[U]

    private def oneLine(s: String) = s.replaceAll("\\s+", " ")
    val select = oneLine(ts.filter(_.id === 1).result.statements.head)
    val selfDelete = oneLine(ts.filter(t => ts.filter(_.content === t.content).length > 1).delete.statements.head)
    val selfUpdate = oneLine(
      ts.filter(t => ts.filter(o => o.content === t.content && o.id =!= t.id).exists).map(_.content)
        .update("x").statements.head
    )
    val otherDelete = oneLine(ts.filter(t => us.filter(_.content === t.content).exists).delete.statements.head)
  }

  @Test def testH2(): Unit = {
    val s = new Statements(H2Profile)
    // Unchanged: single-table SELECTs have no alias
    assertEquals("""select "id", "content" from "t" where "id" = 1""", s.select)
    // The subquery reading the DELETE/UPDATE target table is aliased
    assertEquals(
      """delete from "t" where ( select count(1) from "t" x2 where x2."content" = "t"."content" ) > 1""",
      s.selfDelete
    )
    assertEquals(
      """update "t" set "content" = ? where exists( select x2."id", x2."content" from "t" x2 where (x2."content" = "t"."content") and (not (x2."id" = "t"."id")) )""",
      s.selfUpdate
    )
    // Unchanged: a subquery reading another table is not aliased
    assertEquals(
      """delete from "t" where exists( select "id", "content" from "u" where "content" = "t"."content" )""",
      s.otherDelete
    )
  }

  @Test def testSQLite(): Unit = {
    val s = new Statements(SQLiteProfile)
    // SQLite always aliases single-table SELECTs, so that unknown columns are not read as string literals
    assertEquals("""select x2."id", x2."content" from "t" x2 where x2."id" = 1""", s.select)
    assertEquals(
      """delete from "t" where exists( select 1 from "u" x2 where x2."content" = "t"."content" )""",
      s.otherDelete
    )
  }
}
