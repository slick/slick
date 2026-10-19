package slick.test.jdbc

import org.junit.Assert._
import org.junit.Test

import slick.jdbc.SQLServerProfile

/** SQL Server converts plain '...' literals to the database code page; non-ASCII ones need the N prefix. */
class SQLServerUnicodeLiteralTest {
  import SQLServerProfile.api._

  @Test def testStringLiteral(): Unit = {
    val st = SQLServerProfile.columnTypes.stringJdbcType
    assertEquals("'abc'", st.valueToSQLLiteral("abc"))
    assertEquals("'it''s'", st.valueToSQLLiteral("it's"))
    assertEquals("''", st.valueToSQLLiteral(""))
    assertEquals("NULL", st.valueToSQLLiteral(null))
    assertEquals("N'Björn'", st.valueToSQLLiteral("Björn"))
    assertEquals("N'Bj''örn 中'", st.valueToSQLLiteral("Bj'örn 中"))
    assertEquals("N'ä'", SQLServerProfile.columnTypes.charJdbcType.valueToSQLLiteral('ä'))
  }

  @Test def testStatements(): Unit = {
    class T(tag: Tag) extends Table[(Int, String)](tag, "t") {
      def id = column[Int]("id")
      def s = column[String]("s", O.Default("Björn"))
      def * = (id, s)
    }
    val ts = TableQuery[T]
    val ddl = ts.schema.createStatements.mkString
    assertTrue(ddl, ddl.contains("DEFAULT N'Björn'"))
    val q1 = ts.filter(_.s === "Björn 中").result.statements.mkString
    assertTrue(q1, q1.contains("N'Björn 中'"))
    val q2 = ts.filter(_.s === "abc").result.statements.mkString
    assertTrue(q2, q2.contains("'abc'") && !q2.contains("N'abc'"))
    val q3 = ts.map(_ => LiteralColumn("ä")).result.statements.mkString
    assertTrue(q3, q3.contains("N'ä'"))
  }
}
