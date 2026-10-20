package slick.test.lifted

import org.junit.Assert.*
import org.junit.Test

import slick.jdbc.*

/** A NULL literal that is a column of a select list is written as a NULL of its SQL type (#3890). */
class NullLiteralStatementTest {
  private def sql(profile: JdbcProfile): String = {
    import profile.api.*

    class T(tag: Tag) extends Table[(Int, Option[Int])](tag, "t") {
      def a = column[Int]("a")
      def ob = column[Option[Int]]("ob")
      def * = (a, ob)
    }
    val ts = TableQuery[T]
    val q = ts.map { t =>
      (
        t.a,
        LiteralColumn(Option.empty[Int]),
        LiteralColumn(Option.empty[Boolean]),
        LiteralColumn(Option.empty[Int]).bind,
        LiteralColumn(Option.empty[java.sql.Clob]),
        LiteralColumn(Option.empty[BigDecimal]),
        Case.If(t.a > 1).Then(LiteralColumn(Option.empty[Int])).Else(t.ob)
      )
    }

    profile.api.streamableQueryActionExtensionMethods(q).result.statements.mkString.replaceAll("\\s+", " ")
  }

  /** `decimalNull` is how the profile writes a BigDecimal NULL. A Clob NULL has no literal form, so it stays bound. */
  private def assertTypedNulls(profile: JdbcProfile, intType: String, booleanType: String, decimalNull: String): Unit = {
    val s = sql(profile)

    assertTrue(s, s.contains(s", cast(null as $intType), cast(null as $booleanType), ?, ?, $decimalNull, "))
    // A NULL inside an expression is not a select list column
    assertTrue(s, s.contains("then null else"))
  }

  @Test def testTypedNulls(): Unit = {
    assertTypedNulls(H2Profile, "INTEGER", "BOOLEAN", "cast(null as DECIMAL(21,2))")
    assertTypedNulls(PostgresProfile, "INTEGER", "BOOLEAN", "cast(null as DECIMAL(21,2))")
    assertTypedNulls(HsqldbProfile, "INTEGER", "BOOLEAN", "cast(null as DECIMAL(21,2))")
    assertTypedNulls(SQLiteProfile, "INTEGER", "INTEGER", "cast(null as DECIMAL(21,2))")
    // The precision of a DECIMAL union is capped, so a DECIMAL NULL stays untyped
    assertTypedNulls(DB2Profile, "INTEGER", "CHAR(1)", "null")
    assertTypedNulls(SQLServerProfile, "INTEGER", "BIT", "null")
  }

  @Test def testDerbyTypedNulls(): Unit = {
    val s = sql(DerbyProfile)

    // Derby also casts bound NULLs and the NULLs of expressions in the select list
    assertTrue(s, s.contains(", cast(null as INTEGER), cast(null as SMALLINT), cast(null as INTEGER), cast(null as CLOB), cast(null as DECIMAL(21,2)), "))
  }

  @Test def testUntypedNulls(): Unit = {
    for (profile <- Seq(MySQLProfile, OracleProfile)) {
      val s = sql(profile)

      assertTrue(s, s.contains(", null, null, ?, ?, null, "))
      assertFalse(s, s.contains("cast(null"))
    }
  }
}
