package slick.test.lifted

import org.junit.Assert.*
import org.junit.Test

import slick.jdbc.*

/** A NULL literal that is a column of a select list is written as a NULL of its SQL type (#3890). */
class NullLiteralStatementTest {
  private case class sql(profile: JdbcProfile) {

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

    def get = profile.api.streamableQueryActionExtensionMethods(q).result.statements.mkString.replaceAll("\\s+", " ")
  }

  /** `decimalNull` is how the profile writes a BigDecimal NULL. A Clob NULL has no literal form, so it stays bound. */
  private def assertTypedNulls(profile: JdbcProfile, intType: String, booleanType: String, decimalNull: String): Unit = {
    val s = sql(profile).get

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

  private case class dateTimeSql(profile: JdbcProfile) {

    import profile.api.*
    import java.time.{LocalDate, LocalDateTime, LocalTime}

    val q = Query((LiteralColumn(Option.empty[LocalDate]), LiteralColumn(Option.empty[LocalTime]),
      LiteralColumn(Option.empty[LocalDateTime])))

    def get = profile.api.streamableQueryActionExtensionMethods(q).result.statements.mkString.replaceAll("\\s+", " ")
  }

  @Test def testDateTimeNulls(): Unit = {
    val h2 = dateTimeSql(H2Profile).get
    val hsqldb = dateTimeSql(HsqldbProfile).get

    assertTrue(h2, h2.contains("select cast(null as DATE), cast(null as VARCHAR), cast(null as TIMESTAMP)"))
    // HSQLDB writes java.time literals as strings, which do not match a typed NULL in a union
    assertTrue(hsqldb, hsqldb.contains("select null, null, null "))
  }

  @Test def testDerbyTypedNulls(): Unit = {
    val s = sql(DerbyProfile).get

    // Derby also casts bound NULLs and the NULLs of expressions in the select list
    assertTrue(s, s.contains(", cast(null as INTEGER), cast(null as SMALLINT), cast(null as INTEGER), cast(null as CLOB), cast(null as DECIMAL(21,2)), "))
  }

  @Test def testUntypedNulls(): Unit = {
    for (profile <- Seq(MySQLProfile, OracleProfile)) {
      val s = sql(profile).get

      assertTrue(s, s.contains(", null, null, ?, ?, null, "))
      assertFalse(s, s.contains("cast(null"))
    }
  }
}
