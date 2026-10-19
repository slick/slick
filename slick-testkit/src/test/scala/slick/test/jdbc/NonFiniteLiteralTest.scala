package slick.test.jdbc

import org.junit.Assert._
import org.junit.Test

import slick.jdbc._

/** NaN and the infinities must not be rendered as bare (identifier-like) SQL literals. */
class NonFiniteLiteralTest {
  val profiles: Seq[JdbcProfile] = Seq(
    PostgresProfile, MySQLProfile, OracleProfile, SQLServerProfile, DB2Profile,
    H2Profile, HsqldbProfile, DerbyProfile, SQLiteProfile
  )

  @Test def testNonFiniteFilterUsesBindVariable(): Unit = profiles.foreach { profile =>
    import profile.api._
    class T(tag: Tag) extends Table[(Double, Float)](tag, "t") {
      def d = column[Double]("d")
      def f = column[Float]("f")
      def * = (d, f)
    }
    val ts = TableQuery[T]
    val queries = Seq(
      ts.filter(_.d === Double.NaN),
      ts.filter(_.d === Double.PositiveInfinity),
      ts.filter(_.d === Double.NegativeInfinity),
      ts.filter(_.f === Float.NaN),
      ts.filter(_.d.? === Option(Double.NaN))
    )
    queries.foreach { q =>
      val s = q.result.statements.mkString
      assertFalse(s"$profile: $s", s.contains("NaN") || s.contains("Infinity"))
      assertTrue(s"$profile: $s", s.contains("?"))
    }
    val finite = ts.filter(_.d === 1.5).result.statements.mkString
    assertTrue(s"$profile: $finite", finite.contains("1.5") && !finite.contains("?"))
  }

  @Test def testNonFiniteLiteral(): Unit = {
    assertEquals("cast('NaN' as DOUBLE PRECISION)",
      PostgresProfile.columnTypes.doubleJdbcType.valueToSQLLiteral(Double.NaN))
    assertEquals("cast('Infinity' as DOUBLE PRECISION)",
      PostgresProfile.columnTypes.doubleJdbcType.valueToSQLLiteral(Double.PositiveInfinity))
    assertEquals("cast('-Infinity' as REAL)",
      PostgresProfile.columnTypes.floatJdbcType.valueToSQLLiteral(Float.NegativeInfinity))
    assertEquals("cast('NaN' as BINARY_DOUBLE)",
      OracleProfile.columnTypes.doubleJdbcType.valueToSQLLiteral(Double.NaN))
    assertEquals("1.5", PostgresProfile.columnTypes.doubleJdbcType.valueToSQLLiteral(1.5))

    import PostgresProfile.api._
    class T(tag: Tag) extends Table[Double](tag, "t") {
      def d = column[Double]("d", O.Default(Double.NaN))
      def * = d
    }
    val ddl = TableQuery[T].schema.createStatements.mkString
    assertTrue(ddl, ddl.contains("DEFAULT cast('NaN' as DOUBLE PRECISION)"))
  }
}
