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

  @Test def testMappedNonFiniteFilterUsesBindVariable(): Unit = profiles.foreach { profile =>
    import profile.api._
    import NonFiniteLiteralTest._
    implicit val wdType: BaseColumnType[WD] = MappedColumnType.base[WD, Double](_.value, WD(_))
    implicit val wfType: BaseColumnType[WF] = MappedColumnType.base[WF, Float](_.value, WF(_))
    class T(tag: Tag) extends Table[(WD, WF, Option[WD])](tag, "t") {
      def d = column[WD]("d")
      def f = column[WF]("f")
      def od = column[Option[WD]]("od")
      def * = (d, f, od)
    }
    val ts = TableQuery[T]
    val queries = Seq(
      ts.filter(_.d === WD(Double.NaN)).result,
      ts.filter(_.d === WD(Double.PositiveInfinity)).result,
      ts.filter(_.d === WD(Double.NegativeInfinity)).result,
      ts.filter(_.f === WF(Float.PositiveInfinity)).result,
      ts.filter(_.od === Option(WD(Double.NaN))).result,
      ts.map(_ => WD(Double.PositiveInfinity)).result
    )
    queries.foreach { q =>
      val s = q.statements.mkString
      assertFalse(s"$profile: $s", s.contains("NaN") || s.contains("Infinity"))
      assertTrue(s"$profile: $s", s.contains("?"))
    }
    val finite = ts.filter(_.d === WD(1.5)).result.statements.mkString
    assertTrue(s"$profile: $finite", finite.contains("1.5") && !finite.contains("?"))
  }

  @Test def testTypeWithoutLiteralFormIsAlwaysBound(): Unit = profiles.foreach { profile =>
    import profile.api._
    // A subclass that opts out of literals for the whole type must not get them back per value
    implicit val noLiteralDouble: profile.columnTypes.DoubleJdbcType = new profile.columnTypes.DoubleJdbcType {
      override def hasLiteralForm = false
    }
    class T(tag: Tag) extends Table[Double](tag, "t") {
      def d = column[Double]("d")
      def * = d
    }
    val ts = TableQuery[T]
    Seq(ts.filter(_.d === 1.5).result, ts.filter(_.d.? === Option(1.5)).result).foreach { q =>
      val s = q.statements.mkString
      assertTrue(s"$profile: $s", s.contains("?") && !s.contains("1.5"))
    }
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

    import NonFiniteLiteralTest._
    implicit val wdType: BaseColumnType[WD] = MappedColumnType.base[WD, Double](_.value, WD(_))
    class M(tag: Tag) extends Table[WD](tag, "m") {
      def d = column[WD]("d", O.Default(WD(Double.NegativeInfinity)))
      def * = d
    }
    val mappedDdl = TableQuery[M].schema.createStatements.mkString
    assertTrue(mappedDdl, mappedDdl.contains("DEFAULT cast('-Infinity' as DOUBLE PRECISION)"))
  }
}

object NonFiniteLiteralTest {
  case class WD(value: Double)
  case class WF(value: Float)
}
