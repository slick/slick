package slick.test.jdbc

import java.sql.{DriverManager, Timestamp}
import java.time.{LocalDate, LocalDateTime, LocalTime, ZoneOffset}
import java.util.TimeZone

import org.junit.Assert._
import org.junit.Test

import slick.jdbc._

/** Literals of types declared as DATE, TIME or TIMESTAMP must be typed SQL literals, not plain strings. */
class TemporalLiteralTest {
  val date = LocalDate.of(2020, 1, 2)
  val time = LocalTime.of(3, 4, 5, 123456789)
  val dateTime = LocalDateTime.of(2020, 1, 2, 3, 4, 5, 123456789)
  val instant = dateTime.toInstant(ZoneOffset.UTC)

  @Test def testJdbcEscapes(): Unit = Seq[JdbcProfile](H2Profile, DerbyProfile, DB2Profile).foreach {
    profile =>
      import profile.columnTypes._
      assertEquals(s"$profile", "{d '2020-01-02'}", localDateType.valueToSQLLiteral(date))
      assertEquals(s"$profile", "{ts '2020-01-02 03:04:05.123456789'}", localDateTimeType.valueToSQLLiteral(dateTime))
  }

  @Test def testHsqldb(): Unit = {
    import HsqldbProfile.columnTypes._
    assertEquals("{d '2020-01-02'}", localDateType.valueToSQLLiteral(date))
    // Truncated to the precision of TIMESTAMP, like a bound value
    assertEquals("{ts '2020-01-02 03:04:05.123456'}", localDateTimeType.valueToSQLLiteral(dateTime))
  }

  /** The literal must denote the value that setValue binds, which goes through java.sql.Date or Timestamp */
  @Test def testLiteralsNormalizeLikeBoundValues(): Unit = {
    val default = TimeZone.getDefault
    try {
      TimeZone.setDefault(TimeZone.getTimeZone("Europe/Berlin"))
      // 02:30 falls in the daylight saving gap; Timestamp.valueOf moves it to 03:30
      val inGap = LocalDateTime.of(2020, 3, 29, 2, 30)
      assertEquals("2020-03-29 03:30:00.0", Timestamp.valueOf(inGap).toString)
      assertEquals("{ts '2020-03-29 03:30:00.0'}", H2Profile.columnTypes.localDateTimeType.valueToSQLLiteral(inGap))
      assertEquals("{ts '2020-03-29 03:30:00.0'}", HsqldbProfile.columnTypes.localDateTimeType.valueToSQLLiteral(inGap))
      assertEquals("(convert(datetime2(6), '2020-03-29 03:30:00'))",
        SQLServerProfile.columnTypes.localDateTimeType.valueToSQLLiteral(inGap))
    } finally TimeZone.setDefault(default)
    // 1582-10-10 does not exist in the Julian-Gregorian calendar of java.sql.Date, which moves it to 1582-10-20
    val skipped = LocalDate.of(1582, 10, 10)
    assertEquals("{d '1582-10-20'}", H2Profile.columnTypes.localDateType.valueToSQLLiteral(skipped))
    assertEquals("(convert(date, {d '1582-10-20'}))", SQLServerProfile.columnTypes.localDateType.valueToSQLLiteral(skipped))
  }

  /** HSQLDB truncates bound values to the column's precision; a literal must find the same rows */
  @Test def testHsqldbLiteralMatchesBoundValue(): Unit = {
    import HsqldbProfile.columnTypes._
    val conn = DriverManager.getConnection("jdbc:hsqldb:mem:temporalliteraltest")
    try {
      val st = conn.createStatement()
      st.execute(
        s"create table t (t ${localTimeType.sqlTypeName(None)}, dt ${localDateTimeType.sqlTypeName(None)})")
      val ins = conn.prepareStatement("insert into t values (?, ?)")
      localTimeType.setValue(time, ins, 1)
      localDateTimeType.setValue(dateTime, ins, 2)
      ins.executeUpdate()
      def count(where: String) = {
        val rs = st.executeQuery(s"select count(*) from t where $where")

        rs.next()
        rs.getInt(1)
      }

      assertEquals(1, count(s"t = ${localTimeType.valueToSQLLiteral(time)}"))
      assertEquals(1, count(s"dt = ${localDateTimeType.valueToSQLLiteral(dateTime)}"))
    } finally conn.close()
  }

  @Test def testMySQL(): Unit = {
    import MySQLProfile.columnTypes._
    assertEquals("{d '2020-01-02'}", localDateType.valueToSQLLiteral(date))
    // LocalDateTime and Instant are stored as VARCHAR on MySQL
    assertEquals("'2020-01-02T03:04:05.123456789'", localDateTimeType.valueToSQLLiteral(dateTime))
    assertEquals("'2020-01-02T03:04:05.123456789Z'", instantType.valueToSQLLiteral(instant))
  }

  @Test def testH2Instant(): Unit =
    assertEquals("TIMESTAMP WITH TIME ZONE '2020-01-02T03:04:05.123456789Z'",
      H2Profile.columnTypes.instantType.valueToSQLLiteral(instant))

  @Test def testLocalTime(): Unit = {
    // LocalTime is stored as VARCHAR, except on HSQLDB (TIME(3)) and SQL Server (TIME(6))
    assertEquals("'03:04:05.123456789'", H2Profile.columnTypes.localTimeType.valueToSQLLiteral(time))
    assertEquals("TIME '03:04:05.123'", HsqldbProfile.columnTypes.localTimeType.valueToSQLLiteral(time))
    assertEquals("(convert(time(6), '03:04:05.123456789'))",
      SQLServerProfile.columnTypes.localTimeType.valueToSQLLiteral(time))
  }

  @Test def testHsqldbInstantDefault(): Unit =
    // HSQLDB binds Instant values; the literal is only used for column defaults
    assertEquals("'2020-01-02 3:04:05.123456789+0:00'", HsqldbProfile.columnTypes.instantType.valueToSQLLiteral(instant))

  @Test def testOracleLocalDate(): Unit =
    assertEquals("TO_DATE('2020-01-02', 'SYYYY-MM-DD')", OracleProfile.columnTypes.localDateType.valueToSQLLiteral(date))

  @Test def testSQLServer(): Unit = {
    import SQLServerProfile.columnTypes._
    assertEquals("(convert(date, {d '2020-01-02'}))", localDateType.valueToSQLLiteral(date))
    // SQL Server converts a string with at most 7 fractional digits to DATETIME2
    assertEquals("(convert(datetime2(6), '2020-01-02 03:04:05.1234567'))", localDateTimeType.valueToSQLLiteral(dateTime))
    assertEquals("(convert(datetime2(6), '2020-01-02 03:04:05.1234567'))",
      timestampJdbcType.valueToSQLLiteral(Timestamp.valueOf(dateTime)))
    assertEquals("(convert(datetime2(6), '2020-01-02 03:04:05'))",
      localDateTimeType.valueToSQLLiteral(dateTime.withNano(0)))
  }

  @Test def testPostgresBindsTemporalValues(): Unit = {
    import PostgresProfile.columnTypes._
    assertFalse(localDateType.hasLiteralForm)
    assertFalse(localDateTimeType.hasLiteralForm)
    assertFalse(instantType.hasLiteralForm)
  }

  @Test def testHsqldbQueryStatement(): Unit = {
    import HsqldbProfile.api._
    class T(tag: Tag) extends Table[(LocalDate, LocalDateTime)](tag, "t") {
      def d = column[LocalDate]("d")
      def dt = column[LocalDateTime]("dt")
      def * = (d, dt)
    }
    val ts = TableQuery[T]
    val s = (ts.map(_.d) ++ ts.map(_ => LiteralColumn(date))).result.statements.mkString
    assertTrue(s, s.contains("{d '2020-01-02'}"))
    val s2 = ts.filter(_.dt === LiteralColumn(dateTime)).result.statements.mkString
    assertTrue(s2, s2.contains("{ts '2020-01-02 03:04:05.123456'}"))
  }
}
