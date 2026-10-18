package slick.test.lifted

import java.time.{LocalDateTime, OffsetTime, ZoneOffset}

import org.junit.Assert._
import org.junit.Test

import slick.jdbc.OracleProfile

class OracleLiteralTest {
  import OracleProfile.columnTypes._

  @Test def testLocalDateTimeLiteralKeepsNanos(): Unit = {
    val v = LocalDateTime.of(2020, 1, 2, 3, 4, 5, 123456789)
    assertEquals("TO_TIMESTAMP('2020-01-02 03:04:05.123456789', 'YYYY-MM-DD HH24:MI:SS.FF9')",
      localDateTimeType.valueToSQLLiteral(v))
  }

  @Test def testInstantLiteralKeepsNanos(): Unit = {
    val v = LocalDateTime.of(2020, 1, 2, 3, 4, 5, 123456789).toInstant(ZoneOffset.UTC)
    assertEquals("TO_TIMESTAMP_TZ('2020-01-02 03:04:05.123456789 +00', 'YYYY-MM-DD HH24:MI:SS.FF9 TZH')",
      instantType.valueToSQLLiteral(v))
  }

  @Test def testOffsetTimeLiteralKeepsNanos(): Unit = {
    val v = OffsetTime.of(3, 4, 5, 123456789, ZoneOffset.ofHours(2))
    assertEquals("TO_TIMESTAMP_TZ('1970-01-01 03:04:05.123456789 +0200', 'YYYY-MM-DD HH24:MI:SS.FF9 TZH:TZM')",
      offsetTimeType.valueToSQLLiteral(v))
  }
}
