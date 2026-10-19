package slick.test.jdbc

import java.nio.file.Files
import java.sql.Timestamp
import java.time.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global

import com.typesafe.config.ConfigFactory
import org.junit.Assert.*
import org.junit.Test
import slick.cats.Database
import slick.jdbc.{DatabaseConfig, SQLiteProfile}
import slick.jdbc.SQLiteProfile.api.*

/** Checks the storage format of date/time values on SQLite, as described in the SQLiteProfile
  * documentation, with the default sqlite-jdbc settings and with `date_precision = SECONDS`. */
class SQLiteDateStorageTest {
  class T(tag: Tag) extends Table[(Timestamp, Instant, OffsetDateTime)](tag, "t") {
    def ts = column[Timestamp]("ts")
    def inst = column[Instant]("inst")
    def odt = column[OffsetDateTime]("odt")
    def * = (ts, inst, odt)
  }
  val ts = TableQuery[T]

  val instant = Instant.parse("2020-01-02T03:04:05Z")
  val timestamp = Timestamp.from(instant)
  val odt = OffsetDateTime.ofInstant(instant, ZoneOffset.UTC)

  def withDb[A](properties: String)(f: Database => IO[A]): A = {
    val file = Files.createTempFile("slick-sqlite-dates", ".db")
    val conf = ConfigFactory.parseString(
      s"""db {
         |  driver = "org.sqlite.JDBC"
         |  url = "jdbc:sqlite:${file.toString.replace("\\", "/")}"
         |  connectionPool = disabled
         |  properties = { $properties }
         |}""".stripMargin)
    val dc = DatabaseConfig.forProfileConfig(SQLiteProfile, "db", conf)
    val rawDb = dc.profile.backend.makeDatabase[IO](dc).unsafeRunSync()
    try f(Database.fromCore(rawDb)).unsafeRunSync()
    finally { rawDb.close(); Files.deleteIfExists(file) }
  }

  def setup(db: Database): IO[Unit] =
    db.run(ts.schema.create >> (ts += ((timestamp, instant, odt))).map(_ => ()))

  def rawValues = sql"""select typeof("ts"), "ts", typeof("inst"), "inst", typeof("odt"), "odt" from "t"""".
    as[(String, Long, String, Long, String, String)].head

  @Test def testDefaultMilliseconds(): Unit = withDb("") { db =>
    for {
      _ <- setup(db)
      raw <- db.run(rawValues)
      fn <- db.run(sql"""select datetime("inst" / 1000, 'unixepoch') from "t"""".as[String].head)
      literal <- db.run(ts.filter(_.ts === timestamp).length.result)
      bound <- db.run(ts.filter(_.ts === timestamp.bind).length.result)
      back <- db.run(ts.result.head)
    } yield {
      assertEquals(("integer", instant.toEpochMilli, "integer", instant.toEpochMilli, "text", "2020-01-02T03:04:05Z"), raw)
      assertEquals("2020-01-02 03:04:05", fn)
      assertEquals(1, literal)
      assertEquals(1, bound)
      assertEquals((timestamp, instant, odt), back)
    }
  }

  @Test def testDatePrecisionSeconds(): Unit = withDb("date_precision = SECONDS") { db =>
    for {
      _ <- setup(db)
      raw <- db.run(rawValues)
      fn <- db.run(sql"""select datetime("inst", 'unixepoch') from "t"""".as[String].head)
      literal <- db.run(ts.filter(_.ts === timestamp).length.result)
      bound <- db.run(ts.filter(_.ts === timestamp.bind).length.result)
      back <- db.run(ts.result.head)
    } yield {
      assertEquals(("integer", instant.getEpochSecond, "integer", instant.getEpochSecond, "text", "2020-01-02T03:04:05Z"), raw)
      assertEquals("2020-01-02 03:04:05", fn)
      // Slick's literals are always milliseconds, so only bound parameters match (as documented)
      assertEquals(0, literal)
      assertEquals(1, bound)
      assertEquals((timestamp, instant, odt), back)
    }
  }
}
