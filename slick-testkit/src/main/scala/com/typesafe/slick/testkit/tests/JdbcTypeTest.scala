package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{JdbcTestDB, AsyncTest}
import java.io.{ObjectInputStream, ObjectOutputStream, ByteArrayOutputStream}
import java.sql.{Blob, Date, Time, Timestamp}
import java.util.UUID
import java.time._
import java.time.temporal.ChronoField
import javax.sql.rowset.serial.SerialBlob

import scala.concurrent.Future

/** Data type related tests which are specific to JdbcProfile */
class JdbcTypeTest extends AsyncTest[JdbcTestDB] {
  import tdb.profile.api._

  def testByteArray = {
    class T(tag: Tag) extends Table[(Int, Array[Byte])](tag, "test_ba") {
      def id = column[Int]("id")
      def data = column[Array[Byte]]("data")
      def * = (id, data)
    }
    val ts = TableQuery[T]

    val as1 = for {
      _ <- ts.schema.create
      _ <- ts += (1, Array[Byte](1,2,3))
      _ <- ts += (2, Array[Byte](4,5))
      r1 <- ts.result.map(_.map{ case (id, data) => (id, data.mkString) }.toSet)
      _ = r1 shouldBe Set((1,"123"), (2,"45"))
    } yield ()
    if(implicitly[ColumnType[Array[Byte]]].hasLiteralForm) {
      as1 >> ts.filter(_.data === Array[Byte](4,5)).map(_.data).to[Set].result.map(_.map(_.mkString)).map(_ shouldBe Set("45"))
    } else as1
  }

  def testByteArrayOption = {
    class T(tag: Tag) extends Table[(Int, Option[Array[Byte]])](tag, "test_baopt") {
      def id = column[Int]("id")
      def data = column[Option[Array[Byte]]]("data")
      def * = (id, data)
    }
    val ts = TableQuery[T]

    seq(
      ts.schema.create,
      ts += (1, Some(Array[Byte](6,7))),
      ifCap(rcap.setByteArrayNull)(ts += (2, None)),
      ifNotCap(rcap.setByteArrayNull)(ts.map(_.id) += 2),
      ts.result.map(_.map { case (id, data) => (id, data.map(_.mkString).getOrElse("")) }.toSet).map(_ shouldBe Set((1,"67"), (2,"")))
    )
  }

  def testBlob = ifCapF(rcap.typeBlob) {
    class T(tag: Tag) extends Table[(Int, Blob)](tag, "test3") {
      def id = column[Int]("id")
      def data = column[Blob]("data")
      def * = (id, data)
    }
    val ts = TableQuery[T]

    val a1 = (
      ts.schema.create >>
      (ts += (1, new SerialBlob(Array[Byte](1,2,3)))) >>
      (ts += (2, new SerialBlob(Array[Byte](4,5)))) >>
      ts.result
    ).transactionally
    val p1 = db.stream(a1).mapResult { case (id, data) => (id, data.getBytes(1, data.length.toInt).mkString) }
    materialize(p1).map(_.toSet shouldBe Set((1,"123"), (2,"45"))) flatMap { _ =>
      val f = materializeAsync[(Int, Blob), (Int, String)](db.stream(ts.result.transactionally, bufferNext = false),
        { case (id, data) => db.io((id, data.getBytes(1, data.length.toInt).mkString)) })
      f.map(_.toSet shouldBe Set((1,"123"), (2,"45")))
    }
  }

  def testMappedBlob = ifCap(rcap.typeBlob) {
    case class Serialized[T](value: T)

    implicit def serializedType[T] = MappedColumnType.base[Serialized[T], Blob]({ s =>
      val b = new ByteArrayOutputStream
      val out = new ObjectOutputStream(b)
      out.writeObject(s.value)
      out.flush
      new SerialBlob(b.toByteArray)
    }, { b =>
      val in = new ObjectInputStream(b.getBinaryStream)
      Serialized[T](in.readObject().asInstanceOf[T])
    })

    class T(tag: Tag) extends Table[(Int, Serialized[List[Int]])](tag, "t") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def b = column[Serialized[List[Int]]]("b")
      def * = (id, b)
    }
    val ts = TableQuery[T]

    seq(
      ts.schema.create,
      ts.map(_.b) ++= Seq(Serialized(List(1,2,3)), Serialized(List(4,5))),
      ts.to[Set].result.map(_ shouldBe Set((1, Serialized(List(1,2,3))), (2, Serialized(List(4,5)))))
    ).transactionally
  }

  private def roundtrip[T : BaseColumnType](tn: String, v: T) = {
    class T1(tag: Tag) extends Table[(Int, T)](tag, tn) {
      def id = column[Int]("id")
      def data = column[T]("data")
      def * = (id, data)
    }
    val t1 = TableQuery[T1]

    seq(
      t1.schema.create,
      t1 += (1, v),
      t1.map(_.data).result.head.map(_ shouldBe v),
      t1.filter(_.data === v).map(_.id).result.headOption.map(_ shouldBe Some(1)),
      t1.filter(_.data =!= v).map(_.id).result.headOption.map(_ shouldBe None),
      t1.filter(_.data === v.bind).map(_.id).result.headOption.map(_ shouldBe Some(1)),
      t1.filter(_.data =!= v.bind).map(_.id).result.headOption.map(_ shouldBe None)
    )
  }

  def testUUID =
    roundtrip[UUID]("uuid_t1", UUID.randomUUID())

  def testDate =
    roundtrip("date_t1", Date.valueOf("2012-12-24"))

  def testLocalDate = ifCap(jcap.javaTime) {
    roundtrip[LocalDate]("local_date_t1", LocalDate.now(ZoneOffset.UTC))
  }

  def testTime =
    roundtrip("time_t1", Time.valueOf("17:53:48"))

  private[this] def timeRoundTrip[T : BaseColumnType](tn: String, v: T) = {
    roundtrip(tn, v) >> {
      class T2(tag: Tag) extends Table[Option[T]](tag, s"${tn}_t2") {
        def t = column[Option[T]]("t")
        def * = t
      }
      val t2 = TableQuery[T2]
      t2.schema.create >> (t2 += None) >> t2.result.head.map(_ shouldBe None)
    }
  }

  def testTimestamp = {
    timeRoundTrip[Timestamp](
      "timestamp_t1",
      Timestamp.valueOf("2012-12-24 17:53:48.0")
    )
  }
  def testLocalTime = ifCap(jcap.javaTime) {
    timeRoundTrip[LocalTime](
      "local_time_hour_gt_10",
      generateTestLocalDateTime().toLocalTime.withHour(14)
    )
  }
  def testLocalTimeWithHourLesserThan10 = ifCap(jcap.javaTime) {
    timeRoundTrip[LocalTime](
      "local_time_hour_lt_10",
      generateTestLocalDateTime().toLocalTime.withHour(5)
    )
  }
  def testInstant = ifCap(jcap.javaTime) {
    timeRoundTrip[Instant](
      "instant_t1",
      generateTestLocalDateTime().withHour(15).toInstant(ZoneOffset.UTC)
    )
  }
  def testInstantWithHourLesserThan10 = ifCap(jcap.javaTime) {
    timeRoundTrip[Instant](
      "instant_with_hour_lt_10",
      generateTestLocalDateTime().withHour(5).toInstant(ZoneOffset.UTC)
    )
  }
  def testLocalDateTimeWithHourLesserThan10 = ifCap(jcap.javaTime) {
    timeRoundTrip[LocalDateTime](
      "local_date_time_hour_lt_10",
      generateTestLocalDateTime().withHour(5)
    )
  }
  def testLocalDateTimeWithHourGreaterThan10 = ifCap(jcap.javaTime) {
    timeRoundTrip[LocalDateTime](
      "local_date_time_hour_gt_10",
      generateTestLocalDateTime().withHour(12)
    )
  }
  def testOffsetTime = ifCap(jcap.javaTime) {
    timeRoundTrip[OffsetTime](
      s"offset_time_tz_utc",
      generateTestLocalDateTime().atOffset(ZoneOffset.UTC).toOffsetTime.withHour(15)
    )
  }
  def testOffsetTimeHourLessThan10UTC = ifCap(jcap.javaTime) {
    timeRoundTrip[OffsetTime](
      s"offset_time_utc_hour_lt_10",
      generateTestLocalDateTime().atOffset(ZoneOffset.UTC).toOffsetTime.withHour(5)
    )
  }
  def testOffsetTimeNegativeOffsetGreaterThan10 = ifCap(jcap.javaTime) {
    // Offset -> -11:00 / -11:00
    timeRoundTrip[OffsetTime](
      s"offset_time_tz_ntz_gt_10",
      generateTestLocalDateTime().atZone(ZoneId.of("Pacific/Samoa")).toOffsetDateTime.toOffsetTime.withHour(15)
    )
  }
  def testOffsetTimeNegativeOffsetLessThan10 = ifCap(jcap.javaTime) {
    // Offset -> -3:00 / -3:00
    timeRoundTrip[OffsetTime](
      s"offset_time_tz_ntz_lt_10",
      generateTestLocalDateTime().atZone(ZoneId.of("Antarctica/Rothera")).toOffsetDateTime.toOffsetTime
    )
  }
  def testOffsetTimePositiveOffsetGreaterThan10 = ifCap(jcap.javaTime) {
    // Offset -> +12:00 / +12:00
    timeRoundTrip[OffsetTime](
      s"offset_time_tz_ptz_gt_10",
      generateTestLocalDateTime().atZone(ZoneId.of("Pacific/Wallis")).toOffsetDateTime.toOffsetTime
    )
  }
  def testOffsetTimePositiveOffsetLessThan10 = ifCap(jcap.javaTime) {
    // Offset -> +2:00 / +2:00
    timeRoundTrip[OffsetTime](
      s"offset_time_tz_ptz_lt_10",
      generateTestLocalDateTime().atZone(ZoneId.of("Africa/Johannesburg")).toOffsetDateTime.toOffsetTime
    )
  }
  def testOffsetDateTime = ifCap(jcap.javaTime) {
    timeRoundTrip[OffsetDateTime](
      s"offset_date_time_tz_utc",
      generateTestLocalDateTime().atOffset(ZoneOffset.UTC).withHour(15)
    )
  }
  def testOffsetDateTimeWithHourLesserThan10 = ifCap(jcap.javaTime) {
    timeRoundTrip[OffsetDateTime](
      s"offset_date_time_hour_lt_10",
      generateTestLocalDateTime().atOffset(ZoneOffset.UTC).withHour(5)
    )
  }
  def testOffsetDateTimeNegativeGreaterThan10 = ifCap(jcap.javaTime) {
    // Offset -> -11:00 / -11:00
    timeRoundTrip[OffsetDateTime](
      s"offset_date_time_ntz_gt_10",
      generateTestLocalDateTime().atZone(ZoneId.of("Pacific/Samoa")).toOffsetDateTime.withHour(15)
    )
  }
  def testOffsetDateTimeNegativeLessThan10 = ifCap(jcap.javaTime) {
    // Offset -> -3:00 / -3:00
    timeRoundTrip[OffsetDateTime](
      s"offset_date_time_ntz_lt_10",
      generateTestLocalDateTime().atZone(ZoneId.of("Africa/Addis_Ababa")).toOffsetDateTime.withHour(15)
    )
  }
  def testOffsetDateTimePositiveGreaterThan10 = ifCap(jcap.javaTime) {
    // Offset -> +12:00 / +12:00
    timeRoundTrip[OffsetDateTime](
      s"offset_date_time_ptz_gt_10",
      generateTestLocalDateTime().atZone(ZoneId.of("Pacific/Wallis")).toOffsetDateTime.withHour(15)
    )
  }
  def testOffsetDateTimePositiveLessThan10 = ifCap(jcap.javaTime) {
    // Offset -> +2:00 / +2:00
    timeRoundTrip[OffsetDateTime](
      s"offset_date_time_ptz_lt_10",
      generateTestLocalDateTime().atZone(ZoneId.of("Africa/Johannesburg")).toOffsetDateTime.withHour(15)
    )
  }
  def testZonedDateTime = ifCap(jcap.javaTime) {
    timeRoundTrip[ZonedDateTime](
      s"zoned_date_time_tz_utc",
      generateTestLocalDateTime().atOffset(ZoneOffset.UTC).toZonedDateTime.withHour(14)
    )
  }
  def testZonedDateTimeWithHourLesserThan10 = ifCap(jcap.javaTime) {
    timeRoundTrip[ZonedDateTime](
      s"zoned_date_time_hour_lt_10",
      generateTestLocalDateTime().atOffset(ZoneOffset.UTC).toZonedDateTime.withHour(5)
    )
  }
  def testZonedDateTimeNegativeGreaterThan10 = ifCap(jcap.javaTime) {
    // Offset -> -11:00 / -11:00
    timeRoundTrip[ZonedDateTime](
      s"zoned_date_time_ntz_gt_10",
      generateTestLocalDateTime().atZone(ZoneId.of("Pacific/Samoa")).withHour(5)
    )
  }
  def testZonedDateTimeNegativeLessThan10 = ifCap(jcap.javaTime) {
    // Offset -> -3:00 / -3:00
    timeRoundTrip[ZonedDateTime](
      s"zoned_date_time_ntz_lt_10",
      generateTestLocalDateTime().atZone(ZoneId.of("Pacific/Wallis")).withHour(5)
    )
  }
  def testZonedDateTimePositiveGreaterThan10 = ifCap(jcap.javaTime) {
    // Offset -> +12:00 / +12:00
    timeRoundTrip[ZonedDateTime](
      s"zoned_date_time_ptz_gt_10",
      generateTestLocalDateTime().atZone(ZoneId.of("Pacific/Wallis")).withHour(5)
    )
  }
  def testZonedDateTimePositiveLessThan10 = ifCap(jcap.javaTime) {
    // Offset -> +2:00 / +2:00
    timeRoundTrip[ZonedDateTime](
      s"zoned_date_time_ptz_lt_10",
      generateTestLocalDateTime().atZone(ZoneId.of("Africa/Johannesburg")).withHour(5)
    )
  }

  /**
    * Generates a [[LocalDateTime]] used for the [[java.time]] type tests.
    * The generated test [[LocalDateTime]] will adapt to the database system being used.
    * If the SQL server driver `jtds` is used, there would be a 3 millisecond rounding, so
    * this method will generate a [[LocalDateTime]], using [[LocalDateTime#now]] whose milliseconds
    * ends either 0, 3 or 7. It will just return [[LocalDateTime#now]] if any other driver or database
    * is being used.
    *
    * For more information about the MsSQL issue: https://sourceforge.net/p/jtds/feature-requests/73/
    */
  private[this] def generateTestLocalDateTime() : LocalDateTime = {
    if (tdb.confName.contains("jtds")) {
      val now = Instant.now
      val offset = now.get(ChronoField.MILLI_OF_SECOND) % 10 match {
        case (1|4|8) => -1
        case (2|5|9) => -2
        case 6 => 1
        case _ => 0
      }
      LocalDateTime.ofInstant(now.plusMillis(offset), ZoneOffset.UTC)
    } else
      LocalDateTime.now(ZoneOffset.UTC)
  }

  def testOverrideIdentityType = {
    class T1(tag: Tag) extends Table[Int](tag, "t1") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc, O.SqlType("_FOO_BAR_"))
      def * = id
    }
    val t1 = TableQuery[T1]
    t1.schema.createStatements.mkString.should(_ contains "_FOO_BAR_")
    Future.successful(())
  }
}
