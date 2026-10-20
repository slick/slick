package com.typesafe.slick.testkit.tests

import java.io.{ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import java.sql.{Blob, Clob, Date, SQLFeatureNotSupportedException, Time, Timestamp}
import java.time._
import java.time.format.DateTimeFormatter
import java.time.temporal.{ChronoField, ChronoUnit}
import java.util.UUID

import scala.annotation.tailrec
import scala.util.Random

import cats.effect.IO

import slick.ast.FieldSymbol
import slick.jdbc.{H2Profile, HsqldbProfile, PostgresProfile, SQLiteProfile, SQLServerProfile}

import com.typesafe.slick.testkit.util.{AsyncTest, JdbcTestDB}
import javax.sql.rowset.serial.{SerialBlob, SerialClob}


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

  def testUnicodeStringLiteral = {
    // Non-ASCII text in inlined literals (not bind variables) must survive the round trip.
    // SQL Server needs an NVARCHAR column to store it; its default String type is VARCHAR.
    val unicode = "Björn ł Ω 中文"
    val other = "abc"
    val isSQLServer = tdb.profile.isInstanceOf[SQLServerProfile]
    class T(tag: Tag) extends Table[(Int, String)](tag, "test_unicode_lit") {
      def id = column[Int]("id")
      def s =
        if (isSQLServer) column[String]("s", O.SqlType("NVARCHAR(100)"), O.Default(unicode))
        else column[String]("s", O.Length(100), O.Default(unicode))
      def * = (id, s)
    }
    val ts = TableQuery[T]

    seq(
      ts.schema.create,
      ts.map(_.id) += 1,
      ts += (2, other),
      ts.filter(_.id === 1).map(_.s).result.map(_ shouldBe Seq(unicode)),
      ts.filter(_.s === unicode).map(_.id).result.map(_ shouldBe Seq(1)),
      ts.filter(_.s === other).map(_.id).result.map(_ shouldBe Seq(2)),
      ts.filter(_.id === 2).map(t => (t.id, LiteralColumn(unicode))).result.map(_ shouldBe Seq((2, unicode))),
      ts.schema.drop
    )
  }

  def testBlob: IO[Unit] = ifCapF(rcap.typeBlob) {
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
    val p1 = db.stream(a1).map { case (id, data) => (id, data.getBytes(1, data.length.toInt).mkString) }
    materialize(p1).map(_.toSet shouldBe Set((1,"123"), (2,"45"))) flatMap { _ =>
      val f = materializeAsync[(Int, Blob), (Int, String)](db.stream(ts.result.transactionally),
        { case (id, data) => IO.pure((id, data.getBytes(1, data.length.toInt).mkString)) })
      f.map(_.toSet shouldBe Set((1,"123"), (2,"45")))
    }
  }

  def testClob: IO[Unit] = {
    // The SQLite JDBC driver does not implement Clob
    if(tdb.profile.isInstanceOf[SQLiteProfile]) return IO.unit
    class T(tag: Tag) extends Table[(Int, Clob)](tag, "test_clob") {
      def id = column[Int]("id")
      def data = column[Clob]("data")
      def * = (id, data)
    }
    val ts = TableQuery[T]

    // Some drivers (e.g. Oracle) only accept their own Clob implementation, others (e.g. PostgreSQL)
    // do not implement createClob
    def clob(s: String): DBIO[Clob] = SimpleDBIO { ctx =>
      try {
        val c = ctx.connection.createClob()
        c.setString(1, s)
        c
      } catch { case _: SQLFeatureNotSupportedException => new SerialClob(s.toCharArray) }
    }

    val a1 = (
      ts.schema.create >>
      clob("abc").flatMap(c => ts += (1, c)) >>
      clob("de").flatMap(c => ts += (2, c)) >>
      ts.result
    ).transactionally
    val p1 = db.stream(a1).map { case (id, data) => (id, data.getSubString(1, data.length.toInt)) }
    materialize(p1).map(_.toSet shouldBe Set((1,"abc"), (2,"de")))
  }

  def testClobNullLiteral = {
    class T(tag: Tag) extends Table[Int](tag, "test_clob_null") {
      def id = column[Int]("id")
      def * = id
    }
    val ts = TableQuery[T]

    // DB2 rejects a parameter in the select list whose type it cannot infer (SQLCODE=-418)
    val untypedParametersInSelect = tdb.profile != slick.jdbc.DB2Profile

    // A Clob has no literal form, so a NULL Clob in a select list is a bind variable
    seq(
      ts.schema.create,
      ts += 1,
      if (untypedParametersInSelect) ts.map(t => (t.id, LiteralColumn(Option.empty[Clob]))).result.map(_ shouldBe Seq((1, None)))
      else DBIO.successful(())
    ).andFinally(ts.schema.drop)
  }

  def testMappedBlob = ifCap(rcap.typeBlob) {
    case class Serialized[T](value: T)

    implicit def serializedType[T]: BaseColumnType[Serialized[T]] = MappedColumnType.base[Serialized[T], Blob]({ s =>
      val b = new ByteArrayOutputStream
      val out = new ObjectOutputStream(b)
      out.writeObject(s.value)
      out.flush()
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

  def testUUID =
    roundTrip[UUID](List(UUID.randomUUID()), UUID.randomUUID)

  /**
    *
    * @param values List of static values, useful for potentially known problem values
    * @param dataCreateFn generate random values. If these fail intermittently, don't just re-run. It is highlighting
    *                     a real issue. These should never fail.
    * @param dataCompareFn Optional compare function
    * @tparam T The type to test
    */
  private def roundTrip[T: BaseColumnType](values: List[T],
                                           dataCreateFn: () => T,
                                           dataCompareFn: (Int, Option[T], Option[T]) => Unit =
                                           (id: Int, l: Option[T], r: Option[T]) => (id, l) shouldBe ((id, r)),
                                           tableNameSuffix: String = ""): IO[Unit] = {
    // How many random values to generate and test with
    val testValuesSize = 1000
    val rows = (1 to testValuesSize).map(i => (i, Some(dataCreateFn())))
    val updateValue = dataCreateFn()
    val insertValue = dataCreateFn()
    val defaultValue = dataCreateFn()

    val tableName = "Data_" + values.headOption.getOrElse(dataCreateFn()).getClass.getSimpleName + tableNameSuffix
    class DataTable(tag: Tag) extends Table[(Int, Option[T])](tag, tableName) {
      def id = column[Int]("ID", O.PrimaryKey)
      def data = column[Option[T]]("DATA", O.Default(Some(defaultValue)))
      def * = (id, data)
    }
    val dateTable = TableQuery[DataTable]

    val staticValues = values.zipWithIndex.map(x => (x._2, Some(x._1)))
    db.run(seq(
      dateTable.schema.create,
      dateTable ++= staticValues,
      dateTable.sortBy(_.id).result.map(_.zip(staticValues).
        foreach{case ((lId, lValue), (rId, rValue)) => dataCompareFn(lId, lValue, rValue)}),
      // select based on value literal
      dateTable.filter(r => r.data === values.head && r.id === 0).map(_.id).result.headOption.map(_ shouldBe Some(0)),
      dateTable.filter(r => r.data =!= values.head && r.id === 0).map(_.id).result.headOption.map(_ shouldBe None),
      // select based on value binding
      dateTable.filter(r => r.data === values.head.bind && r.id === 0).map(_.id).result.headOption.map(_ shouldBe Some(0)),
      dateTable.filter(r => r.data =!= values.head.bind && r.id === 0).map(_.id).result.headOption.map(_ shouldBe None)
    )).flatMap { _ =>
      // update value
      val newValue = dataCreateFn()
      db.run(seq(
        dateTable.filter(_.id === 0).map(_.data).update(Some(newValue)),
        dateTable.filter(_.id === 0).result.head.map{case (id, v) => dataCompareFn(id, v, Some(newValue))}
      ))
    }.flatMap { _ =>
      // add and select a null value
      db.run(seq(
        dateTable += (values.size + 1, None),
        dateTable.filter(_.id === values.size + 1).map(_.data).result.head.map(_ shouldBe None)
      ))
    }.flatMap { _ =>
      // filter on a LiteralColumn value
      db.run(seq(
        dateTable += (values.size + 2, Some(defaultValue)),
        dateTable.filter(_.data === LiteralColumn(defaultValue)).map(_.data).result.head.map(_ shouldBe Some(defaultValue))
      ))
    }.flatMap { _ =>
      ifCapF(jcap.mutable) {
        db.run(seq(dateTable.delete, dateTable ++= rows)).flatMap { _ =>
          foreach(db.stream(dateTable.mutate.transactionally)) { m =>
            if (!m.end) {
              // update value for id 1
              if (m.row._1 == 1) m.row = m.row.copy(_2 = Some(updateValue))
              // delete id 2
              else if (m.row._1 == 2) m.delete
              //set id 3 value to NULL
              else if (m.row._1 == 3) m.row = m.row.copy(_2 = None)
              else if (m.row._1 == 4) {
                // insert 2 new rows, one with a value and one NULL
                m += (rows.size + 1, Some(insertValue))
                m += (rows.size + 2, None)
              }
            }
          }
        }.flatMap { _ =>
          db.run(dateTable.sortBy(_.id).result).map(_.zip(
            Seq((1, Some(updateValue)), (3, None)) ++
              rows.slice(3, rows.size) ++
              Seq((testValuesSize + 1, Some(insertValue)), (testValuesSize + 2, None))).
            foreach { case ((lId, lValue), (rId, rValue)) => dataCompareFn(lId, lValue, rValue) }
          )
        }
      }
    }
  }

  val random = Random
  private def randomLocalDateTime() = {
    val seconds = 100000000
    now.plusSeconds(random.nextInt(seconds*2) - seconds)
  }
  lazy val now = generateTestLocalDateTime()
  /**
   * Generates a [[LocalDateTime]] used for the [[java.time]] type tests.
   * The generated test [[LocalDateTime]] will adapt to the database system being used.
   * Older version of MySQL have no millisecond resolution, so set all the ms component
   * of the LocalDateTime to 000.
   */
  private def generateTestLocalDateTime(): LocalDateTime = {
    val now = Instant.now
    val dbCompatibleInstant =
      if (tdb.confName.contains("mysql")) {
        // mysql has no subsecond resolution
        now.`with`(ChronoField.NANO_OF_SECOND, 0)
      } else {
        // after JDK8, Instant.now uses a Clock that has greater than millis resolution. Most
        // database timestamp implementations only have millis resolutions, so only
        // roundtrip values which have no micro and nano parts
        now.`with`(ChronoField.MILLI_OF_SECOND, now.get(ChronoField.MILLI_OF_SECOND))
      }
    LocalDateTime.ofInstant(dbCompatibleInstant, ZoneOffset.UTC)
  }
  val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss")

  // Test the java.sql.* types
  def testDate =
    roundTrip[Date](
      List(Date.valueOf("2012-12-24")),
      () => Date.valueOf(randomLocalDateTime().toLocalDate)
    )

  def testTime =
    roundTrip[Time](
      List(Time.valueOf("17:53:48")),
      () => Time.valueOf(randomLocalDateTime().toLocalTime)
    )

  // both the Timestamp and the LocalDateTime tests allow a difference in roundtripping the values of exactly
  // one hour. This will happen during a DST shift, either backwards or forwards an hour and may happen depending
  // on the configuration of some combination of the database backend, OS and JVM.
  // This is far from ideal, but reflects the reality of mapping into a timestamp without time zone datatype in the db.
  // All the other time datatypes roundtrip cleanly.
  val hourInMs = 3600000
  def testTimestamp = {
    def timestampCompare(id: Int, l: Option[Timestamp], r: Option[Timestamp]) = {
      (l, r) match {
        case (Some(l), Some(r)) =>
          val lTime = l.getTime
          val rTime = r.getTime
          if (lTime != rTime && math.abs(lTime - rTime) != hourInMs)
            (id, l) shouldBe ((id, r))
        case _ => (id, l) shouldBe ((id, r))
      }
    }
    roundTrip[Timestamp](
      List(Timestamp.valueOf("2012-12-24 17:53:48.0"),
        Timestamp.valueOf("2016-10-30 01:12:16.0")),
      dataCreateFn = () => Timestamp.from(randomLocalDateTime().toInstant(ZoneOffset.UTC)),
      dataCompareFn = timestampCompare
    )
  }

  // Test the java.time.* types
  def testLocalDateTime = {
    def localDateTimeCompare(id: Int, l: Option[LocalDateTime], r: Option[LocalDateTime]) = {
      (l, r) match {
        case (Some(l), Some(r)) =>
          if (l != r &&
            math.abs(ChronoUnit.MILLIS.between(l, r)) != hourInMs)
            (id, l) shouldBe ((id, r))
        case _ => (id, l) shouldBe ((id, r))
      }
    }

    roundTrip[LocalDateTime](
      List(LocalDateTime.parse("2018-03-25T01:37:40", formatter),
        generateTestLocalDateTime().withHour(5),
        generateTestLocalDateTime().withHour(12)),
      dataCreateFn = () => randomLocalDateTime(),
      dataCompareFn = localDateTimeCompare
    )
  }

  def testLocalDate =
    roundTrip[LocalDate](
      List(LocalDate.now(ZoneOffset.UTC)),
      () => randomLocalDateTime().toLocalDate
    )

  def testLocalTime =
    roundTrip[LocalTime](
      List(generateTestLocalDateTime().toLocalTime.withHour(14),
        generateTestLocalDateTime().toLocalTime.withHour(5)),
      () => randomLocalDateTime().toLocalTime
    )

  def testLocalTimeFormatCompatibility =
    roundTrip[LocalTime](
      List(
        LocalTime.of(12, 34),
        LocalTime.of(12, 34, 56),
        LocalTime.of(12, 34, 56, 123000000)
      ),
      () => LocalTime.of(12, 34),
      tableNameSuffix = "_format_compat"
    )

  def testInstant =
    roundTrip[Instant](
      List(LocalDateTime.parse("2018-03-25T01:37:40", formatter).toInstant(ZoneOffset.UTC),
        Instant.parse("2015-06-05T09:43:00Z"), // time has zero seconds and milliseconds
        generateTestLocalDateTime().withHour(15).toInstant(ZoneOffset.UTC),
        generateTestLocalDateTime().withHour(5).toInstant(ZoneOffset.UTC)),
      () => randomLocalDateTime().toInstant(ZoneOffset.UTC)
    )

  def testPostgresInstantWithTimeZone: IO[Unit] = tdb.profile match {
    case _: PostgresProfile =>
      // Slick uses the TIMESTAMP mapping by default for instants, however it should also
      // be possible to read/write Instants as TIMESTAMPTZ (with time zone)
      // This test ensures that the profile logic also works correctly for the TIMESTAMPTZ type
      // Shadows the profile's implicit `instantColumnType` so that `roundTrip` picks up the
      // TIMESTAMPTZ mapping (passing implicit arguments explicitly is not portable to Scala 3).
      implicit val instantColumnType: BaseColumnType[Instant] = new PostgresProfile.columnTypes.InstantJdbcType {
        override def sqlTypeName(sym: Option[FieldSymbol]) = "TIMESTAMPTZ"
      }
      roundTrip[Instant](
        List(LocalDateTime.parse("2018-03-25T01:37:40", formatter).toInstant(ZoneOffset.UTC),
          Instant.parse("2015-06-05T09:43:00Z"), // time has zero seconds and milliseconds
          generateTestLocalDateTime().withHour(15).toInstant(ZoneOffset.UTC),
          generateTestLocalDateTime().withHour(5).toInstant(ZoneOffset.UTC)),
        () => randomLocalDateTime().toInstant(ZoneOffset.UTC),
        tableNameSuffix = "_with_time_zone"
      )
    case _                  =>
      IO.unit
  }

  def testH2InstantWithOffset: IO[Unit] = tdb.profile match {
    case _: H2Profile =>
      // Values stored with a non-UTC offset (e.g. written by other clients or by CURRENT_TIMESTAMP in a non-UTC
      // session) must be read back as the correct Instant, including offsets with minutes (#2053, #2055)
      class InstantTable(tag: Tag) extends Table[(Int, Instant)](tag, "Data_Instant_with_offset") {
        def id = column[Int]("ID", O.PrimaryKey)
        def data = column[Instant]("DATA")
        def * = (id, data)
      }
      val instantTable = TableQuery[InstantTable]
      val expected = Seq(
        1 -> Instant.parse("2019-07-17T09:34:00Z"),
        2 -> Instant.parse("2019-07-17T07:04:00Z"),
        3 -> Instant.parse("2019-07-17T22:04:00.123456789Z"),
        4 -> Instant.parse("2019-07-17T12:34:00Z")
      )
      db.run(seq(
        instantTable.schema.create,
        sqlu"""insert into "Data_Instant_with_offset" values (1, TIMESTAMP WITH TIME ZONE '2019-07-17 12:34:00+03:00')""",
        sqlu"""insert into "Data_Instant_with_offset" values (2, TIMESTAMP WITH TIME ZONE '2019-07-17 12:34:00+05:30')""",
        sqlu"""insert into "Data_Instant_with_offset" values (3, TIMESTAMP WITH TIME ZONE '2019-07-17 12:34:00.123456789-09:30')""",
        sqlu"""insert into "Data_Instant_with_offset" values (4, TIMESTAMP WITH TIME ZONE '2019-07-17 12:34:00Z')""",
        instantTable.sortBy(_.id).result.map(_ shouldBe expected),
        instantTable.filter(_.data === expected(1)._2).map(_.id).result.map(_ shouldBe Seq(2)),
        instantTable.filter(_.data === expected(2)._2.bind).map(_.id).result.map(_ shouldBe Seq(3))
      ))
    case _ =>
      IO.unit
  }

  def testPostgresInstantWithInfiniteValues: IO[Unit] = tdb.profile match {
    case _: PostgresProfile =>
      roundTrip[Instant](
        List(
          Instant.MIN,
          Instant.MAX),
        () => randomLocalDateTime().toInstant(ZoneOffset.UTC),
        tableNameSuffix = "_with_infinite_values"
      )
    case _                  =>
      IO.unit
  }

  def testPostgresLocalDateTimeWithInfiniteValues: IO[Unit] = tdb.profile match {
    case _: PostgresProfile =>
      roundTrip[LocalDateTime](
        List(
          LocalDateTime.MIN,
          LocalDateTime.MAX),
        () => randomLocalDateTime(),
        tableNameSuffix = "_with_infinite_values"
      )
    case _                  =>
      IO.unit
  }

  def testPostgresLocalDateWithInfiniteValues: IO[Unit] = tdb.profile match {
    case _: PostgresProfile =>
      roundTrip[LocalDate](
        List(
          LocalDate.MIN,
          LocalDate.MAX),
        () => randomLocalDateTime().toLocalDate,
        tableNameSuffix = "_with_infinite_values"
      )
    case _                  =>
      IO.unit
  }

  def testPostgresLocalTimeWithInfiniteValues: IO[Unit] = tdb.profile match {
    case _: PostgresProfile =>
      roundTrip[LocalTime](
        List(
          LocalTime.MIN,
          LocalTime.MAX),
        () => randomLocalDateTime().toLocalTime,
        tableNameSuffix = "_with_infinite_values"
      )
    case _                  =>
      IO.unit
  }

  def testPostgresOffsetTimeWithInfiniteValues: IO[Unit] = tdb.profile match {
    case _: PostgresProfile =>
      roundTrip[OffsetTime](
        List(
          OffsetTime.MIN,
          OffsetTime.MAX),
        () => randomLocalDateTime().atOffset(ZoneOffset.UTC).toOffsetTime,

        dataCompareFn = (id, original, result) => original match {
        case Some(original) if original == OffsetTime.MAX => OffsetTime.MAX.withOffsetSameLocal(ZoneOffset.ofHoursMinutes(15, 59))
        case Some(original) if original == OffsetTime.MIN => OffsetTime.MIN.withOffsetSameLocal(ZoneOffset.ofHoursMinutes(-15, -59))
        case value => value == result
      },
      tableNameSuffix = "_with_infinite_values"
    )
  case _                  =>
      IO.unit
  }

  private def randomZoneOffset = {
    // offset could be +-18 in java.time context, but postgres and oracle are stricter
    val hours = random.nextInt(25) - 12
    val mins = math.signum(hours) * random.nextInt(2) * 30
    ZoneOffset.ofHoursMinutes(hours, mins)
  }

  def testOffsetTime =
    roundTrip[OffsetTime](
      List(OffsetTime.of(0, 0, 1, 746000000, ZoneOffset.ofHours(1)),
        OffsetTime.of(0, 0, 0, 745000000, ZoneOffset.ofHours(1)),
        generateTestLocalDateTime().atOffset(ZoneOffset.UTC).toOffsetTime.withHour(15),
        generateTestLocalDateTime().atOffset(ZoneOffset.UTC).toOffsetTime.withHour(5),
        generateTestLocalDateTime().atZone(ZoneId.of("Pacific/Samoa")).toOffsetDateTime.toOffsetTime.withHour(15),
        generateTestLocalDateTime().atZone(ZoneId.of("Antarctica/Rothera")).toOffsetDateTime.toOffsetTime,
        generateTestLocalDateTime().atZone(ZoneId.of("Pacific/Wallis")).toOffsetDateTime.toOffsetTime,
        generateTestLocalDateTime().atZone(ZoneId.of("Africa/Johannesburg")).toOffsetDateTime.toOffsetTime),
      () => randomLocalDateTime().atOffset(randomZoneOffset).toOffsetTime
    )

  def testOffsetDateTime =
    roundTrip[OffsetDateTime](
      List(
        OffsetDateTime.parse("2015-06-05T09:43:00+00:00"), // time has zero seconds and milliseconds
        generateTestLocalDateTime().atOffset(ZoneOffset.UTC).withHour(15),
        generateTestLocalDateTime().atOffset(ZoneOffset.UTC).withHour(5),
        generateTestLocalDateTime().atZone(ZoneId.of("Pacific/Samoa")).toOffsetDateTime.withHour(15),
        generateTestLocalDateTime().atZone(ZoneId.of("Africa/Addis_Ababa")).toOffsetDateTime.withHour(15),
        generateTestLocalDateTime().atZone(ZoneId.of("Pacific/Wallis")).toOffsetDateTime.withHour(15),
        generateTestLocalDateTime().atZone(ZoneId.of("Africa/Johannesburg")).toOffsetDateTime.withHour(15)),
      () => randomLocalDateTime().atOffset(randomZoneOffset)
    )

  // the database backends that support named timezones aren't necessarily configured for all
  // the zoneIds returned from ZoneId.getAvailableZoneIds (e.g. Oracle 11), so pick a subset to test with
  val zoneIds = List(
    "Europe/Zaporozhye",
    "America/Argentina/Salta",
    "Etc/GMT+7",
    "Antarctica/Davis",
    "Australia/ACT",
    "America/Dawson_Creek",
    "GB",
    "Pacific/Johnston",
    "Portugal",
    "Australia/Eucla"
  )
  def testZonedDateTime = {
    @tailrec
    def generateTestZonedDateTime(): ZonedDateTime = {
      val zoneId = ZoneId.of(zoneIds(random.nextInt(zoneIds.size)))
      val rules = zoneId.getRules
      val localDateTime = randomLocalDateTime()
      val trans = rules.getTransition(localDateTime)
      if (trans != null && trans.isGap) {
        // an invalid time has been generated (in DST gap),
        // there's a good chance it won't roundtrip cleanly, try again
        generateTestZonedDateTime()
      } else {
        localDateTime.atZone(zoneId)
      }
    }
    val baseLocalDateTime = randomLocalDateTime()
    roundTrip[ZonedDateTime](
      // from the random baseLocalDateTime, generate samples from about 6 months worth
      // of datetimes for each test zoneId
      (0 to 50).map(offset => baseLocalDateTime.plusMinutes(offset * 5500)).toList.
        flatMap(offsetLocalDateTime => zoneIds.map(zoneId => offsetLocalDateTime.atZone(ZoneId.of(zoneId)))),
      generateTestZonedDateTime
    )
  }

  def testNonFiniteFloatingPoint = {
    class T(tag: Tag) extends Table[(Int, Double, Float)](tag, "test_nonfinite") {
      def id = column[Int]("id")
      def d = column[Double]("d")
      def f = column[Float]("f")
      def * = (id, d, f)
    }
    val ts = TableQuery[T]

    // Derby, MySQL, SQL Server and DB2 cannot store NaN or the infinities; SQLite stores NaN as NULL.
    // Oracle's BINARY_DOUBLE can, but its driver's setDouble rejects them ("Overflow").
    val (supportsNaN, supportsInfinity) = tdb.profile match {
      case _: H2Profile | _: HsqldbProfile | _: PostgresProfile => (true, true)
      case _: SQLiteProfile                                     => (false, true)
      case _                                                    => (false, false)
    }

    val infinities = seq(
      ts += (2, Double.PositiveInfinity, Float.PositiveInfinity),
      ts += (3, Double.NegativeInfinity, Float.NegativeInfinity),
      ts.filter(_.d === Double.PositiveInfinity).map(_.id).result.map(_ shouldBe Seq(2)),
      ts.filter(_.d === Double.NegativeInfinity).map(_.id).result.map(_ shouldBe Seq(3)),
      ts.filter(_.f === Float.PositiveInfinity).map(_.id).result.map(_ shouldBe Seq(2)),
      ts.filter(_.f === Float.NegativeInfinity).map(_.id).result.map(_ shouldBe Seq(3)),
      ts.filter(_.id === 1).map(_ => Double.PositiveInfinity).result.head.map(_ shouldBe Double.PositiveInfinity)
    )

    val nan = seq(
      ts += (4, Double.NaN, Float.NaN),
      ts.filter(_.d === Double.NaN).map(_.id).result.map(_ shouldBe Seq(4)),
      ts.filter(_.f === Float.NaN).map(_.id).result.map(_ shouldBe Seq(4)),
      ts.filter(_.id === 1).map(_ => Double.NaN).result.head.map(_.isNaN shouldBe true)
    )

    // Column defaults have to be rendered as SQL literals (a string cast)
    class D(tag: Tag) extends Table[(Int, Double, Double)](tag, "test_nonfinite_default") {
      def id = column[Int]("id")
      def nan = column[Double]("nan", O.Default(Double.NaN))
      def inf = column[Double]("inf", O.Default(Double.NegativeInfinity))
      def * = (id, nan, inf)
    }
    val ds = TableQuery[D]
    val defaults = seq(
      ds.schema.create,
      ds.map(_.id) += 1,
      ds.map(d => (d.nan, d.inf)).result.head.map { case (n, i) =>
        n.isNaN shouldBe true
        i shouldBe Double.NegativeInfinity
      }
    )

    seq(
      ts.schema.create,
      ts += (1, 1.5, 1.5f),
      if (supportsInfinity) infinities else DBIO.successful(()),
      if (supportsNaN) nan else DBIO.successful(()),
      if (supportsNaN) defaults else DBIO.successful(()),
      ts.filter(_.d === 1.5).map(_.id).result.map(_ shouldBe Seq(1))
    )
  }

  def testMappedNonFiniteFloatingPoint = {
    case class WD(value: Double)
    case class WF(value: Float)
    implicit val wdType: BaseColumnType[WD] = MappedColumnType.base[WD, Double](_.value, WD(_))
    implicit val wfType: BaseColumnType[WF] = MappedColumnType.base[WF, Float](_.value, WF(_))

    class T(tag: Tag) extends Table[(Int, WD, WF, Option[WD])](tag, "test_mapped_nonfinite") {
      def id = column[Int]("id")
      def d = column[WD]("d")
      def f = column[WF]("f")
      def od = column[Option[WD]]("od")
      def * = (id, d, f, od)
    }
    val ts = TableQuery[T]

    // Same support matrix as in testNonFiniteFloatingPoint
    val (supportsNaN, supportsInfinity) = tdb.profile match {
      case _: H2Profile | _: HsqldbProfile | _: PostgresProfile => (true, true)
      case _: SQLiteProfile                                     => (false, true)
      case _                                                    => (false, false)
    }

    // Row 1 holds 0.0, which SQLite (and MySQL) make of an inlined cast('Infinity' as DOUBLE)
    val infinities = seq(
      ts += (2, WD(Double.PositiveInfinity), WF(Float.PositiveInfinity), Some(WD(Double.PositiveInfinity))),
      ts += (3, WD(Double.NegativeInfinity), WF(Float.NegativeInfinity), Some(WD(Double.NegativeInfinity))),
      ts.filter(_.d === WD(Double.PositiveInfinity)).map(_.id).result.map(_ shouldBe Seq(2)),
      ts.filter(_.d === WD(Double.NegativeInfinity)).map(_.id).result.map(_ shouldBe Seq(3)),
      ts.filter(_.f === WF(Float.PositiveInfinity)).map(_.id).result.map(_ shouldBe Seq(2)),
      ts.filter(_.f === WF(Float.NegativeInfinity)).map(_.id).result.map(_ shouldBe Seq(3)),
      ts.filter(_.od === Option(WD(Double.PositiveInfinity))).map(_.id).result.map(_ shouldBe Seq(2)),
      ts.filter(_.id === 1).map(_ => WD(Double.NegativeInfinity)).result.head
        .map(_.value shouldBe Double.NegativeInfinity)
    )

    val nan = seq(
      ts += (4, WD(Double.NaN), WF(Float.NaN), Some(WD(Double.NaN))),
      ts.filter(_.d === WD(Double.NaN)).map(_.id).result.map(_ shouldBe Seq(4)),
      ts.filter(_.f === WF(Float.NaN)).map(_.id).result.map(_ shouldBe Seq(4)),
      ts.filter(_.od === Option(WD(Double.NaN))).map(_.id).result.map(_ shouldBe Seq(4))
    )

    // A mapped column default is rendered through the base type's literal (a string cast)
    class D(tag: Tag) extends Table[(Int, WD)](tag, "test_mapped_nonfinite_default") {
      def id = column[Int]("id")
      def inf = column[WD]("inf", O.Default(WD(Double.NegativeInfinity)))
      def * = (id, inf)
    }
    val ds = TableQuery[D]
    val defaults = seq(
      ds.schema.create,
      ds.map(_.id) += 1,
      ds.map(_.inf).result.head.map(_.value shouldBe Double.NegativeInfinity)
    )

    seq(
      ts.schema.create,
      ts += (1, WD(0.0), WF(0.0f), Some(WD(0.0))),
      if (supportsInfinity) infinities else DBIO.successful(()),
      if (supportsNaN) nan else DBIO.successful(()),
      if (supportsNaN) defaults else DBIO.successful(()),
      ts.filter(_.d === WD(0.0)).map(_.id).result.map(_ shouldBe Seq(1))
    )
  }

  def testOverrideIdentityType = {
    class T1(tag: Tag) extends Table[Int](tag, "t1") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc, O.SqlType("_FOO_BAR_"))
      def * = id
    }
    val t1 = TableQuery[T1]
    t1.schema.createStatements.mkString should (_.contains("_FOO_BAR_"))
    IO.unit
  }
}
