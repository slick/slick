package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{AsyncTest, JdbcTestDB}
import java.io.{ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import java.sql.{Blob, Date, Time, Timestamp}
import java.util.UUID
import java.time._
import java.time.format.DateTimeFormatter
import java.time.temporal.{ChronoField, ChronoUnit}
import javax.sql.rowset.serial.SerialBlob

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.util.Random

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
                                           (id: Int, l: Option[T], r: Option[T]) => (id, l) shouldBe(id, r)) = {
    // How many random values to generate and test with
    val testValuesSize = 1000
    val rows = (1 to testValuesSize).map(i => (i, Some(dataCreateFn())))
    val updateValue = dataCreateFn()
    val insertValue = dataCreateFn()

    val tableName = "Data_" + values.headOption.getOrElse(dataCreateFn()).getClass.getSimpleName
    class DataTable(tag: Tag) extends Table[(Int, Option[T])](tag, tableName) {
      def id = column[Int]("ID", O.PrimaryKey)
      def data = column[Option[T]]("DATA")
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
    * If the SQL server driver `jtds` is used, there would be a 3 millisecond rounding, so
    * this method will generate a [[LocalDateTime]], using [[LocalDateTime#now]] whose milliseconds
    * ends either 0. It will just return [[LocalDateTime#now]] if any other driver or database
    * is being used.
    *
    * Older version of MySQL have no millisecond resolution, so set all the ms component
    * of the LocalDateTime to 000.
    *
    * For more information about the MsSQL issue: https://sourceforge.net/p/jtds/feature-requests/73/
    */
  private[this] def generateTestLocalDateTime() : LocalDateTime = {
    if (tdb.confName.contains("jtds")) {
      val now = Instant.now
      val offset = now.get(ChronoField.MILLI_OF_SECOND) % 10
      LocalDateTime.ofInstant(now.plusMillis(-offset), ZoneOffset.UTC)
    } else if (tdb.confName.contains("mysql")) {
      val now = Instant.now
      val msOffset = now.get(ChronoField.MILLI_OF_SECOND)
      LocalDateTime.ofInstant(now.plusMillis(-msOffset), ZoneOffset.UTC)
    } else
      LocalDateTime.now(ZoneOffset.UTC)
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
            (id, l) shouldBe (id, r)
        case _ => (id, l) shouldBe (id, r)
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
            (id, l) shouldBe (id, r)
        case _ => (id, l) shouldBe (id, r)
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

  def testInstant = 
    roundTrip[Instant](
      List(LocalDateTime.parse("2018-03-25T01:37:40", formatter).toInstant(ZoneOffset.UTC),
        generateTestLocalDateTime().withHour(15).toInstant(ZoneOffset.UTC),
        generateTestLocalDateTime().withHour(5).toInstant(ZoneOffset.UTC)),
      () => randomLocalDateTime().toInstant(ZoneOffset.UTC)
    )

  private def randomZoneOffset = {
    // offset could be +-18 in java.time context, but postgres is stricter
    val hours = random.nextInt(26)-13
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
    "America/Argentina/Cordoba",
    "America/Argentina/Salta",
    "Etc/GMT+7",
    "Europe/Kaliningrad",
    "Antarctica/Davis",
    "Mexico/BajaSur",
    "Australia/ACT",
    "America/Dawson_Creek",
    "GB",
    "America/Porto_Acre",
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
    roundTrip[ZonedDateTime](
      List(
        generateTestLocalDateTime().atOffset(ZoneOffset.UTC).toZonedDateTime.withHour(14),
        generateTestLocalDateTime().atOffset(ZoneOffset.UTC).toZonedDateTime.withHour(5),
        generateTestLocalDateTime().atZone(ZoneId.of("Pacific/Samoa")).withHour(5),
        generateTestLocalDateTime().atZone(ZoneId.of("Pacific/Wallis")).withHour(5),
        generateTestLocalDateTime().atZone(ZoneId.of("Pacific/Wallis")).withHour(5),
        generateTestLocalDateTime().atZone(ZoneId.of("Africa/Johannesburg")).withHour(5)),
      generateTestZonedDateTime
    )
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
