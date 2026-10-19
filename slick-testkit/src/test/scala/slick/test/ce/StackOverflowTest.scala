package slick.test.ce

import cats.effect.{IO, Resource}
import cats.syntax.traverse.*
import munit.CatsEffectSuite

import scala.concurrent.duration.*

import slick.{ControlStatus, ControlsConfig, SlickException}
import slick.cats.Database
import slick.jdbc.{DataSourceJdbcDataSource, DatabaseConfig, DriverDataSource, H2Profile}
import slick.jdbc.H2Profile.api.*

/** A `StackOverflowError` thrown inside a running action must fail that action and release its
  * in-flight slot, connection and transaction, like any other error (issue #1972). */
class StackOverflowTest extends CatsEffectSuite {

  override def munitIOTimeout: Duration = 60.seconds

  class Items(tag: Tag) extends Table[Int](tag, "ITEMS") {
    def v = column[Int]("V")
    def * = v
  }
  val items = TableQuery[Items]

  private val maxConnections = 2
  private val maxInflight = 4
  /** More failing runs than in-flight slots, so a leaked slot per failure would block the database. */
  private val repeats = maxInflight + 2

  private val dbResource: Resource[IO, Database] = {
    val ds = new DriverDataSource("jdbc:h2:mem:stackoverflowtest;DB_CLOSE_DELAY=-1", driverClassName = "org.h2.Driver")
    val source = new DataSourceJdbcDataSource(ds, keepAliveConnection = false, maxConnections = None)
    Database.resource(DatabaseConfig.forSource(H2Profile, source)
      .withControls(ControlsConfig(maxConnections = maxConnections, maxInflight = Some(maxInflight))))
  }

  val db = ResourceFunFixture(dbResource.evalTap(db =>
    db.run(DBIO.seq(items.schema.dropIfExists, items.schema.create, items += 1))))

  /** Recurses until the stack overflows. */
  private def overflow(n: Int): Int = if (n < 0) n else overflow(n + 1) + 1

  private val initialStatus = ControlStatus(
    availableConnectionSlots = maxConnections.toLong,
    pendingConnectionSlots = 0,
    availableAdmissionQueueSlots = ControlsConfig().queueSize.toLong,
    availableInflightSlots = maxInflight.toLong
  )

  /** Run `io` `repeats` times; each run must fail with a SlickException caused by a StackOverflowError. */
  private def expectOverflows(io: IO[Any]): IO[Unit] =
    (1 to repeats).toList.traverse { _ =>
      io.timeout(10.seconds).attempt.map {
        case Left(e: SlickException) => assert(e.getCause.isInstanceOf[StackOverflowError], e)
        case other                   => fail(s"Expected a SlickException caused by StackOverflowError, got $other")
      }
    }.void

  private def checkRecovered(db: Database): IO[Unit] =
    for {
      status <- db.controlStatus
      _      =  assertEquals(status, initialStatus)
      rows   <- db.run(items.result).timeout(10.seconds)
    } yield assertEquals(rows, Seq(1))

  db.test("StackOverflowError in a flatMap continuation fails the action and releases its slots") { db =>
    expectOverflows(db.run(DBIO.successful(()).flatMap(_ => items.filter(_.v === overflow(0)).result))) >>
      checkRecovered(db)
  }

  db.test("StackOverflowError in a map function inside a session fails the action") { db =>
    expectOverflows(db.run(items.result.withPinnedSession.flatMap(rs => items.result.map(_ => overflow(rs.size))))) >>
      checkRecovered(db)
  }

  db.test("StackOverflowError in a synchronous action fails the action and releases its connection") { db =>
    expectOverflows(db.run(SimpleDBIO(_ => overflow(0)))) >>
      checkRecovered(db)
  }

  db.test("StackOverflowError in a transaction rolls the transaction back") { db =>
    val tx = (items += 2).flatMap(_ => DBIO.successful(overflow(0))).transactionally
    expectOverflows(db.run(tx)) >>
      checkRecovered(db)
  }

  db.test("StackOverflowError while opening a stream fails the stream and releases its slots") { db =>
    val action = DBIO.successful(()).flatMap(_ => items.filter(_.v === overflow(0)).result)
    expectOverflows(db.stream(action).compile.drain) >>
      checkRecovered(db)
  }

  db.test("StackOverflowError in a streaming synchronous action fails the stream") { db =>
    val action = sql"select v from items".as[Int].map(_ => overflow(0)).flatMap(_ => items.result)
    expectOverflows(db.stream(action.transactionally).compile.drain) >>
      checkRecovered(db)
  }
}
