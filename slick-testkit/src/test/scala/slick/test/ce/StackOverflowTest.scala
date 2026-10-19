package slick.test.ce

import cats.effect.{IO, Resource}
import cats.effect.unsafe.IORuntime
import cats.syntax.traverse.*
import munit.CatsEffectSuite

import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration.*
import scala.util.Failure

import slick.{ControlStatus, ControlsConfig, SlickException}
import slick.basic.BasicBackend
import slick.cats.Database
import slick.dbio.SynchronousDatabaseAction
import slick.jdbc.{DataSourceJdbcDataSource, DatabaseConfig, DriverDataSource, GetResult, H2Profile}
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

  /** The core database, for tests that read a stream's iterator directly; `db` wraps it. */
  private val coreResource: Resource[IO, BasicBackend#BasicDatabaseDef[IO]] = {
    val ds = new DriverDataSource("jdbc:h2:mem:stackoverflowtest;DB_CLOSE_DELAY=-1", driverClassName = "org.h2.Driver")
    val source = new DataSourceJdbcDataSource(ds, keepAliveConnection = false, maxConnections = None)
    val config = DatabaseConfig.forSource(H2Profile, source)
      .withControls(ControlsConfig(maxConnections = maxConnections, maxInflight = Some(maxInflight)))
    Resource.make(config.makeDatabase[IO]())(core => IO.blocking(core.close()).attempt.void)
  }

  private def createItems(db: Database): IO[Unit] =
    db.run(DBIO.seq(items.schema.dropIfExists, items.schema.create, items += 1))

  val db = ResourceFunFixture(coreResource.map(Database.fromCore).evalTap(createItems))

  val coreAndDb = ResourceFunFixture(coreResource.map(core => (core, Database.fromCore(core))).evalTap {
    case (_, db) => createItems(db)
  })

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

  /** Run `io` on a dedicated runtime with a timeout. An unguarded `StackOverflowError` shuts down
    * the cats-effect runtimes, so without the timeout a regression would hang the test. */
  private def runIsolated[A](io: IO[A]): IO[A] = IO.blocking {
    // Named like the suite's implicit runtime so that it shadows it.
    implicit val munitIORuntime: IORuntime = IORuntime.builder().build()
    try io.unsafeRunTimed(60.seconds).getOrElse(fail("Timed out: did a StackOverflowError stop the runtime?"))
    finally munitIORuntime.shutdown()
  }

  /** Reads the items, converting every row by recursing until the stack overflows. */
  private val overflowingRows = {
    implicit val overflowingRow: GetResult[Int] = GetResult(_ => overflow(0))
    sql"select v from items".as[Int]
  }

  /** Read the first row of a stream on a plain blocking thread, outside of any fiber, so that an
    * unguarded `StackOverflowError` fails only this check instead of stopping the runtimes, and
    * release the stream with the error. It must be a SlickException caused by the overflow. */
  private def expectOverflowReadingFirstRow(core: BasicBackend#BasicDatabaseDef[IO], action: DBIOAction[?, Streaming[Int], Nothing]): IO[Unit] =
    core.stream(action).allocatedCase.flatMap { case (it, release) =>
      IO.blocking {
        try { it.hasNext; it.next(); None }
        catch { case t: Throwable => Some(t) }
      }.flatMap {
        case Some(e: SlickException) if e.getCause.isInstanceOf[StackOverflowError] =>
          release(Resource.ExitCase.Errored(e))
        case other =>
          release(Resource.ExitCase.Succeeded) >>
            IO(fail(s"Expected a SlickException caused by StackOverflowError, got $other"))
      }
    }

  /** Check the stream on the core iterator first: only if that is guarded is it safe to run it
    * through the cats facade, where an unguarded overflow would stop the runtimes. */
  private def expectStreamOverflows(core: BasicBackend#BasicDatabaseDef[IO], db: Database, action: DBIOAction[?, Streaming[Int], Nothing]): IO[Unit] =
    expectOverflowReadingFirstRow(core, action) >>
      checkRecovered(db) >>
      runIsolated(expectOverflows(db.stream(action).compile.drain)) >>
      checkRecovered(db)

  coreAndDb.test("StackOverflowError while reading the rows of a stream fails the stream and releases its slots") {
    case (core, db) =>
      expectStreamOverflows(core, db, overflowingRows)
  }

  coreAndDb.test("StackOverflowError while reading the rows of a stream rolls its transaction back") {
    case (core, db) =>
      expectStreamOverflows(core, db, (items += 2).andThen(overflowingRows).transactionally)
  }

  private val overflowing: DBIO[Int] = SimpleDBIO(_ => overflow(0))
  /** The same action as `overflowing`, but behind a `flatMap`, so combinators on it are not fused. */
  private val overflowingNonFused: DBIO[Int] = DBIO.successful(()).flatMap(_ => overflowing)

  /** The outcome of an action, with the classes of a throwable and its cause instead of their messages. */
  private def describe(outcome: Either[Throwable, Any]): String = {
    def throwable(t: Throwable) = s"${t.getClass.getName}(cause: ${Option(t.getCause).map(_.getClass.getName)})"
    outcome match {
      case Left(t)             => s"Left(${throwable(t)})"
      case Right(Failure(t))   => s"Right(Failure(${throwable(t)}))"
      case Right(t: Throwable) => s"Right(${throwable(t)})"
      case Right(v)            => s"Right($v)"
    }
  }

  /** Run the fused and the non-fused form of a combinator on an overflowing action. Both must give
    * the same outcome and call their cleanup the same number of times. */
  private def checkFusedLikeNonFused(db: Database)(combinator: (DBIO[Int], AtomicInteger) => DBIO[Any]): IO[String] = {
    val fusedCleanups = new AtomicInteger
    val nonFusedCleanups = new AtomicInteger
    val fused = combinator(overflowing, fusedCleanups)
    val nonFused = combinator(overflowingNonFused, nonFusedCleanups)
    assert(fused.isInstanceOf[SynchronousDatabaseAction.Fused[?, ?, ?, ?]], fused)
    assert(!nonFused.isInstanceOf[SynchronousDatabaseAction[?, ?, ?, ?]], nonFused)
    for {
      fusedOutcome    <- db.run(fused).timeout(10.seconds).attempt.map(describe)
      nonFusedOutcome <- db.run(nonFused).timeout(10.seconds).attempt.map(describe)
      _               <- checkRecovered(db)
    } yield {
      assertEquals(fusedOutcome, nonFusedOutcome)
      assertEquals(fusedCleanups.get, nonFusedCleanups.get)
      fusedOutcome
    }
  }

  private val slickExceptionFromOverflow =
    s"${classOf[SlickException].getName}(cause: Some(${classOf[StackOverflowError].getName}))"

  db.test("Fused asTry returns a StackOverflowError as a Failure like its non-fused form") { db =>
    checkFusedLikeNonFused(db)((a, _) => a.asTry)
      .map(assertEquals(_, s"Right(Failure($slickExceptionFromOverflow))"))
  }

  db.test("Fused failed returns a StackOverflowError like its non-fused form") { db =>
    checkFusedLikeNonFused(db)((a, _) => a.failed)
      .map(assertEquals(_, s"Right($slickExceptionFromOverflow)"))
  }

  db.test("Fused andFinally runs its cleanup after a StackOverflowError like its non-fused form") { db =>
    checkFusedLikeNonFused(db)((a, cleanups) => a.andFinally(SimpleDBIO(_ => cleanups.incrementAndGet())))
      .map(assertEquals(_, s"Left($slickExceptionFromOverflow)"))
  }

  db.test("Fused cleanUp runs its cleanup after a StackOverflowError like its non-fused form") { db =>
    def withCleanUp(a: DBIO[Int], cleanups: AtomicInteger): DBIO[Any] =
      SynchronousDatabaseAction.fuseUnsafe(a.cleanUp { error =>
        SimpleDBIO { _ =>
          assert(error.exists(_.getCause.isInstanceOf[StackOverflowError]), error)
          cleanups.incrementAndGet()
        }
      })
    checkFusedLikeNonFused(db)(withCleanUp)
      .map(assertEquals(_, s"Left($slickExceptionFromOverflow)"))
  }

  db.test("Fused cleanUp handles a StackOverflowError in its cleanup like its non-fused form") { db =>
    def withCleanUp(a: DBIO[Int], cleanups: AtomicInteger): DBIO[Any] =
      SynchronousDatabaseAction.fuseUnsafe(a.cleanUp(_ => SimpleDBIO(_ => cleanups.incrementAndGet()).andThen(overflowing), keepFailure = true))
    checkFusedLikeNonFused(db)(withCleanUp)
      .map(assertEquals(_, s"Left($slickExceptionFromOverflow)"))
  }
}
