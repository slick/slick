package slick.test.ce

import java.util.concurrent.CancellationException

import cats.effect.{Deferred, IO, Ref, Resource}
import _root_.cats.syntax.parallel.*
import munit.CatsEffectSuite

import scala.concurrent.duration._
import slick.SlickException
import com.typesafe.config.ConfigFactory
import slick.cats
import slick.jdbc.DatabaseConfig
import slick.jdbc.H2Profile
import slick.jdbc.H2Profile.api.*
import slick.jdbc.{DataSourceJdbcDataSource, DriverDataSource}

/** Tests for CE3-specific guarantees in the Slick 4 interpreter.
  *
  * Covers:
  *   - Fiber cancellation during a transaction triggers rollback
  *   - Connection-slot backpressure (Semaphore correctly limits concurrency)
  *   - Streaming with back-pressure (FS2 pull model)
  *   - `DBIO.from` / `DBIO.liftF` lifting CE3 effects into DBIO
  */
class CE3GuaranteesTest extends CatsEffectSuite {

  class Items(tag: Tag) extends Table[Int](tag, "ITEMS") {
    def v = column[Int]("V")
    def * = v
  }
  val items = TableQuery[Items]

  private def h2Source(name: String) = {
    val ds = new DriverDataSource(s"jdbc:h2:mem:$name;DB_CLOSE_DELAY=-1", driverClassName = "org.h2.Driver")
    new DataSourceJdbcDataSource(ds, keepAliveConnection = false, maxConnections = None)
  }

  /** A database with a single connection slot — easy to saturate for backpressure tests. */
  private val singleSlotResource: Resource[IO, cats.Database] =
    cats.Database.resource(DatabaseConfig.forSource(H2Profile, h2Source("ce3test_single"), maxConnections = Some(1)))
  val singleSlotDb = ResourceFunFixture(singleSlotResource)

  /** A database with several connection slots for concurrency tests. */
  private val multiSlotResource: Resource[IO, cats.Database] =
    cats.Database.resource(DatabaseConfig.forSource(H2Profile, h2Source("ce3test_multi"), maxConnections = Some(4)))
  val multiSlotDb = ResourceFunFixture(multiSlotResource)

  /** A database with strict admission controls for inflight/queue tests. */
  val admissionDb = ResourceFunFixture(
    cats.Database.resource(DatabaseConfig.forProfileConfig(H2Profile, "db", ConfigFactory.parseString(
      """
        |db {
        |  dataSourceClass = "org.h2.jdbcx.JdbcDataSource"
        |  properties {
        |    URL = "jdbc:h2:mem:ce3test_admission;DB_CLOSE_DELAY=-1"
        |  }
        |  maxConnections = 1
        |  maxInflightActions = 1
        |  queueSize = 1
        |}
        |""".stripMargin
    )))
  )

  /** A database with short inflight admission timeout for timeout behavior tests. */
  val inflightTimeoutDb = ResourceFunFixture(
    cats.Database.resource(DatabaseConfig.forProfileConfig(H2Profile, "db", ConfigFactory.parseString(
      """
        |db {
        |  dataSourceClass = "org.h2.jdbcx.JdbcDataSource"
        |  properties {
        |    URL = "jdbc:h2:mem:ce3test_inflight_timeout;DB_CLOSE_DELAY=-1"
        |  }
        |  maxConnections = 1
        |  maxInflightActions = 1
        |  queueSize = 1
        |  inflightAdmissionTimeout = 200ms
        |}
        |""".stripMargin
    )))
  )

  /** A database with short connection acquire timeout for timeout behavior tests. */
  val connectionTimeoutDb = ResourceFunFixture(
    cats.Database.resource(DatabaseConfig.forProfileConfig(H2Profile, "db", ConfigFactory.parseString(
      """
        |db {
        |  dataSourceClass = "org.h2.jdbcx.JdbcDataSource"
        |  properties {
        |    URL = "jdbc:h2:mem:ce3test_connection_timeout;DB_CLOSE_DELAY=-1"
        |  }
        |  maxConnections = 1
        |  maxInflightActions = 2
        |  queueSize = 1
        |  connectionAcquireTimeout = 200ms
        |}
        |""".stripMargin
    )))
  )

  // ---------------------------------------------------------------------------
  // Fiber cancellation → rollback
  // ---------------------------------------------------------------------------

  singleSlotDb.test("fiber cancellation during transaction triggers rollback") { db =>
    for {
      _     <- db.run(items.schema.create)
      // A gate that lets us cancel the fiber at a known point inside the transaction
      gate  <- Deferred[IO, Unit]
      fiber <- db.run(
                 (for {
                   _ <- items += 42
                   // Lift an IO that signals readiness then blocks until cancelled
                   _ <- DBIO.from(gate.complete(()) >> IO.never[Unit])
                 } yield ()).transactionally
               ).start
      _     <- gate.get        // wait until the insert has run and IO.never is reached
      _     <- fiber.cancel    // cancel: should trigger rollback in the guaranteeCase finalizer
      _     <- fiber.join      // wait for finalizers to complete
      result <- db.run(items.result)
      _      = assertEquals(result, Vector.empty[Int], "rolled-back insert must not be visible")
      _     <- db.run(items.schema.drop)
    } yield ()
  }

  // ---------------------------------------------------------------------------
  // Connection-slot backpressure
  // ---------------------------------------------------------------------------

  singleSlotDb.test("semaphore serialises concurrent db.run calls when maxConnections = 1") { db =>
    // With maxConnections = 1 the second parTupled call must wait for the first to
    // release its connection.  Both should complete successfully with the same data.
    for {
      _ <- db.run(items.schema.create)
      _ <- db.run(items += 1)
      _ <- db.run(items += 2)
      result <- (db.run(items.result), db.run(items.result)).parTupled
      _  = assertEquals(result._1.toSet, Set(1, 2))
      _  = assertEquals(result._2.toSet, Set(1, 2))
      _ <- db.run(items.schema.drop)
    } yield ()
  }

  multiSlotDb.test("excess concurrent db.run calls are queued, not dropped") { db =>
    // With maxConnections = 4, launch 8 concurrent queries.  The 4 excess fibers
    // suspend on the semaphore and are served once a slot frees up.
    for {
      _ <- db.run(items.schema.create)
      _ <- db.run(DBIO.sequence((1 to 8).map(items += _)))
      results <- (1 to 8).toList.map(_ => db.run(items.result)).parSequence
      _  = assert(results.forall(_.size == 8), "every concurrent query must return all 8 rows")
      _ <- db.run(items.schema.drop)
    } yield ()
  }

  admissionDb.test("maxInflightActions gates execution even for non-DB steps") { db =>
    for {
      gate1 <- Deferred[IO, Unit]
      release1 <- Deferred[IO, Unit]
      gate2 <- Deferred[IO, Unit]

      fiber1 <- db.run(DBIO.from(gate1.complete(()) >> release1.get)).start
      _ <- gate1.get

      fiber2 <- db.run(DBIO.from(gate2.complete(()) >> IO.unit)).start

      // The second action should not start while the first occupies the single inflight slot.
      notStarted <- gate2.get.timeout(250.millis).attempt
      _ = assert(notStarted.isLeft, "second action should still be waiting for inflight admission")

      _ <- release1.complete(())
      _ <- fiber1.join
      _ <- fiber2.join
      _ <- gate2.get
    } yield ()
  }

  admissionDb.test("queueSize bounds callers waiting for inflight admission") { db =>
    def waitUntilQueueFull(deadline: FiniteDuration): IO[Unit] =
      db.controlStatus.flatMap { s =>
        val n = s.availableAdmissionQueueSlots
        if (n == 0L) IO.unit
        else if (deadline <= Duration.Zero) IO.raiseError(new RuntimeException("timed out waiting for queue to fill"))
        else IO.sleep(20.millis) >> waitUntilQueueFull(deadline - 20.millis)
      }

    for {
      gate1 <- Deferred[IO, Unit]
      release1 <- Deferred[IO, Unit]

      fiber1 <- db.run(DBIO.from(gate1.complete(()) >> release1.get)).start
      _ <- gate1.get

      // This one should occupy the only queue slot while waiting for inflight.
      fiber2 <- db.run(DBIO.successful(1)).start
      _ <- waitUntilQueueFull(2.seconds)

      // Third caller should be rejected immediately.
      rejected <- db.run(DBIO.successful(2)).attempt
      _ = assert(rejected.swap.exists(_.isInstanceOf[SlickException]))
      _ = assert(rejected.swap.exists(_.getMessage == "DBIOAction queue full"))

      _ <- release1.complete(())
      _ <- fiber1.join
      _ <- fiber2.join
    } yield ()
  }

  inflightTimeoutDb.test("inflightAdmissionTimeout fails callers waiting for inflight slot") { db =>
    for {
      gate1 <- Deferred[IO, Unit]
      release1 <- Deferred[IO, Unit]

      fiber1 <- db.run(DBIO.from(gate1.complete(()) >> release1.get)).start
      _ <- gate1.get

      timedOut <- db.run(DBIO.successful(2)).attempt
      _ = assert(timedOut.swap.exists(_.isInstanceOf[SlickException]))
      _ = assert(timedOut.swap.exists(_.getMessage.contains("Timed out waiting for inflight admission")))

      _ <- release1.complete(())
      _ <- fiber1.join
    } yield ()
  }

  connectionTimeoutDb.test("connectionAcquireTimeout fails callers waiting for connection slot") { db =>
    for {
      _ <- db.run(items.schema.create)
      gate1 <- Deferred[IO, Unit]
      release1 <- Deferred[IO, Unit]

      // Hold the only connection slot inside a pinned session.
      fiber1 <- db.run((for {
        _ <- items += 1
        _ <- DBIO.from(gate1.complete(()) >> release1.get)
      } yield ()).withPinnedSession).start
      _ <- gate1.get

      timedOut <- db.run(items.result).attempt
      _ = assert(timedOut.swap.exists(_.isInstanceOf[SlickException]))
      _ = assert(timedOut.swap.exists(_.getMessage.contains("Timed out waiting for connection slot")))

      _ <- release1.complete(())
      _ <- fiber1.join
      _ <- db.run(items.schema.drop)
    } yield ()
  }

  // ---------------------------------------------------------------------------
  // Streaming back-pressure
  // ---------------------------------------------------------------------------

  singleSlotDb.test("take(n) on a stream fetches only n rows (pull model)") { db =>
    for {
      _ <- db.run(items.schema.create)
      _ <- db.run(DBIO.sequence((1 to 10).map(items += _)))
      result <- db.stream(items.result).take(3).compile.toVector
      _  = assertEquals(result.size, 3)
      _ <- db.run(items.schema.drop)
    } yield ()
  }

  singleSlotDb.test("stream releases connection after full consumption") { db =>
    // If the connection is not released after the stream finishes, the next db.run
    // would deadlock forever on the single-slot semaphore.
    for {
      _ <- db.run(items.schema.create)
      _ <- db.run(DBIO.sequence((1 to 5).map(items += _)))
      _ <- db.stream(items.result).compile.drain
      result <- db.run(items.result)  // would deadlock if connection was leaked
      _  = assertEquals(result.size, 5)
      _ <- db.run(items.schema.drop)
    } yield ()
  }

  singleSlotDb.test("stream releases connection after early cancellation via take") { db =>
    for {
      _ <- db.run(items.schema.create)
      _ <- db.run(DBIO.sequence((1 to 5).map(items += _)))
      _ <- db.stream(items.result).take(2).compile.drain  // early termination
      result <- db.run(items.result)  // would deadlock if connection was leaked
      _  = assertEquals(result.size, 5)
      _ <- db.run(items.schema.drop)
    } yield ()
  }

  // ---------------------------------------------------------------------------
  // DBIO.from / DBIO.liftF lifting CE3 effects into DBIO
  // ---------------------------------------------------------------------------

  singleSlotDb.test("DBIO.from lifts an IO value into a DBIO action") { db =>
    for {
      _ <- db.run(items.schema.create)
      result <- db.run(for {
                  v <- DBIO.from(IO(42))
                  _ <- items += v
                  r <- items.result
                } yield r)
      _  = assertEquals(result, Vector(42))
      _ <- db.run(items.schema.drop)
    } yield ()
  }

  singleSlotDb.test("DBIO.liftF is an alias for DBIO.from") { db =>
    for {
      _ <- db.run(items.schema.create)
      result <- db.run(for {
                  v <- DBIO.liftF(IO(99))
                  _ <- items += v
                  r <- items.result
                } yield r)
      _  = assertEquals(result, Vector(99))
      _ <- db.run(items.schema.drop)
    } yield ()
  }

  singleSlotDb.test("DBIO.from failure propagates as DBIO failure") { db =>
    val boom = new RuntimeException("boom")
    db.run(DBIO.from(IO.raiseError[Int](boom))).attempt.map { result =>
      assert(result.isLeft)
      assertEquals(result.left.toOption.get.getMessage, "boom")
    }
  }

  singleSlotDb.test("DBIO.from IO error inside transactionally triggers rollback") { db =>
    for {
      _ <- db.run(items.schema.create)
      _ <- db.run(
             (items += 7).andThen(DBIO.from(IO.raiseError[Unit](new RuntimeException("fail"))))
               .transactionally
           ).attempt
      result <- db.run(items.result)
      _  = assertEquals(result, Vector.empty[Int], "IO error inside transactionally must roll back")
      _ <- db.run(items.schema.drop)
    } yield ()
  }

  // ---------------------------------------------------------------------------
  // Cancellation reification — cleanUp / asTry / failed
  // ---------------------------------------------------------------------------

  singleSlotDb.test("cleanUp runs on fiber cancellation and receives CancellationException") { db =>
    for {
      cleanupRan <- Ref[IO].of(false)
      gate       <- Deferred[IO, Unit]
      // Action: signal gate then block forever; cleanUp records whether it ran and what it received
      action = DBIO.from(gate.complete(()) >> IO.never[Unit])
        .cleanUp {
          case Some(_: CancellationException) => DBIO.from(cleanupRan.set(true))
          case _                              => DBIO.successful(())
        }
      fiber <- db.run(action).start
      _     <- gate.get
      _     <- fiber.cancel
      _     <- fiber.join
      ran   <- cleanupRan.get
      _      = assert(ran, "cleanUp must run on cancellation and receive CancellationException")
    } yield ()
  }

  singleSlotDb.test("andFinally runs on fiber cancellation") { db =>
    for {
      finallyRan <- Ref[IO].of(false)
      gate       <- Deferred[IO, Unit]
      action = DBIO.from(gate.complete(()) >> IO.never[Unit])
        .andFinally(DBIO.from(finallyRan.set(true)))
      fiber <- db.run(action).start
      _     <- gate.get
      _     <- fiber.cancel
      _     <- fiber.join
      ran   <- finallyRan.get
      _      = assert(ran, "andFinally must run on cancellation")
    } yield ()
  }

  singleSlotDb.test("asTry propagates cancellation (fiber stays canceled)") { db =>
    for {
      gate   <- Deferred[IO, Unit]
      action = DBIO.from(gate.complete(()) >> IO.never[Unit]).asTry
      fiber  <- db.run(action).start
      _      <- gate.get
      _      <- fiber.cancel
      oc     <- fiber.join
      _       = assert(oc.isCanceled, s"fiber outcome must be Canceled, got $oc")
    } yield ()
  }

  singleSlotDb.test("failed propagates cancellation (fiber stays canceled)") { db =>
    for {
      gate   <- Deferred[IO, Unit]
      action = DBIO.from(gate.complete(()) >> IO.never[Unit]).failed
      fiber  <- db.run(action).start
      _      <- gate.get
      _      <- fiber.cancel
      oc     <- fiber.join
      _       = assert(oc.isCanceled, s"fiber outcome must be Canceled, got $oc")
    } yield ()
  }

  singleSlotDb.test("cancellation is preserved after cleanUp (fiber remains canceled)") { db =>
    for {
      gate  <- Deferred[IO, Unit]
      action = DBIO.from(gate.complete(()) >> IO.never[Unit])
        .cleanUp(_ => DBIO.successful(()))
      fiber <- db.run(action).start
      _     <- gate.get
      _     <- fiber.cancel
      oc    <- fiber.join
      _      = assert(oc.isCanceled, "outcome must be Canceled after cleanUp on cancellation")
    } yield ()
  }
}
