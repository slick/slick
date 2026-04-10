package slick.test.ce

import java.util.concurrent.CancellationException

import cats.effect.{Async, Deferred, IO, Ref, Resource}
import cats.syntax.parallel.*
import munit.CatsEffectSuite
import slick.basic.ConcurrencyControl.Controls

import scala.concurrent.duration._
import slick.SlickException
import slick.jdbc.H2Profile.api.*
import slick.jdbc.{DataSourceJdbcDataSource, DriverDataSource, JdbcBackend}

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
  val singleSlotDb = ResourceFunFixture(
    JdbcBackend.Database.forSource[IO](h2Source("ce3test_single"), maxConnections = Some(1))
  )

  /** A database with several connection slots for concurrency tests. */
  val multiSlotDb = ResourceFunFixture(
    JdbcBackend.Database.forSource[IO](h2Source("ce3test_multi"), maxConnections = Some(4))
  )

  /** A database with strict admission controls for inflight/queue tests. */
  val admissionDb = ResourceFunFixture(
    // TODO(slick4-rebase): Replace this ad-hoc Controls.create + JdbcDatabaseDef construction
    // with the real public DatabaseConfig/JDBC API once that API is available again in the
    // reordered commit sequence.
    Resource.make(
      Controls.create[IO](1L, 1L, 1L).map { controls =>
        new JdbcBackend.JdbcDatabaseDef[IO](h2Source("ce3test_admission"), controls) {
          override implicit val asyncF: Async[IO] = implicitly[Async[IO]]
        }
      }
    )(db => IO.blocking(db.close()))
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
      db.availableAdmissionQueueSlots.flatMap { n =>
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
