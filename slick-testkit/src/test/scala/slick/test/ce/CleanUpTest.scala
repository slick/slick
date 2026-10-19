package slick.test.ce

import java.sql.SQLException
import java.util.concurrent.{CancellationException, CopyOnWriteArrayList}

import scala.jdk.CollectionConverters.*

import cats.effect.{Deferred, IO, Outcome}
import munit.CatsEffectSuite

import slick.cats.Database
import slick.jdbc.{DataSourceJdbcDataSource, DatabaseConfig, DriverDataSource, H2Profile}
import slick.jdbc.H2Profile.api.*

/** `cleanUp(f)` calls `f` exactly once, with the outcome of the action it guards. */
class CleanUpTest extends CatsEffectSuite {

  class Items(tag: Tag) extends Table[Int](tag, "CLEANUP_ITEMS") {
    def v = column[Int]("V")
    def * = v
  }
  val items = TableQuery[Items]

  // A database name used only by this suite (its tests run one after another). DB_CLOSE_DELAY=-1 keeps the
  // in-memory database alive between connections, as keepAliveConnection is false.
  private val h2Db = ResourceFunFixture(
    Database.resource(DatabaseConfig.forSource(H2Profile, new DataSourceJdbcDataSource(
      new DriverDataSource("jdbc:h2:mem:cleanup_test;DB_CLOSE_DELAY=-1", driverClassName = "org.h2.Driver"),
      keepAliveConnection = false, maxConnections = None)))
      .evalTap(db => db.run(items.schema.dropIfExists >> items.schema.create >> (items ++= Seq(1, 2, 3))))
  )

  private val heapDb = ResourceFunFixture(
    Database.resource(slick.basic.DatabaseConfig.forProfileConfig(
      slick.memory.MemoryProfile, "heap", com.typesafe.config.ConfigFactory.parseString("heap {}")))
  )

  /** A cleanup function that records every argument it is called with. */
  private class Recorder {
    private val calls = new CopyOnWriteArrayList[Option[Throwable]]()
    val f: Option[Throwable] => DBIO[Unit] = { e =>
      calls.add(e)
      DBIO.successful(())
    }
    def get: IO[Vector[Option[Throwable]]] = IO(calls.asScala.toVector)
  }
  private def recorder: IO[Recorder] = IO(new Recorder)

  private val boom = new RuntimeException("boom")

  private def assertCancelledOnce(calls: Vector[Option[Throwable]]): Unit = {
    assertEquals(calls.size, 1, calls.toString)
    assert(calls.head.exists(_.isInstanceOf[CancellationException]), calls.toString)
  }

  /** Run an action that never completes and cancel it once it is running inside the cleanUp. */
  private def cancelledRun(db: Database, rec: Recorder): IO[Outcome[IO, Throwable, Int]] =
    for {
      started <- Deferred[IO, Unit]
      fiber   <- db.run(DBIO.successful(()).flatMap(_ => DBIO.from(started.complete(()) *> IO.never[Int])).cleanUp(rec.f)).start
      _       <- started.get
      _       <- fiber.cancel
      outcome <- fiber.join
    } yield outcome

  h2Db.test("cleanUp calls f once with None after a successful non-fused action") { db =>
    for {
      rec   <- recorder
      r     <- db.run(DBIO.successful(()).flatMap(_ => items.length.result).cleanUp(rec.f))
      calls <- rec.get
    } yield {
      assertEquals(r, 3)
      assertEquals(calls, Vector(None))
    }
  }

  h2Db.test("cleanUp calls f once with None after a successful synchronous action") { db =>
    for {
      rec   <- recorder
      r     <- db.run(items.length.result.cleanUp(rec.f))
      calls <- rec.get
    } yield {
      assertEquals(r, 3)
      assertEquals(calls, Vector(None))
    }
  }

  h2Db.test("cleanUp calls f once with the error after a failed action") { db =>
    for {
      rec   <- recorder
      r     <- db.run(DBIO.successful(()).flatMap(_ => DBIO.failed(boom)).cleanUp(rec.f)).attempt
      calls <- rec.get
    } yield {
      assertEquals(r, Left(boom))
      assertEquals(calls, Vector(Some(boom)))
    }
  }

  h2Db.test("cleanUp calls f once inside transactionally and withPinnedSession") { db =>
    for {
      rec1   <- recorder
      rec2   <- recorder
      _      <- db.run(items.length.result.cleanUp(rec1.f).transactionally)
      _      <- db.run(items.length.result.cleanUp(rec2.f).withPinnedSession)
      calls1 <- rec1.get
      calls2 <- rec2.get
    } yield {
      assertEquals(calls1, Vector(None))
      assertEquals(calls2, Vector(None))
    }
  }

  h2Db.test("cleanUp calls f once with a CancellationException when the action is canceled") { db =>
    for {
      rec   <- recorder
      r     <- cancelledRun(db, rec)
      calls <- rec.get
    } yield {
      assert(r.isCanceled, r.toString)
      assertCancelledOnce(calls)
    }
  }

  h2Db.test("a partial cleanUp function that does not handle cancellation works on a successful action") { db =>
    val f: PartialFunction[Option[Throwable], DBIO[Unit]] = {
      case None                  => DBIO.successful(())
      case Some(_: SQLException) => DBIO.successful(())
    }
    db.run(DBIO.successful(()).flatMap(_ => items.length.result).cleanUp(f)).map(r => assertEquals(r, 3))
  }

  h2Db.test("cleanUp in the non-streaming prefix of a stream calls f once") { db =>
    for {
      rec   <- recorder
      rows  <- db.stream(DBIO.successful(()).flatMap(_ => items.length.result).cleanUp(rec.f) >>
                 items.sortBy(_.v).result).compile.toList
      calls <- rec.get
    } yield {
      assertEquals(rows, List(1, 2, 3))
      assertEquals(calls, Vector(None))
    }
  }

  heapDb.test("cleanUp calls f once on a heap database") { db =>
    for {
      ok        <- recorder
      failed    <- recorder
      cancelled <- recorder
      _         <- db.run(DBIO.successful(()).flatMap(_ => DBIO.successful(1)).cleanUp(ok.f))
      _         <- db.run(DBIO.successful(()).flatMap(_ => DBIO.failed(boom)).cleanUp(failed.f)).attempt
      _         <- cancelledRun(db, cancelled)
      okCalls   <- ok.get
      errCalls  <- failed.get
      canCalls  <- cancelled.get
    } yield {
      assertEquals(okCalls, Vector(None))
      assertEquals(errCalls, Vector(Some(boom)))
      assertCancelledOnce(canCalls)
    }
  }
}
