package slick.test.compat

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import munit.FunSuite
import org.reactivestreams.Publisher

import slick.jdbc.H2Profile.api.*
import slick.jdbc.JdbcBackend
import slick.compat.*

/** Tests for [[slick.compat.Database]] and the `slick.compat` package enrichments.
  *
  * Covers:
  *   - `Database.forURL` factory — construction and `close()`
  *   - `Database.wrap` factory — caller-managed lifecycle
  *   - `db.run(action)` returns `Future[R]`
  *   - `db.stream(action)` returns `Publisher[T]` (Reactive Streams)
  *   - `DBIO.from(future)` enrichment — Future overload resolves correctly
  *   - `db.streamAsPublisher(action)` extension on `Database[IO]`
  */
class V3CompatTest extends FunSuite {

  // ---------------------------------------------------------------------------
  // Schema
  // ---------------------------------------------------------------------------

  class Items(tag: Tag) extends Table[Int](tag, "V3COMPAT_ITEMS") {
    def v = column[Int]("V")
    def * = v
  }
  val items = TableQuery[Items]

  // ---------------------------------------------------------------------------
  // Helpers
  // ---------------------------------------------------------------------------

  private def await[A](f: Future[A]): A = Await.result(f, 10.seconds)

  /** Fresh in-memory H2 database for each test, via forURL factory. */
  private def withDb[A](testName: String)(f: slick.compat.Database => A): A = {
    val db = slick.compat.Database.forURL(
      s"jdbc:h2:mem:v3compat_$testName;DB_CLOSE_DELAY=-1",
      driver = "org.h2.Driver",
      keepAliveConnection = true
    )
    try f(db)
    finally db.close()
  }

  // ---------------------------------------------------------------------------
  // db.run — returns Future[R]
  // ---------------------------------------------------------------------------

  test("db.run returns Future[R] for a non-streaming action") {
    withDb("run") { db =>
      await(db.run(items.schema.create))
      await(db.run(items ++= Seq(1, 2, 3)))
      val result = await(db.run(items.result))
      assertEquals(result.toSet, Set(1, 2, 3))
    }
  }

  test("db.run propagates DBIO failures as failed Future") {
    withDb("run_failure") { db =>
      // Run against a table that doesn't exist → SQL exception
      val f = db.run(items.result)
      intercept[Exception](await(f))
    }
  }

  // ---------------------------------------------------------------------------
  // db.stream — returns Publisher[T]
  // ---------------------------------------------------------------------------

  test("db.stream returns a Publisher[T] that delivers all elements") {
    withDb("stream") { db =>
      await(db.run(items.schema.create))
      await(db.run(items ++= Seq(10, 20, 30)))

      val publisher: Publisher[Int] = db.stream(items.sortBy(_.v).result)
      val collected = collectPublisher(publisher)
      assertEquals(collected, List(10, 20, 30))
    }
  }

  // ---------------------------------------------------------------------------
  // DBIO.from(future) — Future overload via implicit enrichment
  // ---------------------------------------------------------------------------

  test("DBIO.from accepts a Future[R] after import slick.compat.*") {
    withDb("dbio_from_future") { db =>
      // This must compile and resolve to the Future overload, not the generic F[_] one.
      def externalCall(): Future[Int] = Future.successful(42)

      val action: DBIO[Int] = DBIO.from(externalCall())
      val result = await(db.run(action))
      assertEquals(result, 42)
    }
  }

  test("DBIO.from(future) composes correctly inside a for-comprehension") {
    withDb("dbio_from_compose") { db =>
      await(db.run(items.schema.create))

      def lookupOffset(): Future[Int] = Future.successful(100)

      val action: DBIO[Seq[Int]] = for {
        offset <- DBIO.from(lookupOffset())
        _      <- items ++= Seq(offset + 1, offset + 2)
        rows   <- items.result
      } yield rows

      val result = await(db.run(action))
      assertEquals(result.toSet, Set(101, 102))
    }
  }

  // ---------------------------------------------------------------------------
  // Database.wrap — caller-managed lifecycle
  // ---------------------------------------------------------------------------

  test("Database.wrap produces a working Database whose close() is a no-op") {
    // Allocate the underlying Database[IO] and Dispatcher[IO] manually
    val dbResource = JdbcBackend.Database
      .forURL[IO]("jdbc:h2:mem:v3compat_wrap;DB_CLOSE_DELAY=-1", driver = "org.h2.Driver", keepAliveConnection = true)
    val combined = dbResource.flatMap(d => Dispatcher.parallel[IO].map(disp => (d, disp)))
    val ((underlyingDb, dispatcher), releaseAll) = combined.allocated.unsafeRunSync()

    val db = slick.compat.Database.wrap(underlyingDb, dispatcher)

    try {
      await(db.run(items.schema.create))
      await(db.run(items ++= Seq(7, 8, 9)))
      val result = await(db.run(items.result))
      assertEquals(result.toSet, Set(7, 8, 9))

      // close() on a wrapped Database must be a no-op — the underlying resources are still open
      db.close()

      // The underlying Database[IO] is still usable after wrap's close()
      val stillWorks = underlyingDb.run(items.result).unsafeRunSync()
      assertEquals(stillWorks.toSet, Set(7, 8, 9))
    } finally {
      releaseAll.unsafeRunSync()
    }
  }

  // ---------------------------------------------------------------------------
  // streamAsPublisher extension on Database[IO] (DatabasePublisherOps)
  // ---------------------------------------------------------------------------

  test("db.streamAsPublisher produces a Publisher[T] from a Database[IO]") {
    val dbResource = JdbcBackend.Database
      .forURL[IO]("jdbc:h2:mem:v3compat_streampub;DB_CLOSE_DELAY=-1", driver = "org.h2.Driver", keepAliveConnection = true)
    val combined = dbResource.flatMap(d => Dispatcher.parallel[IO].map(disp => (d, disp)))
    val ((underlyingDb, dispatcher), releaseAll) = combined.allocated.unsafeRunSync()

    try {
      underlyingDb.run(items.schema.create >> (items ++= Seq(5, 6, 7))).unsafeRunSync()

      implicit val disp: Dispatcher[IO] = dispatcher
      val publisher: Publisher[Int] = underlyingDb.streamAsPublisher(items.sortBy(_.v).result)
      assertEquals(collectPublisher(publisher), List(5, 6, 7))
    } finally {
      releaseAll.unsafeRunSync()
    }
  }

  // ---------------------------------------------------------------------------
  // Streaming lifecycle — connection pinning, full consumption, cancellation
  // ---------------------------------------------------------------------------

  /** Number of free connection slots on the underlying Database[IO].
    * `maxConnections - freePermits` = number of connections currently checked out. */
  private def freePermits(db: slick.compat.Database): Long =
    db.underlying.availableConnectionSlots.unsafeRunSync()

  test("stream: connection is pinned while streaming and released after full consumption") {
    withDb("stream_pin_consume") { db =>
      await(db.run(items.schema.create))
      await(db.run(items ++= Seq(1, 2, 3, 4, 5)))

      val initialFree = freePermits(db)

      import java.util.concurrent.{CountDownLatch, CopyOnWriteArrayList, TimeUnit}
      import org.reactivestreams.{Subscriber, Subscription}

      val results           = new CopyOnWriteArrayList[Int]()
      val firstElementLatch = new CountDownLatch(1)  // fires after the first onNext
      val proceedLatch      = new CountDownLatch(1)  // test releases this to let stream finish
      val doneLatch         = new CountDownLatch(1)  // fires on onComplete / onError
      @volatile var midStreamFree: Long    = -1
      @volatile var afterCompleteFree: Long = -1
      @volatile var sub: Subscription      = null

      db.stream(items.sortBy(_.v).result).subscribe(new Subscriber[Int] {
        def onSubscribe(s: Subscription): Unit = { sub = s; s.request(1) }
        def onNext(t: Int): Unit = {
          results.add(t)
          firstElementLatch.countDown()
          // Pause here so the test can sample mid-stream permit count before requesting more.
          proceedLatch.await(10, TimeUnit.SECONDS)
          sub.request(Long.MaxValue)
        }
        def onError(t: Throwable): Unit = { doneLatch.countDown() }
        def onComplete(): Unit = {
          afterCompleteFree = freePermits(db)
          doneLatch.countDown()
        }
      })

      assert(firstElementLatch.await(10, TimeUnit.SECONDS), "timed out waiting for first element")
      midStreamFree = freePermits(db)
      proceedLatch.countDown()

      assert(doneLatch.await(10, TimeUnit.SECONDS), "timed out waiting for onComplete")

      assertEquals(results.size(), 5)
      // One permit consumed while streaming.
      assertEquals(midStreamFree, initialFree - 1L,
        "expected exactly one connection checked out mid-stream")
      // Permit returned after stream completes.
      assertEquals(afterCompleteFree, initialFree,
        "expected connection returned to pool after full consumption")
    }
  }

  test("stream: connection is released when the subscriber cancels before full consumption") {
    withDb("stream_pin_cancel") { db =>
      await(db.run(items.schema.create))
      await(db.run(items ++= Seq(1, 2, 3, 4, 5)))

      val initialFree = freePermits(db)

      import java.util.concurrent.{CountDownLatch, TimeUnit}
      import org.reactivestreams.{Subscriber, Subscription}

      val firstElementLatch = new CountDownLatch(1)
      val doneLatch         = new CountDownLatch(1)
      @volatile var sub: Subscription       = null
      @volatile var midStreamFree: Long     = -1
      @volatile var afterCancelFree: Long   = -1

      db.stream(items.sortBy(_.v).result).subscribe(new Subscriber[Int] {
        def onSubscribe(s: Subscription): Unit = { sub = s; s.request(1) }
        def onNext(t: Int): Unit               = firstElementLatch.countDown()
        def onError(t: Throwable): Unit        = doneLatch.countDown()
        def onComplete(): Unit                 = doneLatch.countDown()
      })

      assert(firstElementLatch.await(10, TimeUnit.SECONDS), "timed out waiting for first element")
      midStreamFree = freePermits(db)

      // Cancel — FS2 will interrupt the stream fiber and release the connection via bracketCase.
      sub.cancel()

      // Poll until the semaphore permit is returned (cancellation is async).
      val deadline = System.currentTimeMillis() + 5000L
      while (freePermits(db) < initialFree && System.currentTimeMillis() < deadline)
        Thread.sleep(20)
      afterCancelFree = freePermits(db)

      // One permit consumed while the first element was in flight.
      assertEquals(midStreamFree, initialFree - 1L,
        "expected exactly one connection checked out mid-stream")
      // Permit returned after cancellation.
      assertEquals(afterCancelFree, initialFree,
        "expected connection returned to pool after subscriber cancel")
    }
  }

  // ---------------------------------------------------------------------------
  // Helper: synchronously collect a Publisher[T] into a List
  // ---------------------------------------------------------------------------

  private def collectPublisher[T](publisher: Publisher[T]): List[T] = {
    import java.util.concurrent.{CountDownLatch, CopyOnWriteArrayList}
    import org.reactivestreams.{Subscriber, Subscription}

    val results = new CopyOnWriteArrayList[T]()
    val latch   = new CountDownLatch(1)
    @volatile var error: Throwable = null

    publisher.subscribe(new Subscriber[T] {
      def onSubscribe(s: Subscription): Unit = s.request(Long.MaxValue)
      def onNext(t: T): Unit                 = results.add(t)
      def onError(t: Throwable): Unit        = { error = t; latch.countDown() }
      def onComplete(): Unit                 = latch.countDown()
    })

    latch.await(10, java.util.concurrent.TimeUnit.SECONDS)
    if (error != null) throw error
    import scala.jdk.CollectionConverters.*
    results.asScala.toList
  }
}
