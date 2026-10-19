package slick.test.future

import java.util.concurrent.RejectedExecutionException
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.concurrent.duration.*

import org.junit.Assert.*
import org.junit.{After, Before, Test}
import org.reactivestreams.{Subscriber, Subscription}

import slick.future.{Database, DatabasePublisher}
import slick.jdbc.{DatabaseConfig, GetResult, H2Profile}

/** A stream that fails while its rows are read or consumed rolls back its transaction and
  * reports the error; completion and cancellation commit. */
class StreamOutcomeTest {
  implicit val ec: ExecutionContext = ExecutionContext.global

  import H2Profile.api.*

  class Items(tag: Tag) extends Table[Int](tag, "STREAM_OUTCOME_ITEMS") {
    def id = column[Int]("ID")
    def * = id
  }

  private val items = TableQuery[Items]
  // A database of its own for each test, so that tests can't see each other's rows even if they run concurrently
  private val dc = DatabaseConfig.forURL(H2Profile,
    s"jdbc:h2:mem:StreamOutcomeTest-${java.util.UUID.randomUUID()};DB_CLOSE_DELAY=-1", driver = "org.h2.Driver")
  private val timeout = 30.seconds
  private var db: Database = null

  /** The row that `txAction` inserts inside the transaction before streaming. */
  private val insertedRow = 100

  private val failingGetResult: GetResult[Int] = GetResult { r =>
    val v = r.nextInt()
    if (v == 3) throw new RuntimeException("GetResult fails on row 3")
    v
  }

  /** Insert `insertedRow`, then stream all rows, in one transaction. */
  private def txAction(getResult: GetResult[Int] = GetResult.GetInt): DBIOAction[Seq[Int], Streaming[Int], Effect.All] = {
    implicit val rowResult: GetResult[Int] = getResult
    ((items += insertedRow) >> sql"select ID from STREAM_OUTCOME_ITEMS order by ID".as[Int]).transactionally
  }

  private def await[A](f: Future[A]): A = Await.result(f, timeout)

  private def insertedRowVisible: Boolean =
    await(db.run(items.filter(_.id === insertedRow).length.result)) > 0

  private def assertRolledBack(): Unit =
    assertFalse("the transaction must be rolled back", insertedRowVisible)

  /** The publisher may release the stream just after the subscriber has seen its last element,
    * so wait for the commit to become visible. Each poll is a quick query; one that hangs fails
    * the test through its own timeout. */
  private def assertEventuallyCommitted(): Unit = {
    val deadline = timeout.fromNow
    while (!insertedRowVisible && deadline.hasTimeLeft()) Thread.sleep(10)
    assertTrue("the transaction must be committed", insertedRowVisible)
  }

  /** Records the argument of a `cleanUp` call. It starts as `null`, not `None`, so that a test
    * expecting `None` fails if `cleanUp` is never called. */
  private def cleanUpArgument() = new AtomicReference[Option[Throwable]](null)

  /** The throwable the future failed with, including fatal ones that `Try` would not catch. */
  private def failure(f: Future[?]): Throwable =
    (try Right(await(f)) catch { case e: Throwable => Left(e) }) match {
      case Left(e)  => e
      case Right(v) => throw new AssertionError(s"expected a failure but got $v")
    }

  /** Subscribe, request one element at a time, and complete with all elements; `onNext` throws
    * on `throwOn`. */
  private def subscribeAll(publisher: DatabasePublisher[Int], throwOn: Option[Int] = None): Future[List[Int]] = {
    val promise = Promise[List[Int]]()
    val buf = List.newBuilder[Int]
    publisher.subscribe(new Subscriber[Int] {
      private var subscription: Subscription = null
      def onSubscribe(s: Subscription): Unit = { subscription = s; s.request(1) }
      def onNext(x: Int): Unit = {
        if (throwOn.contains(x)) throw new RuntimeException(s"onNext fails on $x")
        buf += x
        subscription.request(1)
      }
      def onError(e: Throwable): Unit = promise.tryFailure(e)
      def onComplete(): Unit = promise.trySuccess(buf.result())
    })
    promise.future
  }

  /** Subscribe, take the first element and cancel. */
  private def subscribeTakeOne(publisher: DatabasePublisher[Int]): Future[Int] = {
    val promise = Promise[Int]()
    publisher.subscribe(new Subscriber[Int] {
      private var subscription: Subscription = null
      def onSubscribe(s: Subscription): Unit = { subscription = s; s.request(1) }
      def onNext(x: Int): Unit = { subscription.cancel(); promise.trySuccess(x) }
      def onError(e: Throwable): Unit = promise.tryFailure(e)
      def onComplete(): Unit = promise.tryFailure(new AssertionError("completed without elements"))
    })
    promise.future
  }

  private def assertStackOverflow(e: Throwable): Unit = {
    assertTrue(s"expected a SlickException but got $e", e.isInstanceOf[SlickException])
    assertTrue(s"expected a StackOverflowError cause but got ${e.getCause}", e.getCause.isInstanceOf[StackOverflowError])
  }

  private def recurse(n: Int): Int = if (n == 0) 0 else 1 + recurse(n - 1)

  /** An ExecutionContext that runs the first `accepted` tasks on the global one and rejects the
    * rest, as a shut-down executor does. */
  private def rejectingAfter(accepted: Int): ExecutionContext = new ExecutionContext {
    private val submitted = new AtomicInteger(0)

    def execute(runnable: Runnable): Unit =
      if (submitted.incrementAndGet() <= accepted) ExecutionContext.global.execute(runnable)
      else throw new RejectedExecutionException("executor shut down")
    def reportFailure(cause: Throwable): Unit = ExecutionContext.global.reportFailure(cause)
  }

  /** An ExecutionContext that runs a task on a new thread and a task submitted from within a task
    * inline, as a saturated executor with `CallerRunsPolicy` does. */
  private def inlineNested: ExecutionContext = new ExecutionContext {
    private val inTask = new ThreadLocal[Boolean] { override def initialValue(): Boolean = false }

    def execute(runnable: Runnable): Unit =
      if (inTask.get) runnable.run()
      else {
        val thread = new Thread(() => { inTask.set(true); runnable.run() })
        // A fatal error propagates out of the task as intended; don't print it as uncaught
        thread.setUncaughtExceptionHandler((_, _) => ())
        thread.start()
      }
    def reportFailure(cause: Throwable): Unit = ExecutionContext.global.reportFailure(cause)
  }

  @Before
  def setUp(): Unit = {
    db = await(Database.open(dc))
    await(db.run(items.schema.dropIfExists >> items.schema.create >> (items ++= Seq(1, 2, 3, 4, 5))))
  }

  @After
  def tearDown(): Unit =
    if (db != null) {
      try await(db.run(items.schema.dropIfExists))
      finally db.close()
    }

  @Test
  def foreachConsumerFailureRollsBack(): Unit = {
    val e = failure(db.stream(txAction()).foreach(x => if (x == 3) throw new RuntimeException("consumer fails")))
    assertEquals("consumer fails", e.getMessage)
    assertRolledBack()
  }

  @Test
  def foreachReadFailureRollsBack(): Unit = {
    val e = failure(db.stream(txAction(failingGetResult)).foreach(_ => ()))
    assertEquals("GetResult fails on row 3", e.getMessage)
    assertRolledBack()
  }

  @Test
  def mapResultFailureRollsBack(): Unit = {
    val e = failure(db.stream(txAction()).mapResult(x => if (x == 3) throw new RuntimeException("mapResult fails") else x).foreach(_ => ()))
    assertEquals("mapResult fails", e.getMessage)
    assertRolledBack()
  }

  @Test
  def subscriberReadFailureRollsBack(): Unit = {
    val e = failure(subscribeAll(db.stream(txAction(failingGetResult))))
    assertEquals("GetResult fails on row 3", e.getMessage)
    assertRolledBack()
  }

  @Test
  def subscriberOnNextFailureRollsBack(): Unit = {
    val e = failure(subscribeAll(db.stream(txAction()), throwOn = Some(3)))
    assertEquals("onNext fails on 3", e.getMessage)
    assertRolledBack()
  }

  @Test
  def subscriberNonPositiveRequestRollsBack(): Unit = {
    val promise = Promise[Unit]()
    db.stream(txAction()).subscribe(new Subscriber[Int] {
      def onSubscribe(s: Subscription): Unit = s.request(0)
      def onNext(x: Int): Unit = ()
      def onError(e: Throwable): Unit = promise.tryFailure(e)
      def onComplete(): Unit = promise.trySuccess(())
    })
    assertTrue(failure(promise.future).isInstanceOf[IllegalArgumentException])
    assertRolledBack()
  }

  @Test
  def subscriberCancelCommits(): Unit = {
    assertEquals(1, await(subscribeTakeOne(db.stream(txAction()))))
    assertEventuallyCommitted()
  }

  @Test
  def subscriberFullConsumptionCommits(): Unit = {
    assertEquals(List(1, 2, 3, 4, 5, insertedRow), await(subscribeAll(db.stream(txAction()))))
    assertEventuallyCommitted()
  }

  @Test
  def foreachFullConsumptionCommits(): Unit = {
    await(db.stream(txAction()).foreach(_ => ()))
    assertTrue("the transaction must be committed", insertedRowVisible)
  }

  @Test
  def cleanUpSeesStreamFailure(): Unit = {
    val seen = cleanUpArgument()
    val action = txAction().cleanUp { e => seen.set(e); DBIO.successful(()) }
    val e = failure(db.stream(action).foreach(x => if (x == 3) throw new RuntimeException("consumer fails")))
    assertEquals(Some(e), seen.get)
  }

  @Test
  def cleanUpSeesStreamSuccess(): Unit = {
    val seen = cleanUpArgument()
    val action = txAction().cleanUp { e => seen.set(e); DBIO.successful(()) }
    await(db.stream(action).foreach(_ => ()))
    assertEquals(None, seen.get)
  }

  @Test
  def foreachRejectedBeforeAcquireFails(): Unit = {
    implicit val ec: ExecutionContext = rejectingAfter(0) // shadows the default ec
    val e = failure(db.stream(txAction()).foreach(_ => ()))
    assertTrue(s"expected a RejectedExecutionException but got $e", e.isInstanceOf[RejectedExecutionException])
    assertRolledBack()
  }

  @Test
  def foreachRejectedAfterAcquireReleasesAndRollsBack(): Unit = {
    val seen = cleanUpArgument()
    val action = txAction().cleanUp { e => seen.set(e); DBIO.successful(()) }
    // The first task acquires the stream; the executor rejects the task that would consume it.
    implicit val ec: ExecutionContext = rejectingAfter(1) // shadows the default ec
    val e = failure(db.stream(action).foreach(_ => ()))
    assertTrue(s"expected a RejectedExecutionException but got $e", e.isInstanceOf[RejectedExecutionException])
    assertEquals("the stream must be released as failed", Some(e), seen.get)
    assertRolledBack()
  }

  @Test
  def foreachStackOverflowFailsAndRollsBack(): Unit = {
    assertStackOverflow(failure(db.stream(txAction()).foreach(x => if (x == 3) throw new StackOverflowError("consumer overflows"))))
    assertRolledBack()
  }

  @Test
  def foreachDeepRecursionFailsAndRollsBack(): Unit = {
    assertStackOverflow(failure(db.stream(txAction()).foreach(x => if (x == 3) recurse(Int.MaxValue))))
    assertRolledBack()
  }

  @Test
  def mapResultStackOverflowFailsAndRollsBack(): Unit = {
    val publisher = db.stream(txAction()).mapResult(x => if (x == 3) throw new StackOverflowError("mapResult overflows") else x)
    assertStackOverflow(failure(publisher.foreach(_ => ())))
    assertRolledBack()
  }

  @Test
  def subscriberStackOverflowFailsAndRollsBack(): Unit = {
    val publisher = db.stream(txAction()).mapResult(x => if (x == 3) throw new StackOverflowError("mapResult overflows") else x)
    assertStackOverflow(failure(subscribeAll(publisher)))
    assertRolledBack()
  }

  @Test
  def foreachFatalErrorInInlineNestedTaskFails(): Unit = {
    // The stream is acquired on another thread, and the task that consumes it runs inline there
    implicit val ec: ExecutionContext = inlineNested // shadows the default ec
    val e = failure(db.stream(txAction()).foreach(x => if (x == 3) throw new LinkageError("consumer fails fatally")))
    // A Promise boxes a fatal error in an ExecutionException
    assertTrue(s"expected a LinkageError cause but got $e", e.getCause.isInstanceOf[LinkageError])
    assertRolledBack()
  }
}
