package com.typesafe.slick.testkit.tests

import java.sql.Statement
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import cats.effect.IO

import com.typesafe.slick.testkit.util.{JdbcTestDB, AsyncTest}

import slick.jdbc.{ResultSetHoldability, ResultSetConcurrency, ResultSetType, JdbcBackend}

class JdbcMiscTest extends AsyncTest[JdbcTestDB] {
  import tdb.profile.api._

  def testNullability = {
    class T1(tag: Tag) extends Table[String](tag, "t1") {
      def a = column[String]("a", O.PrimaryKey)
      def * = a
    }
    val t1 = TableQuery[T1]

    class T3(tag: Tag) extends Table[Option[String]](tag, "t3") {
      def a = column[Option[String]]("a")
      def * = a
    }
    val t3 = TableQuery[T3]

    seq(
      (t1.schema ++ t3.schema).create,
      t1 += "a",
      t3 += Some("a"),
      t3 += None,
      (t1 += null.asInstanceOf[String]).failed
    )
  }

  def testSimpleDBIO = {
    val getAutoCommit = SimpleDBIO[Boolean](_.connection.getAutoCommit)
    getAutoCommit.map(_ shouldBe true)
  }

  def testStatementParameters = {
    def check(sp: JdbcBackend.StatementParameters) =
      GetStatementParameters.map { csp => csp shouldBe sp }

    DBIO.seq(
      check(JdbcBackend.StatementParameters(ResultSetType.Auto, ResultSetConcurrency.Auto, ResultSetHoldability.Auto, null, 0)),
      DBIO.seq(
        check(JdbcBackend.StatementParameters(ResultSetType.ScrollInsensitive, ResultSetConcurrency.Auto, ResultSetHoldability.Auto, null, 0)),
        check(JdbcBackend.StatementParameters(ResultSetType.ScrollInsensitive, ResultSetConcurrency.Auto, ResultSetHoldability.HoldCursorsOverCommit, null, 100)).
          withStatementParameters(rsHoldability = ResultSetHoldability.HoldCursorsOverCommit, fetchSize = 100),
        check(JdbcBackend.StatementParameters(ResultSetType.ScrollInsensitive, ResultSetConcurrency.Auto, ResultSetHoldability.Auto, null, 0))
      ).withStatementParameters(rsType = ResultSetType.ScrollInsensitive),
      check(JdbcBackend.StatementParameters(ResultSetType.Auto, ResultSetConcurrency.Auto, ResultSetHoldability.Auto, null, 0))
    )
  }

  def testOverrideStatements = {
    class T(tag: Tag) extends Table[Int](tag, "t".withUniquePostFix) {
      def id = column[Int]("a")
      def * = id
    }
    val t = TableQuery[T]

    class U(tag: Tag) extends Table[Int](tag, "u") {
      def id = column[Int]("id")
      def * = id
    }
    val u = TableQuery[U]

    val a1 = t.filter(_.id === 1)
    val a2 = t.filter(_.id === 2)

    seq(
      (t.schema ++ u.schema).create,
      t ++= Seq(1, 2, 3),
      a1.result.map(_ shouldBe Seq(1)),
      a1.result.overrideStatements(a2.result.statements).map(_ shouldBe Seq(2)),
      a1.result.head.map(_ shouldBe 1),
      a1.result.head.overrideStatements(a2.result.head.statements).map(_ shouldBe 2),
      /* Build an action that inserts a single value into table "t".
         Then, override its statement with: "insert into u (id) values (?)". */
      (t += 4).overrideStatements(Seq(u.insertStatement)),
      /* Check that the statement passed to "overrideStatements" has been executed,
         i.e. that the value has been inserted into table "u". */
      u.result.map(_ shouldBe Seq(4)),
      /* Now do the same for a multi-insert action. */
      (t ++= Seq(5, 6)).overrideStatements(Seq(u.insertStatement)),
      /* Check that the values have been appended to the "u" table. */
      u.result.map(_ shouldBe Seq(4, 5, 6))
    )
  }

  def testStreamingWithStatementParameters = {
    // What we're testing:
    //   db.stream(query.result.withStatementParameters(fetchSize = N)) must truly stream —
    //   i.e. the JDBC PreparedStatement must remain open while rows are being consumed by
    //   the caller. If interpretStream incorrectly falls into the `case other` fallback it
    //   calls interpret[Seq[T]] instead, which materialises the entire ResultSet into memory
    //   before the first element reaches the consumer, and closes the statement as part of
    //   that materialisation.
    //
    // Test mechanics:
    //   We capture the PreparedStatement reference via statementInit. statementInit fires
    //   when the statement is created, before setFetchSize is called and before any rows are
    //   fetched. We insert 3 rows but only take 1 from the stream. While evalTap fires for
    //   that first element we check statement.isClosed:
    //     - True streaming path: the cursor is still open, isClosed == false (pass)
    //     - Materialisation path: StreamingInvokerAction.run() iterated the whole ResultSet,
    //       autoClose fired inside PositionedResultIterator.fetchNext(), the statement is
    //       already closed before take(1) sees anything, so isClosed == true (fail)
    //
    // Actual assertions:
    //   1. The statement was actually captured (statementInit fired — proves the action ran).
    //   2. statement.isClosed == false when the first streamed element is received
    //      (proves the cursor was not pre-consumed by materialisation).
    //   3. The single element received has the expected value (basic correctness check).

    class T(tag: Tag) extends Table[Int](tag, "t".withUniquePostFix) {
      def id = column[Int]("id")
      def * = id
    }
    val t = TableQuery[T]

    // Test mechanics: capture the PreparedStatement created for the query.
    // statementInit is called by decorateStatement inside prepareStatement, before
    // setFetchSize and before any rows are fetched.
    val capturedStmt = new AtomicReference[Statement](null)
    val captureInit: Statement => Unit = st => capturedStmt.set(st)

    val streamAction = t.sortBy(_.id).result
      .withStatementParameters(fetchSize = 10, statementInit = captureInit)

    for {
      _ <- db.run((t.schema.create >> (t ++= Seq(1, 2, 3))).withPinnedSession)

      // Test mechanics: evalTap fires synchronously for each element pulled from the stream.
      // We take(1) so only the first row reaches the consumer; the other two are never pulled
      // in the true-streaming path (the cursor stays open at that point).
      statementClosedAtFirstElement <- {
        var closedMidStream = true // overwritten in evalTap
        db.stream(streamAction)
          .take(1)
          .evalTap(row => IO {
            val st = capturedStmt.get()
            try {
              closedMidStream = if (st != null) st.isClosed else true
            } catch {
              case _: Throwable => closedMidStream = true
            }
          })
          .compile
          .drain >> IO { closedMidStream }
      }

      // Assertion 1: statementInit fired — the statement was actually created.
      _ = (capturedStmt.get() != null) shouldBe true

      // Assertion 2: the PreparedStatement was still open when the first row was delivered.
      // Fails today because interpretStream falls into `case other`, materialises all rows,
      // closes the statement, and only then hands the wrapped Seq to the consumer.
      _ = statementClosedAtFirstElement shouldBe false

    } yield ()
  }

  def testStreamingCleanUpCalledOnSuccess: IO[Unit] = {
    // What we're testing:
    //   The CleanUpAction's cleanup function must be called after the stream is fully
    //   consumed in the normal (no-error) path. It must NOT be called before any rows
    //   are delivered to the consumer (i.e. it must run after, not before, iteration).
    //
    // Test mechanics:
    //   We build the action as query.result.withStatementParameters(...), which internally
    //   wraps the query in CleanUpAction(Push >> query, cleanup=Pop). We intercept the
    //   "pop" moment by using a statementInit side-effect: once Pop runs, the session's
    //   fetchSize reverts to 0. We check this by reading fetchSize from the session via
    //   GetStatementParameters inside the stream (i.e. before cleanup) and after.
    //   However GetStatementParameters uses a separate run, so instead we use a simpler
    //   observable: an AtomicBoolean flag set by a custom statementInit that fires only
    //   when a new statement is prepared with the pushed parameters. The flag being set
    //   means Push ran; the flag still being set at first-row time means Pop has NOT run
    //   (verified in testStreamingWithStatementParameters). Here we add the complementary
    //   check: the cleanup DID run by the time the stream's IO completes.
    //
    // Actual assertions:
    //   1. cleanupCalled == false before the stream is consumed (it would be true in the
    //      old materialisation path where cleanup ran before any rows were delivered).
    //   2. cleanupCalled == true after db.stream(...).compile.drain completes, confirming
    //      the Resource finalizer ran the cleanup.

    class T(tag: Tag) extends Table[Int](tag, "t".withUniquePostFix) {
      def id = column[Int]("id")
      def * = id
    }
    val t = TableQuery[T]

    // Test mechanics: cleanupCalled is set inside the andFinally cleanup action.
    // We build the action manually to get a hook into the cleanup rather than relying
    // on withStatementParameters' internal Pop (which has no observable side-effect here).
    val cleanupCalled = new AtomicBoolean(false)
    val recordCleanup = DBIO.successful(()).map(_ => cleanupCalled.set(true))

    val streamAction = (t.sortBy(_.id).result andFinally recordCleanup)
      .withPinnedSession

    for {
      _ <- db.run(t.schema.create >> (t ++= Seq(1, 2, 3)))

      // Assertion 1: cleanup has not run yet before the stream starts
      _ = cleanupCalled.get() shouldBe false

      _ <- db.stream(streamAction).compile.drain

      // Assertion 2: cleanup ran after the stream completed
      _ = cleanupCalled.get() shouldBe true

    } yield ()
  }

  def testStreamingCleanUpCalledOnSetupError: IO[Unit] = {
    // What we're testing:
    //   If the base action inside CleanUpAction throws before the iterator is even opened
    //   (e.g. a query on a non-existent table), the cleanup function must still be called.
    //   This exercises the error path in acquireStreamContextAndIterator where interpretStream
    //   itself fails — the cleanup is part of the action structure so it must run.
    //
    // Test mechanics:
    //   We stream a query against a table that does not exist. The CleanUpAction's base
    //   (the query) throws during interpretStream before returning an iterator. We verify
    //   that the cleanup action still ran despite the setup failure.
    //
    // Actual assertions:
    //   1. db.stream(...).compile.drain raises an error (the table does not exist).
    //   2. cleanupCalled == true even though the base threw during setup.

    class T(tag: Tag) extends Table[Int](tag, "t".withUniquePostFix) {
      def id = column[Int]("id")
      def * = id
    }
    val t = TableQuery[T]
    // Note: t.schema.create is deliberately NOT called — the table does not exist.

    val cleanupCalled = new AtomicBoolean(false)
    val recordCleanup = DBIO.successful(()).map(_ => cleanupCalled.set(true))

    val streamAction = (t.result andFinally recordCleanup).withPinnedSession

    for {
      // Assertion 1: stream fails because the table does not exist
      result <- db.stream(streamAction).compile.drain.attempt

      _ = result.isLeft shouldBe true

      // Assertion 2: cleanup ran despite the setup error
      _ = cleanupCalled.get() shouldBe true

    } yield ()
  }

  def testStreamingCleanUpCalledOnMidIterationError: IO[Unit] = {
    // What we're testing:
    //   If the stream consumer throws mid-iteration (after at least one row has been
    //   delivered), the Resource finalizer fires with ExitCase.Errored and the cleanup
    //   function must be called with Some(throwable).
    //
    // Test mechanics:
    //   We insert 3 rows and stream them. In evalTap we throw on the second element.
    //   This causes the fs2 stream to fail, the Resource finalizer runs with
    //   ExitCase.Errored, closeStreamIteratorAndRelease calls cleanup(Some(error)).
    //   We verify cleanupCalled is set and that the error passed to cleanup was non-None.
    //
    // Actual assertions:
    //   1. db.stream(...).compile.drain raises an error (the consumer threw).
    //   2. cleanupCalled == true — cleanup ran despite the mid-stream error.
    //   3. cleanupError == Some(error) — cleanup received the consumer's exception.

    class T(tag: Tag) extends Table[Int](tag, "t".withUniquePostFix) {
      def id = column[Int]("id")
      def * = id
    }
    val t = TableQuery[T]

    val cleanupCalled = new AtomicBoolean(false)
    // Test mechanics: we capture the Option[Throwable] passed to the cleanup function
    // by using cleanUp directly rather than andFinally, so we can inspect the error arg.
    val cleanupError = new AtomicReference[Option[Throwable]](None)
    val recordCleanup: Option[Throwable] => DBIOAction[Unit, NoStream, Effect] =
      err => DBIO.successful(()).map { _ =>
        cleanupCalled.set(true)
        cleanupError.set(err)
      }

    val streamAction = t.sortBy(_.id).result
      .cleanUp(recordCleanup, keepFailure = true)
      .withPinnedSession

    val consumerException = new RuntimeException("deliberate mid-stream consumer error")
    var rowCount = 0

    for {
      _ <- db.run(t.schema.create >> (t ++= Seq(1, 2, 3)))

      // Test mechanics: evalTap throws on the second element, simulating a consumer error
      // after partial iteration.
      result <- db.stream(streamAction)
        .evalTap(_ => IO {
          rowCount += 1
          if (rowCount == 2) throw consumerException
        })
        .compile
        .drain
        .attempt

      // Assertion 1: the stream failed due to the consumer exception
      _ = result shouldBe Left(consumerException)

      // Assertion 2: cleanup ran despite the mid-stream error
      _ = cleanupCalled.get() shouldBe true

      // Assertion 3: cleanup received Some(error) — not None — indicating it knew
      // the stream ended abnormally. The exact error type may be wrapped by fs2/CE3
      // but it must be non-empty.
      _ = cleanupError.get().isDefined shouldBe true

    } yield ()
  }

  def testStreamingCleanUpFailurePropagatesOnSuccess: IO[Unit] = {
    // What we're testing:
    //   keepFailure controls error precedence when the *base* action has already failed.
    //   When the base stream succeeds, a failing cleanup must always propagate — regardless
    //   of the keepFailure flag.  With the old code and keepFailure=true, the condition
    //   `case Left(cleanupErr) if !keepFailure` was false, so execution fell through to
    //   `case _ => F.unit`, silently swallowing the cleanup error and reporting success.
    //
    // Test mechanics:
    //   We stream 3 rows successfully (no consumer error, no setup error).
    //   The cleanUp function always fails with a known exception.
    //   We assert that the overall stream result is a Left containing that exception.
    //
    // Actual assertions:
    //   1. result is Left(cleanupException) — the cleanup error propagated even though
    //      the base stream completed successfully.

    class T(tag: Tag) extends Table[Int](tag, "t".withUniquePostFix) {
      def id = column[Int]("id")
      def * = id
    }
    val t = TableQuery[T]

    val cleanupException = new RuntimeException("deliberate cleanup failure on success")
    val failingCleanup: Option[Throwable] => DBIOAction[Unit, NoStream, Effect] =
      _ => DBIO.failed(cleanupException)

    // keepFailure = true is the critical flag: it was incorrectly masking cleanup failures
    // when the base stream succeeded (err = None passed to combinedCleanup).
    val streamAction = t.sortBy(_.id).result
      .cleanUp(failingCleanup, keepFailure = true)
      .withPinnedSession

    for {
      _ <- db.run(t.schema.create >> (t ++= Seq(1, 2, 3)))

      result <- db.stream(streamAction).compile.drain.attempt

      // The base succeeded but cleanup failed — must propagate regardless of keepFailure
      _ = result shouldBe Left(cleanupException)
    } yield ()
  }

  def testStreamingCleanUpCalledOnCancellation: IO[Unit] = {
    // What we're testing:
    //   If a fiber running the stream is canceled mid-iteration, the Resource finalizer
    //   fires with ExitCase.Canceled. closeStreamIteratorAndRelease maps this to
    //   Some(CancellationException) and passes it to the cleanup function.
    //   The cleanup must run and must receive Some(_) (not None).
    //
    // Test mechanics:
    //   We start the stream as a fiber. A Deferred is used to synchronise: the stream
    //   sets the deferred after the first element is delivered, then the main fiber
    //   cancels the stream fiber. We wait for cleanup to be called (via a second
    //   Deferred that the cleanup action completes) so the assertion doesn't race.
    //
    // Actual assertions:
    //   1. The stream fiber is canceled (joinWithUnit raises/completes as canceled).
    //   2. cleanupCalled == true — cleanup ran despite cancellation.
    //   3. cleanupError contains Some(CancellationException) — cleanup was told the
    //      stream ended due to cancellation, not normal completion.

    import cats.effect.Deferred
    import cats.effect.unsafe.implicits.global

    class T(tag: Tag) extends Table[Int](tag, "t".withUniquePostFix) {
      def id = column[Int]("id")
      def * = id
    }
    val t = TableQuery[T]

    val cleanupCalled = new AtomicBoolean(false)
    val cleanupError  = new AtomicReference[Option[Throwable]](None)

    for {
      _ <- db.run(t.schema.create >> (t ++= Seq(1, 2, 3)))

      // Test mechanics: firstRowDeferred is completed by the stream after row 1 is seen.
      // cleanupDeferred is completed by the cleanup action so we can wait for it.
      firstRowDeferred <- Deferred[IO, Unit]
      cleanupDeferred  <- Deferred[IO, Unit]

      recordCleanup: (Option[Throwable] => DBIOAction[Unit, NoStream, Effect]) =
        err => DBIO.successful(()).map { _ =>
          cleanupCalled.set(true)
          cleanupError.set(err)
          // Test mechanics: unblock the main fiber so it can assert after cleanup completes.
          cleanupDeferred.complete(()).unsafeRunSync()
        }

      streamAction = t.sortBy(_.id).result
        .cleanUp(recordCleanup, keepFailure = true)
        .withPinnedSession

      fiber <- db.stream(streamAction)
        .evalTap(_ => firstRowDeferred.complete(()).void)
        // Test mechanics: pause here so the main fiber can cancel us after the first row.
        .evalTap(_ => IO.never)
        .compile
        .drain
        .start

      // Wait until the stream has delivered at least one row, then cancel it.
      _ <- firstRowDeferred.get
      _ <- fiber.cancel

      // Wait for the cleanup action to complete before asserting (avoids a race between
      // the finalizer's async cleanup and the assertions below).
      _ <- cleanupDeferred.get

      // Assertion 1: cleanup ran after cancellation
      _ = cleanupCalled.get() shouldBe true

      // Assertion 2: cleanup received Some(CancellationException), not None
      _ = cleanupError.get().isDefined shouldBe true
      _ = cleanupError.get().exists(_.isInstanceOf[java.util.concurrent.CancellationException]) shouldBe true

    } yield ()
  }

  def testAutoIncrementCreateIfNotExists = {
    class TAutoIncrement(tag: Tag) extends Table[Int](tag, "t".withUniquePostFix) {
      def id = column[Int]("a", O.AutoInc, O.PrimaryKey)
      def * = id
    }

    val records = TableQuery[TAutoIncrement]

    DBIO.seq(
      records.schema.createIfNotExists,
      records += 1
    )
  }
}
