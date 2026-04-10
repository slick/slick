package slick.basic

import java.io.Closeable

import cats.effect.{Async, Ref, Resource}
import cats.effect.kernel.Outcome
import cats.syntax.all.*
import cats.effect.syntax.all.*
import fs2.Stream


import slick.SlickException
import slick.compat.collection.*
import slick.dbio.*
import slick.util.*
import slick.basic.ConcurrencyControl.*

import com.typesafe.config.Config
import org.slf4j.LoggerFactory

import ClassLoaderUtil.defaultClassLoader

/** Backend for the basic database and session handling features.
  * Concrete backends like `JdbcBackend` extend this type and provide concrete
  * types for `Database`, `DatabaseFactory` and `Session`. */
trait BasicBackend { self =>
  protected lazy val actionLogger = new SlickLogger(LoggerFactory.getLogger(classOf[BasicBackend].getName+".action"))
  protected lazy val streamLogger = new SlickLogger(LoggerFactory.getLogger(classOf[BasicBackend].getName+".stream"))

  /** Non-parameterized marker trait for any database instance, regardless of effect type.
    * Use this type when you need to refer to "any database" without knowing the effect type. */
  trait AnyDatabaseDef extends Closeable {
    /** Create a new session. The session needs to be closed explicitly by calling its close() method. */
    def createSession(): BasicSessionDef
  }

  /** The type of database objects used by this backend, parameterized by effect type. */
  type Database[F[_]] >: Null <: BasicDatabaseDef[F]
  /** The type of the database factory used by this backend. */
  type DatabaseFactory >: Null
  /** The type of session objects used by this backend. */
  type Session >: Null <: BasicSessionDef
  /** The type of the context used for running SynchronousDatabaseActions */
  type Context >: Null <: BasicActionContext
  /** The type of the context used for streaming SynchronousDatabaseActions */
  type StreamingContext >: Null <: Context & BasicStreamingActionContext

  /** The database factory */
  val Database: DatabaseFactory

  /** Create a Database instance through [[https://github.com/typesafehub/config Typesafe Config]].
    * The supported config keys are backend-specific. This method is used by `DatabaseConfig`.
    *
    * Returns a `Resource[F, Database[F]]` that manages the database lifecycle.
    *
    * @param path The path in the configuration file for the database configuration, or an empty
    *             string for the top level of the `Config` object.
    * @param config The `Config` object to read from.
    * @param classLoader The ClassLoader to use for loading custom classes.
    */
  def createDatabase[F[_]: Async](config: Config, path: String, classLoader: ClassLoader = defaultClassLoader): Resource[F, Database[F]]

  // -----------------------------------------------------------------------
  // Execution state
  // -----------------------------------------------------------------------

  /** Per-`db.run` execution state carried through the interpreter via a CE3 `Ref`.
    *
    * `session` is typed as `Option[AnyRef]` rather than `Option[Session]` to avoid
    * path-dependent type issues with the abstract `Session` type member in Scala 2.13.
    * Callers cast to `Session` as needed.
    *
    * `sessionRelease` holds the finalizer produced by `Resource.allocated` for the
    * currently-open session + semaphore permit pair.  It is set when a session is first
    * acquired and cleared (and run) when the session is released. */
  case class ExecState(
    ordinal:               Long,             // globally assigned submission order for this DBIO chain
    session:               Option[AnyRef],   // None = no JDBC connection acquired yet; cast to Session; AnyRef avoids Scala 2.13 path-dependent type issues with abstract type member Session
    sessionRelease:        Option[AnyRef],   // None = no session open; Some(F[Unit]) = finalizer to close session + release connection slot; AnyRef for the same reason as session
    transactionDepth:      Int,              // nesting depth of .transactionally scopes (0 = no transaction)
    isolationLevel:        Option[Int],      // None = database default
    previousIsolationLevel: Option[Int],     // isolation level to restore after outermost transaction ends
    pinnedDepth:           Int               // nesting depth of withPinnedSession scopes (0 = not pinned)
  ) {
    def inTransaction: Boolean = transactionDepth > 0
    /** The session must be held open if there is an active transaction or pinned scope. */
    def pinned: Boolean = pinnedDepth > 0 || transactionDepth > 0
  }

  object ExecState {
    def initial(ordinal: Long): ExecState = ExecState(
      ordinal                = ordinal,
      session                = None,
      sessionRelease         = None,
      transactionDepth       = 0,
      isolationLevel         = None,
      previousIsolationLevel = None,
      pinnedDepth            = 0
    )
  }

  // -----------------------------------------------------------------------
  // Database definition
  // -----------------------------------------------------------------------

  /** A database instance to which connections can be created.
    *
    * `F[_]` is the effect type (e.g. `cats.effect.IO`).
    * All `run` and `stream` calls on a given database instance use the same effect type.
    *
    * Concrete subclasses supply:
    *   - `asyncF`
    *   - `controls` (admission control + connection arbiter)
    */
  trait BasicDatabaseDef[F[_]] extends AnyDatabaseDef { this: Database[F] =>

    implicit val asyncF: Async[F]

    /** Concurrency controls for admission and connection-slot arbitration. */
    val controls: Controls[F]

    protected final def admissionControl: AdmissionControl[F] = controls.admissionControl
    protected final def connectionArbiter: ConnectionArbiter[F] = controls.connectionArbiter

    /** Create a new session. The session needs to be closed explicitly by calling its close() method. */
    def createSession(): Session

    /** Free all resources allocated by Slick for this Database. */
    override def close(): Unit

    // ------------------------------------------------------------------
    // Public API
    // ------------------------------------------------------------------

    /** Run a DBIOAction and return the result in F[R]. */
    final def run[R](a: DBIOAction[R, NoStream, Nothing]): F[R] =
      admissionControl.withInflight {
        connectionArbiter.allocateOrdinal.flatMap { ordinal =>
          asyncF.ref(ExecState.initial(ordinal)).flatMap { ctx =>
            interpret[R](a, ctx)
          }
        }
      }

    /** Stream results of a streaming DBIOAction as an FS2 Stream.
      * Back-pressure is structural — the fiber suspends when the consumer is slow. */
    final def stream[T](a: DBIOAction[?, Streaming[T], Nothing]): Stream[F, T] =
      Stream.bracketCase(admissionControl.inflightAcquire >> connectionArbiter.allocateOrdinal) { (_, _) =>
        admissionControl.inflightRelease
      }.flatMap { ordinal =>
        Stream.eval(asyncF.ref(ExecState.initial(ordinal))).flatMap { ctx =>
          streamInterpret[T](a, ctx)
        }
      }

    /** Number of currently free connection slots managed by the connection arbiter. */
    final def availableConnectionSlots: F[Long] =
      connectionArbiter.available

    /** Number of callers currently waiting in the connection arbiter queue. */
    final def pendingConnectionSlots: F[Int] =
      connectionArbiter.pending

    /** Number of free admission-queue slots. */
    final def availableAdmissionQueueSlots: F[Long] =
      admissionControl.queueAvailable

    /** Number of free inflight slots. */
    final def availableInflightSlots: F[Long] =
      admissionControl.inflightAvailable

    // ------------------------------------------------------------------
    // Interpreter
    // ------------------------------------------------------------------

    /** A `Resource` that acquires one connection-arbiter permit and releases it in its finalizer.
      * The finalizer always runs regardless of how the `use`/`allocated` body completes. */
    private def permitR(ordinal: Long): Resource[F, Unit] =
      Resource.make(connectionArbiter.acquire(ordinal))(_ => connectionArbiter.release)

    /** A `Resource` that opens a new session and closes it (best-effort) in its finalizer.
      * Close is wrapped in `.attempt` so a JDBC exception during close does not suppress
      * the original error or prevent subsequent Resource finalizers from running. */
    private def sessionR: Resource[F, Session] =
      Resource.make(asyncF.blocking(createSession()))(s => asyncF.blocking(s.close()).attempt.void)

    /** A `Resource` that acquires a connection slot *and* opens a session.
      * Finalizers run in reverse acquisition order: session is closed first, then the
      * slot is released — so the slot is always returned to the arbiter even if
      * `session.close()` throws. */
    private def acquiredSessionR(ordinal: Long): Resource[F, Session] = permitR(ordinal).flatMap(_ => sessionR)

    /** Atomically take the `sessionRelease` finalizer out of `ExecState` and run it.
      * The take-and-clear is a single `Ref.modify` so it is idempotent: a second call
      * after the first has already cleared the field is a no-op.  This is the single
      * release path used by `withSession`, `PinnedSessionAction`, and `TransactionalAction`. */
    private def releaseSession(ctx: Ref[F, ExecState]): F[Unit] =
      ctx.modify { s =>
        (s.copy(session = None, sessionRelease = None), s.sessionRelease)
      }.flatMap {
        case Some(rel) => rel.asInstanceOf[F[Unit]]
        case None      => asyncF.unit
      }

    /** Install a freshly-acquired session into `ExecState` and run transaction setup if needed.
      * Returns the updated `ExecState` on success.
      *
      * On error or cancellation the session + permit are released via `releaseAcquired` and
      * `ExecState` is rolled back to remove the session-instance fields that were written during
      * this call (`session`, `sessionRelease`, `previousIsolationLevel`). The transaction-scope
      * fields (`transactionDepth`, `isolationLevel`) are left untouched because they belong to
      * the outer scope, not to this session handoff. */
    private def installSession(
      ctx:             Ref[F, ExecState],
      state:           ExecState,
      session:         Session,
      releaseAcquired: F[Unit]
    ): F[ExecState] = {
      val F = asyncF
      val install: F[Unit] =
        ctx.update(_.copy(
          session        = Some(session),
          sessionRelease = Some(releaseAcquired.asInstanceOf[AnyRef])
        ))
      val setupTx: F[Unit] =
        if (state.inTransaction)
          F.blocking(setupTransaction(session, state.isolationLevel)).flatMap { prev =>
            ctx.update(_.copy(previousIsolationLevel = prev))
          }
        else F.unit

      // On failed handoff: release the resource and roll back only the session-instance
      // fields. Identity-check on the session object guards against clearing a session that
      // was installed by a later successful acquire in the same ctx (shouldn't happen, but
      // is a safe guard against double-clean in unexpected recovery flows).
      F.guaranteeCase(install >> setupTx >> ctx.get) {
        case Outcome.Succeeded(_) =>
          F.unit
        case _ =>
          releaseAcquired.attempt.void >>
          ctx.update { s =>
            s.session match {
              case Some(curr) if curr eq session.asInstanceOf[AnyRef] =>
                s.copy(session = None, sessionRelease = None, previousIsolationLevel = None)
              case _ =>
                s
            }
          }
      }
    }

    /** Enter a transaction scope: run `setupTransaction` on the JDBC connection (if a
      * session is already open and this is the outermost scope), then increment
      * `transactionDepth` and record the isolation level.
      *
      * '''Must be called in an uncancelable region''' (e.g. inside
      * `asyncF.uncancelable` or as the acquire of `Stream.bracketCase`).
      * `setupTransaction` mutates the JDBC connection (`setAutoCommit(false)`,
      * `setTransactionIsolation`) and the subsequent `ctx.update` records that
      * a transaction is active.  If cancellation could land between these two
      * steps, the connection would be left in a transactional state with no
      * matching `exitTransactionScope` to commit/rollback and restore it. */
    private def enterTransactionScope(
      ctx: Ref[F, ExecState],
      isolationLevel: Option[Int]
    ): F[Unit] =
      ctx.get.flatMap { s =>
        val isOutermost = s.transactionDepth == 0
        // Run setupTransaction *before* incrementing transactionDepth so that a
        // JDBC failure (e.g. broken connection, unsupported isolation level) does
        // not leave the depth incremented with no matching decrement.  The callers
        // (interpret / streamInterpret) only register exitTransactionScope as a
        // finalizer *after* enterTransactionScope succeeds, so any state mutation
        // that happens here must be safe to leave behind on failure.
        val setup: F[Option[Int]] =
          if (isOutermost) {
            s.session match {
              case Some(sess) =>
                asyncF.blocking(setupTransaction(sess.asInstanceOf[Session], isolationLevel))
              case None =>
                asyncF.pure(None) // no session yet; setupTransaction called in installSession
            }
          } else asyncF.pure(None)

        setup.flatMap { prevIsolation =>
          ctx.update(s => s.copy(
            transactionDepth       = s.transactionDepth + 1,
            isolationLevel         = if (isOutermost) isolationLevel else s.isolationLevel,
            previousIsolationLevel = if (isOutermost) prevIsolation.orElse(s.previousIsolationLevel) else s.previousIsolationLevel
          ))
        }
      }

    /** Exit a transaction scope: commit or rollback (outermost only), decrement depth,
      * release the session if no longer pinned.  Commit/rollback failure is re-raised
      * *after* decrement and release so cleanup always happens. */
    private def exitTransactionScope(
      ctx: Ref[F, ExecState],
      succeeded: Boolean
    ): F[Unit] =
      ctx.get.flatMap { s =>
        val commitOrRollback: F[Unit] =
          if (s.transactionDepth == 1) {
            s.session match {
              case Some(sess) =>
                if (succeeded)
                  asyncF.blocking(commitTransaction(sess.asInstanceOf[Session], s.previousIsolationLevel))
                else
                  asyncF.blocking(rollbackTransaction(sess.asInstanceOf[Session], s.previousIsolationLevel))
              case None => asyncF.unit
            }
          } else asyncF.unit

        val decrementDepth: F[Unit] =
          ctx.update(s => s.copy(
            transactionDepth       = math.max(0, s.transactionDepth - 1),
            isolationLevel         = if (s.transactionDepth <= 1) None else s.isolationLevel,
            previousIsolationLevel = if (s.transactionDepth <= 1) None else s.previousIsolationLevel
          )) >> (if (s.transactionDepth == 0)
            asyncF.delay(actionLogger.warn("exitTransactionScope called with transactionDepth == 0; this is a bug"))
          else asyncF.unit)

        val releaseIfUnpinned: F[Unit] =
          ctx.get.flatMap(s2 => if (!s2.pinned) releaseSession(ctx) else asyncF.unit)

        commitOrRollback.attempt.flatMap { commitResult =>
          decrementDepth >> releaseIfUnpinned >>
          commitResult.fold(asyncF.raiseError, _ => asyncF.unit)
        }
      }

    /** Acquire a session lazily (only when actually needed by a SynchronousDatabaseAction),
      * run `f` with it (also providing the current ExecState snapshot), and release it
      * unless the session is pinned (transaction or withPinnedSession scope active).
      *
      * Ownership model:
      *   - The acquire + install region is wrapped in `uncancelable` so that cancellation
      *     cannot land between `acquiredSessionR.allocated` returning and `installSession`
      *     writing the finalizer into `ExecState`.  Only the user action `f` runs cancelably
      *     (via `poll`).
      *   - `releaseAcquired` is stored in `ExecState.sessionRelease` so that whichever scope
      *     ends last (this call for unpinned, an outer pin/tx for pinned) can invoke it via
      *     `releaseSession(ctx)`.
      *   - If `installSession` fails or is canceled it rolls back `ExecState` and calls
      *     `releaseAcquired` itself so neither the session nor the permit leaks.
      *   - `releaseSession` is idempotent (atomic take-and-clear), so there is no
      *     double-close or double-release regardless of which path runs. */
    private def withSession[R](
      ctx: Ref[F, ExecState]
    )(f: (Session, ExecState) => F[R]): F[R] = {
      val F = asyncF
      ctx.get.flatMap { state =>
        state.session match {
          case Some(session) =>
            // Connection already acquired (pinned or in-transaction) — reuse it
            f(session.asInstanceOf[Session], state)

          case None =>
            // Acquire + install are uncancelable so that a cancellation between
            // `allocated` returning and `installSession` completing cannot orphan the
            // resource.  User work runs cancelably via poll.
            F.uncancelable { poll =>
              acquiredSessionR(state.ordinal).allocated.flatMap { case (session, releaseAcquired) =>
                installSession(ctx, state, session, releaseAcquired).flatMap { s1 =>
                  poll(f(session, s1)).guarantee {
                    ctx.get.flatMap { s2 =>
                      if (!s2.pinned) releaseSession(ctx) else F.unit
                    }
                  }
                }
              }
            }
        }
      }
    }

    /** Set up a transaction on a freshly-acquired connection.
      * Returns the previous isolation level (to be restored after the transaction ends),
      * or None if the isolation level was not changed. Override in JdbcBackend. */
    protected def setupTransaction(session: Session, isolationLevel: Option[Int]): Option[Int] = None

    /** Commit the transaction on the session, restoring the given isolation level if provided.
      * Override in JdbcBackend. */
    protected def commitTransaction(session: Session, previousIsolationLevel: Option[Int]): Unit = ()

    /** Rollback the transaction on the session, restoring the given isolation level if provided.
      * Override in JdbcBackend. */
    protected def rollbackTransaction(session: Session, previousIsolationLevel: Option[Int]): Unit = ()

    /** The core recursive interpreter for `DBIOAction` values.
      *
      * - `SynchronousDatabaseAction` steps run in `F.blocking` on the CE3 blocking pool.
      * - No explicit stack-level tracking: CE3 `flatMap` is stack-safe.
      * - Execution state (session, transaction depth, pinning) is tracked in a `Ref[F, ExecState]`.
      * - Cancellation triggers rollback via `guaranteeCase`.
      */
    protected def interpret[R](
      a: DBIOAction[R, NoStream, Nothing],
      ctx: Ref[F, ExecState]
    ): F[R] = {
      val F = asyncF
      logAction(a)
      // Wrap in F.defer so that recursive calls to interpret do not consume Scala stack
      // frames. Without this, deeply nested FlatMapAction / AndThenAction structures
      // (e.g. 10,000+ levels) would cause a StackOverflowError.  CE3's defer pushes the
      // continuation onto the run-loop instead of building Scala frames.
      F.defer {
      a match {
        case SuccessAction(v) =>
          F.pure(v)

        case FailureAction(t) =>
          F.raiseError(t)

        case LiftFAction(fa) =>
          // fa is already an F[R]; type safety guaranteed by DBIO.liftF
          fa.asInstanceOf[F[R]]

        case FlatMapAction(base, f) =>
          interpret[Any](base.asInstanceOf[DBIOAction[Any, NoStream, Nothing]], ctx)
            .flatMap { v =>
              val next = f.asInstanceOf[Any => DBIOAction[R, NoStream, Nothing]](v)
              interpret[R](next, ctx)
            }

        case AndThenAction(actions) =>
          val last = actions.length - 1
          def run(pos: Int): F[Any] = {
            val fi = interpret[Any](actions(pos).asInstanceOf[DBIOAction[Any, NoStream, Nothing]], ctx)
            if (pos == last) fi
            else fi.flatMap(_ => run(pos + 1))
          }
          run(0).asInstanceOf[F[R]]

        case sa @ SequenceAction(actions) =>
          val len = actions.length
          def run(pos: Int, acc: Vector[Any]): F[Vector[Any]] = {
            if (pos == len) F.pure(acc)
            else
              interpret[Any](actions(pos).asInstanceOf[DBIOAction[Any, NoStream, Nothing]], ctx)
                .flatMap(v => run(pos + 1, acc :+ v))
          }
          run(0, Vector.empty).map { results =>
            val b = sa.cbf.asInstanceOf[Factory[Any, R]].newBuilder
            results.foreach(b += _)
            b.result()
          }

        case CleanUpAction(base, f, keepFailure) =>
          // Use uncancelable+poll so that:
          //   - the base action runs cancelably (via poll),
          //   - the cleanup action f(...) always runs, even on cancellation,
          //   - after cleanup, the original outcome is restored.
          // On cancellation, f receives Some(CancellationException) so user code can
          // distinguish cancel from normal errors.
          F.uncancelable { poll =>
            poll(interpret[R](base.asInstanceOf[DBIOAction[R, NoStream, Nothing]], ctx))
              .guaranteeCase {
                case Outcome.Succeeded(_) =>
                  interpret[Any](f(None).asInstanceOf[DBIOAction[Any, NoStream, Nothing]], ctx).void
                case Outcome.Errored(err) =>
                  // Run cleanup with the error. Re-raise original unless keepFailure=false
                  // and cleanup itself also fails (cleanup error takes precedence).
                  interpret[Any](f(Some(err)).asInstanceOf[DBIOAction[Any, NoStream, Nothing]], ctx).void
                    .recoverWith { case cleanupErr if !keepFailure => F.raiseError(cleanupErr) }
                case Outcome.Canceled() =>
                  // Run cleanup with a CancellationException so user code can react to cancel.
                  // Errors from cleanup are swallowed so as not to mask the cancellation.
                  interpret[Any](
                    f(Some(new java.util.concurrent.CancellationException("DBIO action canceled")))
                      .asInstanceOf[DBIOAction[Any, NoStream, Nothing]], ctx).attempt.void
              }
          }

        case FailedAction(inner) =>
          interpret[Any](inner.asInstanceOf[DBIOAction[Any, NoStream, Nothing]], ctx)
            .attempt
            .flatMap {
              case Left(t)  => F.pure(t.asInstanceOf[R])
              case Right(_) => F.raiseError(new NoSuchElementException("Action.failed did not fail"))
            }

        case AsTryAction(inner) =>
          interpret[Any](inner.asInstanceOf[DBIOAction[Any, NoStream, Nothing]], ctx)
            .attempt
            .map {
              case Right(v) => scala.util.Success(v).asInstanceOf[R]
              case Left(t)  => scala.util.Failure(t).asInstanceOf[R]
            }

        case NamedAction(inner, _) =>
          interpret[R](inner.asInstanceOf[DBIOAction[R, NoStream, Nothing]], ctx)

        case TransactionalAction(inner, isolationLevel) =>
          // uncancelable ensures enterTransactionScope and guaranteeCase registration are
          // atomic: a cancellation between them would leave transactionDepth incremented
          // with no finalizer to decrement it.  The inner action runs cancelably via poll.
          asyncF.uncancelable { poll =>
            enterTransactionScope(ctx, isolationLevel) >>
            poll(interpret[R](inner.asInstanceOf[DBIOAction[R, NoStream, Nothing]], ctx))
              .guaranteeCase { outcome =>
                exitTransactionScope(ctx, succeeded = outcome.isSuccess)
              }
          }

        case PinnedSessionAction(inner) =>
          // uncancelable pairs the increment and the guarantee registration atomically,
          // mirroring the TransactionalAction pattern.  Without this, cancellation between
          // the increment and the guarantee could leave pinnedDepth permanently > 0,
          // preventing releaseSession from ever running.  Inner action runs cancelably via poll.
          asyncF.uncancelable { poll =>
            ctx.update(s => s.copy(pinnedDepth = s.pinnedDepth + 1)) >>
            poll(interpret[R](inner.asInstanceOf[DBIOAction[R, NoStream, Nothing]], ctx))
              .guarantee {
                ctx.update(s => s.copy(pinnedDepth = s.pinnedDepth - 1)) >>
                ctx.get.flatMap { s =>
                  // Only release the connection if there are no more pins or transactions
                  if (!s.pinned) releaseSession(ctx) else asyncF.unit
                }
              }
          }

        case a: SynchronousDatabaseAction[?, ?, ?, ?, ?] =>
          withSession[R](ctx) { (session, state) =>
            F.blocking {
              a.asInstanceOf[SynchronousDatabaseAction[R, NoStream, Context, StreamingContext, Nothing]]
               .run(sessionAsContext(session, state))
            }
          }

        case a: DBIOAction[?, ?, ?] =>
          F.raiseError(new SlickException(s"Unsupported database action $a for $this"))
      }
      } // end F.defer
    }

    /** Like `withSession` but for streaming: acquires/reuses a session and passes it to `f`
      * which returns a `Stream[F, T]`. The session is released (unless pinned) when the
      * stream finishes, errors, or is cancelled.
      *
      * Uses `Stream.bracketCase` for the acquire-new-session path so that release is
      * registered structurally at acquisition time, closing the cancellation windows that
      * existed when staged `flatMap` + `onFinalize` were used:
      *   - the gap between `acquiredSessionR.allocated` returning and `installSession`
      *     completing (A3), and
      *   - the gap between `installSession` completing and `onFinalize` being attached to
      *     the downstream stream (A4). */
    private def withSessionStream[T](
      ctx: Ref[F, ExecState]
    )(f: (Session, ExecState) => Stream[F, T]): Stream[F, T] = {
      val F = asyncF
      Stream.eval(ctx.get).flatMap { state =>
        state.session match {
          case Some(session) =>
            // Reuse existing pinned/transactional session
            f(session.asInstanceOf[Session], state)

          case None =>
            // bracketCase: acquire = allocated + install; release = releaseSession if unpinned.
            // Both acquire and release are uncancelable by bracketCase semantics, so no gap
            // can exist between the two.
            Stream.bracketCase(
              acquire = acquiredSessionR(state.ordinal).allocated.flatMap { case (session, releaseAcquired) =>
                installSession(ctx, state, session, releaseAcquired).map(s1 => (session, s1))
              }
            )(
              release = (_, _) =>
                ctx.get.flatMap { s =>
                  if (!s.pinned) releaseSession(ctx) else F.unit
                }
            ).flatMap { case (session, updatedState) =>
              f(session, updatedState)
            }
        }
      }
    }

    protected def streamInterpret[T](
      a: DBIOAction[?, Streaming[T], Nothing],
      ctx: Ref[F, ExecState]
    ): Stream[F, T] = {
      val F = asyncF
      a match {
        case sa: SynchronousDatabaseAction[?, ?, ?, ?, ?]
            if sa.supportsStreaming =>
          withSessionStream[T](ctx) { (session, state) =>
            val sda = sa.asInstanceOf[SynchronousDatabaseAction[?, Streaming[T], Context, StreamingContext, Nothing]]
            streamFromSDA[T](sda, session, state)
          }

        case sa: SynchronousDatabaseAction[?, ?, ?, ?, ?] =>
          // FusedAndThenAction (supportsStreaming = false): unfuse and recurse so that prefix
          // actions run through the interpreter and the final streaming action is streamed.
          streamInterpret[T](
            sa.nonFusedEquivalentAction.asInstanceOf[DBIOAction[?, Streaming[T], Nothing]], ctx)

        case AndThenAction(actions) =>
          // Run all prefix actions through the interpreter, then stream the last one.
          val prefix = actions.init.asInstanceOf[IndexedSeq[DBIOAction[Any, NoStream, Nothing]]]
          val last   = actions.last.asInstanceOf[DBIOAction[?, Streaming[T], Nothing]]
          if (prefix.isEmpty) streamInterpret[T](last, ctx)
          else
            Stream.eval(prefix.foldLeft(F.unit)((acc, act) =>
              acc >> interpret[Any](act, ctx).void
            )) >> streamInterpret[T](last, ctx)

        case PinnedSessionAction(inner) =>
          // bracketCase pairs the increment and decrement structurally, mirroring the
          // TransactionalAction stream path.  Without this, cancellation between the
          // Stream.eval(increment) and onFinalize attachment could leave pinnedDepth
          // permanently > 0, preventing releaseSession from ever running.
          Stream.bracketCase(
            acquire = ctx.update(s => s.copy(pinnedDepth = s.pinnedDepth + 1))
          ) { (_, _) =>
            ctx.update(s => s.copy(pinnedDepth = s.pinnedDepth - 1)) >>
            ctx.get.flatMap { s =>
              if (!s.pinned) releaseSession(ctx) else F.unit
            }
          } >> streamInterpret[T](inner.asInstanceOf[DBIOAction[?, Streaming[T], Nothing]], ctx)

        case TransactionalAction(inner, isolationLevel) =>
          // bracketCase pairs enterTransactionScope and exitTransactionScope structurally,
          // closing the cancellation race window where depth was incremented but the
          // finalizer was not yet registered.
          Stream.bracketCase(enterTransactionScope(ctx, isolationLevel)) { (_, exitCase) =>
            exitTransactionScope(ctx, succeeded = exitCase == Resource.ExitCase.Succeeded)
          } >> streamInterpret[T](inner.asInstanceOf[DBIOAction[?, Streaming[T], Nothing]], ctx)

        case FlatMapAction(base, f) =>
          // Run the (non-streaming) base through the interpreter, then recurse into
          // streamInterpret for the produced streaming action.
          Stream.eval(
            interpret[Any](base.asInstanceOf[DBIOAction[Any, NoStream, Nothing]], ctx)
          ).flatMap { v =>
            val next = f.asInstanceOf[Any => DBIOAction[?, Streaming[T], Nothing]](v)
            streamInterpret[T](next, ctx)
          }

        case NamedAction(inner, _) =>
          streamInterpret[T](inner.asInstanceOf[DBIOAction[?, Streaming[T], Nothing]], ctx)

        case _ =>
          // Non-SDA streaming action — fall back to running through interpreter and collecting.
          // This path should not be reached for any first-class action type; it exists as a
          // safety net for third-party DBIOAction subclasses that are not natively streamable.
          Stream.eval(interpret[Seq[T]](
            a.asInstanceOf[DBIOAction[Seq[T], NoStream, Nothing]], ctx
          )).flatMap(seq => Stream.emits(seq))
      }
    }

    /** Build an FS2 Stream from a SynchronousDatabaseAction.
      * Must be implemented by each backend that supports streaming. */
    protected def streamFromSDA[T](
      a: SynchronousDatabaseAction[?, Streaming[T], Context, StreamingContext, Nothing],
      session: Session,
      state: ExecState
    ): Stream[F, T]

    /** Wrap a Session and ExecState as a Context for passing to SynchronousDatabaseAction.run. */
    protected def sessionAsContext(session: Session, state: ExecState): Context

    // ------------------------------------------------------------------
    // Logging
    // ------------------------------------------------------------------

    protected[this] def logAction(a: DBIOAction[?, NoStream, Nothing]): Unit = {
      if (actionLogger.isDebugEnabled && a.isLogged) {
        val logA = a.nonFusedEquivalentAction
        val aPrefix = if (a eq logA) "" else "[fused] "
        val dump = new TreePrinter(prefix = "    ", firstPrefix = aPrefix, narrow = {
          case a: DBIOAction[?, ?, ?] => a.nonFusedEquivalentAction
          case o                      => o
        }).get(logA)
        val msg = DumpInfo.highlight(dump.substring(0, dump.length - 1))
        actionLogger.debug(msg)
      }
    }
  }

  // -----------------------------------------------------------------------
  // Session definition
  // -----------------------------------------------------------------------

  /** A logical session of a `Database`. The underlying database connection is created lazily on demand. */
  trait BasicSessionDef extends Closeable {
    /** Close this Session. */
    def close(): Unit

    /** Force an actual database session to be opened. Slick sessions are lazy, so you do not
      * get a real database connection until you need it or you call force() on the session. */
    def force(): Unit
  }

  // -----------------------------------------------------------------------
  // Action context (used by SynchronousDatabaseAction.run)
  // -----------------------------------------------------------------------

  /** The context object passed to `SynchronousDatabaseAction` instances by the execution engine.
    * The heavy concurrency state lives in [[ExecState]] / `Ref`; this is a thin wrapper that
    * gives SDAs access to the session and statement parameters. */
  trait BasicActionContext {
    def session: Session
    /** Current transaction nesting depth (0 = no transaction, 1 = one level, etc.) */
    def transactionDepth: Int
    /** Whether the session is currently pinned (from ExecState.pinned). */
    def isPinned: Boolean
  }

  /** A special BasicActionContext for streaming SynchronousDatabaseActions. */
  trait BasicStreamingActionContext extends BasicActionContext with StreamingActionContext
}
