package slick.basic

import java.io.Closeable
import java.util.concurrent.atomic.{AtomicLong, AtomicReferenceArray}

import scala.collection.compat.*
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}
import scala.util.control.NonFatal

import slick.SlickException
import slick.dbio.*
import slick.util.*
import slick.util.AsyncExecutor.{Continuation, Fresh, Priority, WithConnection}

import com.typesafe.config.Config
import org.reactivestreams.*
import org.slf4j.LoggerFactory

/** Backend for the basic database and session handling features.
  * Concrete backends like `JdbcBackend` extend this type and provide concrete
  * types for `Database`, `DatabaseFactory` and `Session`. */
trait BasicBackend { self =>
  protected lazy val actionLogger = new SlickLogger(LoggerFactory.getLogger(classOf[BasicBackend].getName+".action"))
  protected lazy val streamLogger = new SlickLogger(LoggerFactory.getLogger(classOf[BasicBackend].getName+".stream"))

  /** The type of database objects used by this backend. */
  type Database >: Null <: BasicDatabaseDef
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
    * @param path The path in the configuration file for the database configuration, or an empty
    *             string for the top level of the `Config` object.
    * @param config The `Config` object to read from.
    */
  def createDatabase(config: Config, path: String): Database

  /** A database instance to which connections can be created. */
  trait BasicDatabaseDef extends Closeable { this: Database =>
    /** Create a new session. The session needs to be closed explicitly by calling its close() method. */
    def createSession(): Session

    /** Free all resources allocated by Slick for this Database. This is done asynchronously, so
      * you need to wait for the returned `Future` to complete in order to ensure that everything
      * has been shut down. */
    def shutdown: Future[Unit] = Future(close())(ExecutionContext.fromExecutor(AsyncExecutor.shutdownExecutor))

    /** Free all resources allocated by Slick for this Database, blocking the current thread until
      * everything has been shut down.
      *
      * Backend implementations which are based on a naturally blocking shutdown procedure can
      * simply implement this method and get `shutdown` as an asynchronous wrapper for free. If
      * the underlying shutdown procedure is asynchronous, you should implement `shutdown` instead
      * and wrap it with `Await.result` in this method. */
    override def close(): Unit

    protected def prioritizedRunnable(priority: => Priority,
                                      run: AsyncExecutor.PrioritizedRunnable.SetConnectionReleased => Unit
                                     ): AsyncExecutor.PrioritizedRunnable =
      AsyncExecutor.PrioritizedRunnable(priority, run)

    /** Run an Action asynchronously and return the result as a Future. */
    final def run[R](a: DBIOAction[R, NoStream, Nothing]): Future[R] = runInternal(a, useSameThread = false)

    private[slick] final def runInternal[R](a: DBIOAction[R, NoStream, Nothing], useSameThread: Boolean): Future[R] =
      try runInContext(a, createDatabaseActionContext(useSameThread), streaming = false, topLevel = true)
      catch { case NonFatal(ex) => Future.failed(ex) }

    /** Create a `Publisher` for Reactive Streams which, when subscribed to, will run the specified
      * `DBIOAction` and return the result directly as a stream without buffering everything first.
      * This method is only supported for streaming actions.
      *
      * The Publisher itself is just a stub that holds a reference to the action and this Database.
      * The action does not actually start to run until the call to `onSubscribe` returns, after
      * which the Subscriber is responsible for reading the full response or cancelling the
      * Subscription. The created Publisher can be reused to serve a multiple Subscribers,
      * each time triggering a new execution of the action.
      *
      * For the purpose of combinators such as `cleanup` which can run after a stream has been
      * produced, cancellation of a stream by the Subscriber is not considered an error. For
      * example, there is no way for the Subscriber to cause a rollback when streaming the
      * results of `someQuery.result.transactionally`.
      *
      * When using a JDBC back-end, all `onNext` calls are done synchronously and the ResultSet row
      * is not advanced before `onNext` returns. This allows the Subscriber to access LOB pointers
      * from within `onNext`. If streaming is interrupted due to back-pressure signaling, the next
      * row will be prefetched (in order to buffer the next result page from the server when a page
      * boundary has been reached). */
    final def stream[T](a: DBIOAction[?, Streaming[T], Nothing]): DatabasePublisher[T] =
      streamInternal(a, useSameThread = false)

    private[slick] final def streamInternal[T](a: DBIOAction[?, Streaming[T], Nothing],
                                               useSameThread: Boolean): DatabasePublisher[T] =
      createPublisher(a, s => createStreamingDatabaseActionContext(s, useSameThread))

    /** Create a Reactive Streams `Publisher` using the given context factory. */
    protected[this] def createPublisher[T](a: DBIOAction[?, Streaming[T], Nothing],
                                           createCtx: Subscriber[? >: T] => StreamingContext): DatabasePublisher[T] =
    { (s: Subscriber[? >: T]) =>
      if (s eq null) throw new NullPointerException("Subscriber is null")
      val ctx = createCtx(s)
      if (streamLogger.isDebugEnabled) streamLogger.debug(s"Signaling onSubscribe($ctx)")
      val subscribed = try {
        s.onSubscribe(ctx.subscription)
        true
      } catch {
        case NonFatal(ex) =>
          streamLogger.warn("Subscriber.onSubscribe failed unexpectedly", ex)
          false
      }
      if (subscribed) {
        try {
          runInContext(a, ctx, streaming = true, topLevel = true).onComplete {
            case Success(_) => ctx.tryOnComplete
            case Failure(t) => ctx.tryOnError(t)
          }(DBIO.sameThreadExecutionContext)
        } catch {
          case NonFatal(ex) => ctx.tryOnError(ex)
        }
      }
    }

    /** Create the default DatabaseActionContext for this backend. */
    protected[this] def createDatabaseActionContext[T](_useSameThread: Boolean): Context

    /** Create the default StreamingDatabaseActionContext for this backend. */
    protected[this] def createStreamingDatabaseActionContext[T](s: Subscriber[? >: T],
                                                                useSameThread: Boolean): StreamingContext

    /** Run an Action in an existing DatabaseActionContext. This method can be overridden in
      * subclasses to support new DatabaseActions which cannot be expressed through
      * SynchronousDatabaseAction.
      *
      * @param streaming Whether to return the result as a stream. In this case, the context must
      *                  be a `StreamingDatabaseActionContext` and the Future result should be
      *                  completed with `null` or failed after streaming has finished. This
      *                  method should not call any `Subscriber` method other than `onNext`. */
    protected[this] def runInContext[R](a: DBIOAction[R, NoStream, Nothing],
                                        ctx: Context,
                                        streaming: Boolean,
                                        topLevel: Boolean): Future[R] =
      runInContextSafe(a, ctx, streaming, topLevel, stackLevel = 0)

    // If recursion has reached the limit then run next batch in trampoline
    private[this] def runInContextSafe[R](a: DBIOAction[R, NoStream, Nothing],
                                          ctx: Context,
                                          streaming: Boolean,
                                          topLevel: Boolean,
                                          stackLevel: Int): Future[R] =
      if (stackLevel < 100)
        runInContextInline(a, ctx, streaming, topLevel, stackLevel + 1)
      else {
        val promise = Promise[R]()
        val runnable = new Runnable {
          override def run() = {
            try {
              promise.completeWith(runInContextInline(a, ctx, streaming, topLevel, stackLevel = 1))
            } catch {
              case NonFatal(ex) => promise.failure(ex)
            }
          }
        }
        DBIO.sameThreadExecutionContext.execute(runnable)
        promise.future
      }

    private[this] def runInContextInline[R](a: DBIOAction[R, NoStream, Nothing],
                                            ctx: Context,
                                            streaming: Boolean,
                                            topLevel: Boolean,
                                            stackLevel: Int): Future[R] = {
      logAction(a, ctx)
      a match {
        case SuccessAction(v) => Future.successful(v)
        case FailureAction(t) => Future.failed(t)
        case FutureAction(f) => f
        case FlatMapAction(base, f, ec) =>
          runInContextSafe(base, ctx, streaming = false, topLevel = topLevel, stackLevel)
            .flatMap(v => runInContext(f(v), ctx, streaming, topLevel = false))(ctx.getEC(ec))
        case AndThenAction(actions) =>
          val last = actions.length - 1

          def run(pos: Int, v: Any): Future[Any] = {
            val f1 = runInContextSafe(actions(pos), ctx, streaming && pos == last, topLevel && pos == 0, stackLevel)

            if(pos == last) f1
            else f1.flatMap(run(pos + 1, _))(DBIO.sameThreadExecutionContext)
          }

          run(0, null).asInstanceOf[Future[R]]
        case sa@SequenceAction(actions) =>
          val len = actions.length
          val results = new AtomicReferenceArray[Any](len)

          def run(pos: Int): Future[Any] = {
            if (pos == len) Future.successful {
              val b = sa.cbf.asInstanceOf[Factory[Any, R]].newBuilder
              var i = 0
              while (i < len) {
                b += results.get(i)
                i += 1
              }
              b.result()
            }
            else
              runInContextSafe(actions(pos), ctx, streaming = false, topLevel = topLevel && pos == 0, stackLevel)
                .flatMap { (v: Any) =>
                  results.set(pos, v)
                  run(pos + 1)
                }(DBIO.sameThreadExecutionContext)
          }

          run(0).asInstanceOf[Future[R]]
        case CleanUpAction(base, f, keepFailure, ec) =>
          val p = Promise[R]()
          runInContextSafe(base, ctx, streaming, topLevel, stackLevel).onComplete { t1 =>
            try {
              val a2 = f(t1 match {
                case Success(_) => None
                case Failure(t) => Some(t)
              })
              runInContext(a2, ctx, streaming = false, topLevel = false).onComplete { t2 =>
                if (t2.isFailure && (t1.isSuccess || !keepFailure)) p.complete(t2.asInstanceOf[Failure[R]])
                else p.complete(t1)
              }(DBIO.sameThreadExecutionContext)
            } catch {
              case NonFatal(ex) =>
                val e = t1 match {
                  case Failure(t) if keepFailure => t
                  case _ => ex
                }

                if (!p.tryFailure(e)) {
                  actionLogger.warn("Exception after promise completed", e)
                }
            }
          }(ctx.getEC(ec))
          p.future
        case FailedAction(a) =>
          runInContextSafe(a, ctx, streaming = false, topLevel = topLevel, stackLevel).failed.asInstanceOf[Future[R]]
        case AsTryAction(a) =>
          val p = Promise[R]()
          runInContextSafe(a, ctx, streaming = false, topLevel = topLevel, stackLevel)
            .onComplete(v => p.success(v.asInstanceOf[R]))(DBIO.sameThreadExecutionContext)
          p.future
        case NamedAction(a, _) =>
          runInContextSafe(a, ctx, streaming, topLevel, stackLevel)
        case a: SynchronousDatabaseAction[?, ?, ?, ?, ?] =>
          if (streaming) {
            if (a.supportsStreaming)
              streamSynchronousDatabaseAction(
                a.asInstanceOf[SynchronousDatabaseAction[?, ? <: NoStream, Context, StreamingContext, ? <: Effect]],
                ctx.asInstanceOf[StreamingContext],
                !topLevel
              ).asInstanceOf[Future[R]]
            else
              runInContextSafe(
                CleanUpAction(
                  AndThenAction(Vector(DBIO.Pin, a.nonFusedEquivalentAction)),
                  _ => DBIO.Unpin,
                  keepFailure = true,
                  DBIO.sameThreadExecutionContext),
                ctx,
                streaming,
                topLevel,
                stackLevel)
          }
          else
            runSynchronousDatabaseAction(
              a.asInstanceOf[SynchronousDatabaseAction[R, NoStream, Context, StreamingContext, ?]],
              ctx,
              !topLevel
            )
        case a: DatabaseAction[?, ?, ?]               =>
          throw new SlickException(s"Unsupported database action $a for $this")
      }
    }

    /** Within a synchronous execution, ensure that a Session is available. */
    protected[this] final def acquireSession(ctx: Context): Unit =
      if(!ctx.isPinned) ctx.currentSession = createSession()

    /** Within a synchronous execution, close the current Session unless it is pinned.
     *
     * @param handleCloseError What to do with non-fatal errors that arise while closing the Session. */
    protected[this] final def releaseSession(ctx: Context,
                                             handleCloseError: PartialFunction[Throwable, Unit] = PartialFunction.empty
                                            ): Unit =
      if (!ctx.isPinned) {
        val session = ctx.currentSession
        ctx.currentSession = null
        if (session != null)
          try session.close() catch handleCloseError
      }

    /** Run a `SynchronousDatabaseAction` on this database. */
    protected[this] def runSynchronousDatabaseAction[R](a: SynchronousDatabaseAction[R, NoStream, Context, StreamingContext, ?],
                                                        ctx: Context,
                                                        continuation: Boolean): Future[R] = {
      val promise = Promise[R]()
      ctx.getEC(synchronousExecutionContext).execute(prioritizedRunnable(
        priority = {
          ctx.readSync
          ctx.priority(continuation)
        },
        run = { setConnectionReleased =>
          try {
            ctx.readSync
            val res = try {
              acquireSession(ctx)
              val res = try a.run(ctx) catch { case NonFatal(ex) =>
                releaseSession(ctx, { case NonFatal(ex) => actionLogger.warn("Exception while closing session", ex) })
                throw ex
              }
              releaseSession(ctx)
              res
            } finally {
              if (!ctx.isPinned && ctx.priority(continuation) != WithConnection) setConnectionReleased()
              ctx.sync = 0
            }
            promise.success(res)
          } catch { case NonFatal(ex) => promise.tryFailure(ex) }
        }
      ))
      promise.future
    }

    /** Stream a `SynchronousDatabaseAction` on this database. */
    protected[this]
    def streamSynchronousDatabaseAction(a: SynchronousDatabaseAction[?, ? <: NoStream, Context, StreamingContext, ? <: Effect],
                                        ctx: StreamingContext,
                                        continuation: Boolean): Future[Null] = {
      ctx.streamingAction = a
      scheduleSynchronousStreaming(a, ctx, continuation)(null)
      ctx.streamingResultPromise.future
    }

    /** Stream a part of the results of a `SynchronousDatabaseAction` on this database. */
    protected[BasicBackend]
    def scheduleSynchronousStreaming(a: SynchronousDatabaseAction[?, ? <: NoStream, Context, StreamingContext, ? <: Effect],
                                     ctx: StreamingContext,
                                     continuation: Boolean)
                                    (initialState: a.StreamState): Unit =
      try {
        def str(l: Long) = if(l != Long.MaxValue) l else if(GlobalConfig.unicodeDump) "\u221E" else "oo"
        ctx.getEC(synchronousExecutionContext).execute(prioritizedRunnable (
          priority = {
            ctx.readSync
            ctx.priority(continuation)
          },
          run = { setConnectionReleased =>
            try {
              val debug = streamLogger.isDebugEnabled
              var state = initialState
              ctx.readSync

              if(state eq null) {
                try {
                  acquireSession(ctx)
                } catch { case NonFatal(ex) =>
                  if (!ctx.isPinned) setConnectionReleased()
                  throw ex
                }
              }
              var demand = ctx.demandBatch
              var realDemand = if(demand < 0) demand - Long.MinValue else demand

              while({
                try {
                  if(debug)
                    streamLogger.debug(
                      (if(state eq null) "Starting initial" else "Restarting ") +
                        " streaming action, realDemand = " +
                        str(realDemand)
                    )

                  if(ctx.cancelled) {
                    if(ctx.deferredError ne null) throw ctx.deferredError
                    if(state ne null) { // streaming cancelled before finishing
                      val oldState = state
                      state = null
                      a.cancelStream(ctx, oldState)
                    }
                  } else if(realDemand > 0 || (state eq null)) {
                    val oldState = state
                    state = null
                    state = a.emitStream(ctx, realDemand, oldState)
                  }

                  if(state eq null) { // streaming finished and cleaned up
                    releaseSession(
                      ctx = ctx,
                      handleCloseError = {
                        case NonFatal(ex) => streamLogger.warn("Exception while closing session", ex)
                      }
                    )
                    if(!ctx.isPinned) setConnectionReleased()
                    ctx.sync = 0
                    ctx.streamingResultPromise.trySuccess(null)
                  }

                } catch { case NonFatal(ex) =>
                  if(state ne null) try a.cancelStream(ctx, state) catch ignoreFollowOnError
                  releaseSession(ctx, { case NonFatal(ex) => streamLogger.warn("Exception while closing session", ex) })
                  if (!ctx.isPinned) setConnectionReleased()
                  ctx.sync = 0
                  throw ex

                } finally {
                  ctx.streamState = state
                  ctx.sync = 0
                }

                if(debug) {
                  if(state eq null)
                    streamLogger.debug(
                      s"Sent up to ${str(realDemand)} elements - Stream " +
                        (if(ctx.cancelled) "cancelled" else "completely delivered")
                    )
                  else
                    streamLogger.debug(
                      s"Sent ${str(realDemand)} elements, more available - Performing atomic state transition"
                    )
                }

                demand = ctx.delivered(demand)
                realDemand = if(demand < 0) demand - Long.MinValue else demand

                ((state ne null) && realDemand > 0)
              }) ()

              if(debug) {
                if(state ne null)
                  streamLogger.debug("Suspending streaming action with continuation (more data available)")
                else
                  streamLogger.debug("Finished streaming action")
              }

            } catch {
              case NonFatal(ex) => ctx.streamingResultPromise.tryFailure(ex)
            }
          }
        ))
      } catch { case NonFatal(ex) =>
        streamLogger.warn("Error scheduling synchronous streaming", ex)
        throw ex
      }



    /** Return the default ExecutionContext for this Database which should be used for running
      * SynchronousDatabaseActions for asynchronous execution. */
    protected[this] def synchronousExecutionContext: ExecutionContext

    protected[this] def logAction(a: DBIOAction[?, NoStream, Nothing], ctx: Context): Unit = {
      if(actionLogger.isDebugEnabled && a.isLogged) {
        ctx.sequenceCounter += 1
        val logA = a.nonFusedEquivalentAction
        val aPrefix = if(a eq logA) "" else "[fused] "
        val dump = new TreePrinter(prefix = "    ", firstPrefix = aPrefix, narrow = {
          case a: DBIOAction[?, ?, ?] => a.nonFusedEquivalentAction
          case o                      => o
        }).get(logA)
        val msg = DumpInfo.highlight("#" + ctx.sequenceCounter) + ": " + dump.substring(0, dump.length-1)
        actionLogger.debug(msg)
      }
    }
  }

  /** A logical session of a `Database`. The underlying database connection is created lazily on demand. */
  trait BasicSessionDef extends Closeable {
    /** Close this Session. */
    def close(): Unit

    /** Force an actual database session to be opened. Slick sessions are lazy, so you do not
      * get a real database connection until you need it or you call force() on the session. */
    def force(): Unit
  }

  /** The context object passed to database actions by the execution engine. */
  trait BasicActionContext extends ActionContext {
    /** Whether to run all operations on the current thread or schedule them normally on the
      * appropriate ExecutionContext. This is used by the blocking API. */
    protected[BasicBackend] val useSameThread: Boolean

    /** Return the specified ExecutionContext unless running in same-thread mode, in which case
      * `Action.sameThreadExecutionContext` is returned instead. */
    private[BasicBackend] def getEC(ec: ExecutionContext): ExecutionContext =
      if(useSameThread) DBIO.sameThreadExecutionContext else ec

    /** A volatile variable to enforce the happens-before relationship (see
      * [[https://docs.oracle.com/javase/specs/jls/se7/html/jls-17.html]] and
      * [[http://gee.cs.oswego.edu/dl/jmm/cookbook.html]]) when executing something in
      * a synchronous action context. It is read when entering the context and written when leaving
      * so that all writes to non-volatile variables within the context are visible to the next
      * synchronous execution. */
    @volatile private[BasicBackend] var sync = 0

    private[BasicBackend] def readSync = sync // workaround for SI-9053 to avoid warnings

    private[BasicBackend] var currentSession: Session = null

    private[BasicBackend] def priority(continuation: Boolean): Priority = {
      if (currentSession != null) WithConnection
      else if (continuation) Continuation
      else Fresh
    }

    /** Used for the sequence counter in Action debug output. This variable is volatile because it
      * is only updated sequentially but not protected by a synchronous action context. */
    @volatile private[BasicBackend] var sequenceCounter = 0

    def session: Session = currentSession
  }

  /** A special DatabaseActionContext for streaming execution. */
  class BasicStreamingActionContext(subscriber: Subscriber[?],
                                                    override protected[BasicBackend] val useSameThread: Boolean,
                                                    database: Database)
    extends BasicActionContext with StreamingActionContext with Subscription {
    /** Whether the Subscriber has been signaled with `onComplete` or `onError`. */
    private[this] var finished = false

    /** The total number of elements requested and not yet marked as delivered by the synchronous
      * streaming action. Whenever this value drops to 0, streaming is suspended. When it is raised
      * up from 0 in `request`, streaming is scheduled to be restarted. It is initially set to
      * `Long.MinValue` when streaming starts. Any negative value above `Long.MinValue` indicates
      * the actual demand at that point. It is reset to 0 when the initial streaming ends. */
    private[this] val remaining = new AtomicLong(Long.MinValue)

    /** An error that will be signaled to the Subscriber when the stream is cancelled or
      * terminated. This is used for signaling demand overflow in `request()` while guaranteeing
      * that the `onError` message does not overlap with an active `onNext` call. */
    private[BasicBackend] var deferredError: Throwable = null

    /** The state for a suspended streaming action. Must only be set from a synchronous action
      * context. */
    private[BasicBackend] var streamState: AnyRef = null

    /** The streaming action which may need to be continued with the suspended state */
    private[BasicBackend] var streamingAction: SynchronousDatabaseAction[?, ? <: NoStream, Context, StreamingContext, ? <: Effect] = null

    @volatile private[this] var cancelRequested = false

    /** The Promise to complete when streaming has finished. */
    val streamingResultPromise = Promise[Null]()

    /** Indicate that the specified number of elements has been delivered. Returns the remaining
      * demand. This is an atomic operation. It must only be called from the synchronous action
      * context which performs the streaming. */
    def delivered(num: Long): Long = remaining.addAndGet(-num)

    /** Get the current demand that has not yet been marked as delivered and mark it as being in
      * the current batch. When this value is negative, the initial streaming action is still
      * running and the real demand can be computed by subtracting `Long.MinValue` from the
      * returned value. */
    def demandBatch: Long = remaining.get()

    /** Whether the stream has been cancelled by the Subscriber */
    def cancelled: Boolean = cancelRequested

    def emit(v: Any): Unit = subscriber.asInstanceOf[Subscriber[Any]].onNext(v)

    /** Finish the stream with `onComplete` if it is not finished yet. May only be called from a
      * synchronous action context. */
    def tryOnComplete: Unit = if(!finished && !cancelRequested) {
      if(streamLogger.isDebugEnabled) streamLogger.debug("Signaling onComplete()")
      finished = true
      try subscriber.onComplete() catch {
        case NonFatal(ex) => streamLogger.warn("Subscriber.onComplete failed unexpectedly", ex)
      }
    }

    /** Finish the stream with `onError` if it is not finished yet. May only be called from a
      * synchronous action context. */
    def tryOnError(t: Throwable): Unit = if(!finished) {
      if(streamLogger.isDebugEnabled) streamLogger.debug(s"Signaling onError($t)")
      finished = true
      try subscriber.onError(t) catch {
        case NonFatal(ex) => streamLogger.warn("Subscriber.onError failed unexpectedly", ex)
      }
    }

    /** Restart a suspended streaming action. Must only be called from the Subscriber context. */
    def restartStreaming: Unit = {
      readSync
      val s = streamState
      if(s ne null) {
        streamState = null
        if(streamLogger.isDebugEnabled)
          streamLogger.debug("Scheduling stream continuation after transition from demand = 0")
        val a = streamingAction
        database.scheduleSynchronousStreaming(a, this.asInstanceOf[StreamingContext], continuation = true)(
          s.asInstanceOf[a.StreamState]
        )
      } else {
        if(streamLogger.isDebugEnabled)
          streamLogger.debug("Saw transition from demand = 0, but no stream continuation available")
      }
    }

    override def subscription: BasicStreamingActionContext = this

    ////////////////////////////////////////////////////////////////////////// Subscription methods

    def request(l: Long): Unit = if(!cancelRequested) {
      if(l <= 0) {
        deferredError =
          new IllegalArgumentException("Requested count must not be <= 0 (see Reactive Streams spec, 3.9)")
        cancel()
      } else {
        if(!cancelRequested && remaining.getAndAdd(l) == 0L) restartStreaming
      }
    }

    def cancel(): Unit = if(!cancelRequested) {
      cancelRequested = true
      // Restart streaming because cancelling requires closing the result set and the session from
      // within a synchronous action context. This will also complete the result Promise and thus
      // allow the rest of the scheduled Action to run.
      if(remaining.getAndSet(Long.MaxValue) == 0L) restartStreaming
    }
  }
}
