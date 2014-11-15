package scala.slick.backend

import scala.language.existentials

import scala.concurrent.{Promise, ExecutionContext, Future}
import scala.slick.action._
import scala.slick.util.{DumpInfo, TreeDump, SlickLogger}
import scala.util.{Try, Success, Failure, DynamicVariable}
import scala.util.control.NonFatal
import scala.slick.SlickException
import java.io.Closeable

import org.slf4j.LoggerFactory

/** Backend for the basic database and session handling features.
  * Concrete backends like `JdbcBackend` extend this type and provide concrete
  * types for `Database`, `DatabaseFactory` and `Session`. */
trait DatabaseComponent { self =>
  protected lazy val actionLogger = new SlickLogger(LoggerFactory.getLogger(classOf[DatabaseComponent].getName+".action"))

  type This >: this.type <: DatabaseComponent
  /** The type of database objects used by this backend. */
  type Database <: DatabaseDef
  /** The type of the database factory used by this backend. */
  type DatabaseFactory <: DatabaseFactoryDef
  /** The type of session objects used by this backend. */
  type Session >: Null <: SessionDef
  /** The action effects supported by this backend. */
  type Effects <: Effect.Read with Effect.BackendType[This]

  /** The database factory */
  val Database: DatabaseFactory

  /** A database instance to which connections can be created. */
  trait DatabaseDef { this: Database =>
    /** Create a new session. The session needs to be closed explicitly by calling its close() method. */
    def createSession(): Session

    /** Free all resources allocated by Slick for this Database. */
    def close(): Unit

    /** Run an Action asynchronously and return the result as a Future. */
    final def run[R](a: Action[Effects, R]): Future[R] = runInContext(a, new DatabaseActionContext)

    /** Run an Action in an existing DatabaseActionContext. This method can be overridden in
      * subclasses to support new DatabaseActions which cannot be expressed through
      * SynchronousDatabaseAction. */
    protected def runInContext[R](a: Action[Effects, R], ctx: DatabaseActionContext): Future[R] = {
      if(actionLogger.isDebugEnabled && a.isLogged) {
        ctx.sequenceCounter += 1
        val logA = a.nonFusedEquivalentAction
        val aPrefix = if(a eq logA) "" else "[fused] "
        val dump = TreeDump.get(logA, prefix = "    ", firstPrefix = aPrefix, narrow = {
          case a: Action[_, _] => a.nonFusedEquivalentAction
          case o => o
        })
        val msg = DumpInfo.highlight("#" + ctx.sequenceCounter) + ": " + dump.substring(0, dump.length-1)
        actionLogger.debug(msg)
      }
      a match {
        case SuccessAction(v) => Future.successful(v)
        case FailureAction(t) => Future.failed(t)
        case FutureAction(f) => f
        case FlatMapAction(base, f, ec) => runInContext(base, ctx).flatMap(v => runInContext(f(v), ctx))(ec)
        case AndThenAction(a1, a2) =>
          runInContext(a1, ctx).flatMap(_ => runInContext(a2, ctx))(Action.sameThreadExecutionContext)
        case ZipAction(a1, a2) =>
          runInContext(a1, ctx).flatMap { r1 =>
            runInContext(a2, ctx).map { r2 =>
              (r1, r2)
            }(Action.sameThreadExecutionContext)
          }(Action.sameThreadExecutionContext).asInstanceOf[Future[R]]
        case AndFinallyAction(a1, a2) =>
          val p = Promise[R]()
          runInContext(a1, ctx).onComplete { t1 =>
            runInContext(a2, ctx).onComplete { t2 =>
              if(t1.isFailure || t2.isSuccess) p.complete(t1)
              else p.complete(t2.asInstanceOf[Failure[R]])
            } (Action.sameThreadExecutionContext)
          } (Action.sameThreadExecutionContext)
          p.future
        case FailedAction(a) =>
          runInContext(a, ctx).failed.asInstanceOf[Future[R]]
        case AsTryAction(a) =>
          val p = Promise[R]()
          runInContext(a, ctx).onComplete(v => p.success(v.asInstanceOf[R]))(Action.sameThreadExecutionContext)
          p.future
        case a: SynchronousDatabaseAction[_, _, _] =>
          runSynchronousDatabaseAction(a.asInstanceOf[SynchronousDatabaseAction[This, _, R]], ctx)
        case a: DatabaseAction[_, _, _] =>
          throw new SlickException(s"Unsupported database action $a for $this")
      }
    }

    /** Run a `SynchronousDatabaseAction` on this database. */
    protected[this] def runSynchronousDatabaseAction[R](a: SynchronousDatabaseAction[This, _, R], ctx: DatabaseActionContext): Future[R] =
      scheduleSynchronousDatabaseAction {
        ctx.synchronized { // There is never any contention but context and session contain non-volatile data
          if(!ctx.isPinned) ctx.currentSession = createSession()
          val res = try a.run(ctx) catch { case NonFatal(ex) =>
            if(!ctx.isPinned) {
              try ctx.currentSession.close() catch { case NonFatal(_) => }
              ctx.currentSession = null
            }
            throw ex
          }
          if(!ctx.isPinned) {
            ctx.currentSession.close()
            ctx.currentSession = null
          }
          res
        }
      }

    /** Schedule a synchronous block of code from that runs a `SynchronousDatabaseAction` for
      * asynchronous execution. */
    protected[this] def scheduleSynchronousDatabaseAction[R](f: => R): Future[R]

    /** Run the supplied function with a new session and automatically close the session at the end.
      * Exceptions thrown while closing the session are propagated, but only if the code block using the
      * session terminated normally. Otherwise the first exception wins. */
    def withSession[T](f: Session => T): T = {
      val s = createSession()
      var ok = false
      try {
        val res = f(s)
        ok = true
        res
      } finally {
        if(ok) s.close() // Let exceptions propagate normally
        else {
          // f(s) threw an exception, so don't replace it with an Exception from close()
          try s.close() catch { case _: Throwable => }
        }
      }
    }

    /** Run the supplied thunk with a new session and automatically close the
      * session at the end.
      * The session is stored in a dynamic (inheritable thread-local) variable
      * which can be accessed with the implicit function in
      * Database.dynamicSession. */
    def withDynSession[T](f: => T): T = withSession { s: Session => withDynamicSession(s)(f) }

    /** Run the supplied function with a new session in a transaction and automatically close the session at the end. */
    def withTransaction[T](f: Session => T): T = withSession { s => s.withTransaction(f(s)) }

    /** Run the supplied thunk with a new session in a transaction and
      * automatically close the session at the end.
      * The session is stored in a dynamic (inheritable thread-local) variable
      * which can be accessed with the implicit function in
      * Database.dynamicSession. */
    def withDynTransaction[T](f: => T): T = withDynSession { Database.dynamicSession.withTransaction(f) }
  }

  private[this] val dyn = new DynamicVariable[Session](null)

  /** Run a block of code with the specified `Session` bound to the thread-local `dynamicSession`. */
  protected def withDynamicSession[T](s: Session)(f: => T): T = dyn.withValue(s)(f)

  /** Factory methods for creating `Database` instances. */
  trait DatabaseFactoryDef {
    /** An implicit function that returns the thread-local session in a withSession block. */
    implicit def dynamicSession: Session = {
      val s = dyn.value
      if(s eq null)
        throw new SlickException("No implicit session available; dynamicSession can only be used within a withDynSession block")
      else s
    }
  }

  /** A logical session of a `Database`. The underlying database connection is created lazily on demand. */
  trait SessionDef extends Closeable {
    /** Close this Session. */
    def close(): Unit

    /** Call this method within a `withTransaction` call to roll back the current
      * transaction after `withTransaction` returns. */
    def rollback(): Unit

    /** Run the supplied function within a transaction. If the function throws an Exception
      * or the session's `rollback()` method is called, the transaction is rolled back,
      * otherwise it is committed when the function returns. */
    def withTransaction[T](f: => T): T

    /** Use this Session as the `dynamicSession` for running the supplied thunk. */
    def asDynamicSession[T](f: => T): T = withDynamicSession[T](this.asInstanceOf[Session])(f)

    /** Force an actual database session to be opened. Slick sessions are lazy, so you do not
      * get a real database connection until you need it or you call force() on the session. */
    def force(): Unit
  }

  /** The context object passed to database actions by the execution engine. */
  class DatabaseActionContext extends ActionContext[This] {
    private[DatabaseComponent] var currentSession: Session = null
    @volatile private[DatabaseComponent] var sequenceCounter = 0

    def session: Session = currentSession
  }
}
