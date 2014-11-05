package scala.slick.backend

import scala.concurrent.{ExecutionContext, Future}
import scala.slick.action._
import scala.slick.backend.DatabaseComponent.SimpleDatabaseAction
import scala.util.DynamicVariable
import scala.slick.SlickException
import java.io.Closeable

/** Backend for the basic database and session handling features.
  * Concrete backends like `JdbcBackend` extend this type and provide concrete
  * types for `Database`, `DatabaseFactory` and `Session`. */
trait DatabaseComponent { self =>
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
    def run[R](a: Action[Effects, R]): Future[R] = a match { //TODO optimize execution
      case ConstantAction(v) => Future.successful(v)
      case FutureAction(f) => f
      case FlatMapAction(base, f, ec) => run(base).flatMap(v => run(f(v)))(ec)
      case AndThenAction(a1, a2) => run(a1).flatMap(_ => run(a2))(DatabaseComponent.sameThreadExecutionContext)
      case a: DatabaseComponent.SimpleDatabaseAction[_, _, _] =>
        runSimpleDatabaseAction(a.asInstanceOf[SimpleDatabaseAction[This, _, R]])
      case a: DatabaseAction[_, _, _] =>
        throw new SlickException(s"Unsupported database action $a for $this")
    }

    /** Run a `SimpleDatabaseAction` on this database. */
    protected[this] def runSimpleDatabaseAction[R](a: SimpleDatabaseAction[This, _, R]): Future[R]

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
}

object DatabaseComponent {
  /** An ExecutionContext used internally for executing plumbing operations during Action
    * composition. */
  private[slick] object sameThreadExecutionContext extends ExecutionContext {
    override def execute(runnable: Runnable): Unit = runnable.run()
    override def reportFailure(t: Throwable): Unit = throw t
  }

  /** A 'simple' database action provides a function from a `Session` to the result type.
    * `DatabaseComponent.DatabaseDef.run` supports this kind of action out of the box through
    * `DatabaseComponent.DatabaseDef.runSimpleDatabaseAction` so that `run` does not need to
    * be extended if all primitive database actions can be expressed in this way. These actions
    * also implement construction-time fusion. */
  trait SimpleDatabaseAction[B <: DatabaseComponent, -E <: Effect, +R] extends DatabaseAction[B, E, R] { self =>
    def run(session: B#Session): R
    override def andThen[E2 <: Effect, R2](a: Action[E2, R2]): Action[E with Effect.BackendType[B] with E2, R2] = a match {
      case s2: SimpleDatabaseAction[_, _, _] => new SimpleDatabaseAction[B, Effect, R2] {
        def run(s: B#Session): R2 = {
          self.run(s)
          s2.asInstanceOf[SimpleDatabaseAction[B, E2, R2]].run(s)
        }
      }
      case a => AndThenAction(this, a)
    }
  }
}
