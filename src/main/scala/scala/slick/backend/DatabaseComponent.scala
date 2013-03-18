package scala.slick.backend

import scala.util.DynamicVariable
import scala.slick.SlickException
import java.io.Closeable

/**
 * Backend cake slice for the basic database and session handling features.
 */
trait DatabaseComponent { self =>
  type Database <: DatabaseDef
  type DatabaseFactory <: DatabaseFactoryDef
  type Session >: Null <: SessionDef

  val Database: DatabaseFactory

  /**
   * A database instance to which connections can be created.
   */
  trait DatabaseDef {
    /**
     * Create a new session. The session needs to be closed explicitly by calling its close() method.
     */
    def createSession(): Session

    /**
     * Run the supplied function with a new session and automatically close the session at the end.
     */
    def withSession[T](f: Session => T): T = {
      val s = createSession()
      try { f(s) } finally s.close()
    }

    /**
     * Run the supplied thunk with a new session and automatically close the session at the end.
     * The session is stored in a thread-local variable which can be accessed with the implicit
     * function in Database.Implicit.
     */
    def withSession[T](f: => T): T = withSession { s: Session => withThreadLocalSession(s)(f) }

    /**
     * Run the supplied function with a new session in a transaction and automatically close the session at the end.
     */
    def withTransaction[T](f: Session => T): T = withSession { s => s.withTransaction(f(s)) }

    /**
     * Run the supplied thunk with a new session in a transaction and automatically close the session at the end.
     * The session is stored in a thread-local variable which can be accessed with the implicit
     * function in Database.Implicit.
     */
    def withTransaction[T](f: => T): T = withSession { Database.threadLocalSession.withTransaction(f) }
  }

  private[this] val dyn = new DynamicVariable[Session](null)

  protected def withThreadLocalSession[T](s: Session)(f: => T): T = dyn.withValue(s)(f)

  trait DatabaseFactoryDef {
    /**
     * An implicit function that returns the thread-local session in a withSession block
     */
    implicit def threadLocalSession: Session = {
      val s = dyn.value
      if(s eq null)
        throw new SlickException("No implicit session available; threadLocalSession can only be used within a withSession block")
      else s
    }
  }

  trait SessionDef extends Closeable {
    /**
     * Close this Session.
     */
    def close(): Unit

    /**
     * Call this method within a <em>withTransaction</em> call to roll back the current
     * transaction after <em>withTransaction</em> returns.
     */
    def rollback(): Unit

    /**
     * Run the supplied function within a transaction. If the function throws an Exception
     * or the session's rollback() method is called, the transaction is rolled back,
     * otherwise it is committed when the function returns.
     */
    def withTransaction[T](f: => T): T

    /**
     * Use this Session as the threadLocalSession for running the supplied thunk.
     */
    def asThreadLocal[T](f: => T): T = withThreadLocalSession[T](this.asInstanceOf[Session])(f)

    /**
     * Force an actual database session to be opened. Slick sessions are lazy,
     * so you do not get a real database connection until you need it or you
     * call force() on the session.
     */
    def force(): Unit
  }
}
