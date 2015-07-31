package slick.memory

import com.typesafe.config.Config
import org.reactivestreams.Subscriber

import scala.concurrent.{ExecutionContext, Future, blocking}
import slick.SlickException
import slick.dbio._
import slick.backend.{RelationalBackend, DatabaseComponent}
import slick.util.Logging
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Try}

/** The backend for DistributedDriver */
trait DistributedBackend extends RelationalBackend with Logging {
  type This = DistributedBackend
  type Database = DatabaseDef
  type Session = SessionDef
  type DatabaseFactory = DatabaseFactoryDef
  type Context = BasicActionContext
  type StreamingContext = BasicStreamingActionContext

  val Database = new DatabaseFactoryDef
  val backend: DistributedBackend = this

  def createDatabase(config: Config, path: String): Database =
    throw new SlickException("DistributedBackend cannot be configured with an external config file")

  class DatabaseDef(val dbs: Vector[DatabaseComponent#DatabaseDef], val executionContext: ExecutionContext) extends super.DatabaseDef {
    protected[this] def createDatabaseActionContext[T](_useSameThread: Boolean): Context =
      new BasicActionContext { val useSameThread = _useSameThread }

    protected[this] def createStreamingDatabaseActionContext[T](s: Subscriber[_ >: T], useSameThread: Boolean): StreamingContext =
      new BasicStreamingActionContext(s, useSameThread, DatabaseDef.this)

    def createSession(): Session = {
      val sessions = new ArrayBuffer[DatabaseComponent#Session]
      for(db <- dbs)
        sessions += Try(db.createSession()).recoverWith { case ex =>
          sessions.reverseIterator.foreach { s => Try(s.close()) }
          Failure(ex)
        }.get
      new SessionDef(sessions.toVector)
    }

    protected[this] val synchronousExecutionContext: ExecutionContext = new ExecutionContext {
      def reportFailure(t: Throwable): Unit = executionContext.reportFailure(t)
      def execute(runnable: Runnable): Unit = executionContext.execute(new Runnable {
        def run(): Unit = blocking(runnable.run)
      })
    }

    override def shutdown: Future[Unit] = Future.successful(())
    def close: Unit = ()
  }

  class DatabaseFactoryDef {
    /** Create a new distributed database instance that uses the supplied ExecutionContext for
      * asynchronous execution of database actions. */
    def apply(dbs: TraversableOnce[DatabaseComponent#DatabaseDef], executionContext: ExecutionContext): Database =
      new DatabaseDef(dbs.toVector, executionContext)
  }

  class SessionDef(val sessions: Vector[DatabaseComponent#Session]) extends super.SessionDef {
    def close() {
      sessions.map(s => Try(s.close())).collectFirst{ case Failure(t) => t }.foreach(throw _)
    }

    def rollback() =
      throw new SlickException("DistributedBackend does not currently support transactions")

    def force() {
      sessions.foreach(_.force)
    }

    def withTransaction[T](f: => T) =
      throw new SlickException("DistributedBackend does not currently support transactions")
  }
}

object DistributedBackend extends DistributedBackend
