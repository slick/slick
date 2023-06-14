package slick.memory

import scala.collection.compat.*
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{blocking, ExecutionContext, Future}
import scala.util.{Failure, Try}

import slick.SlickException
import slick.basic.BasicBackend
import slick.relational.RelationalBackend
import slick.util.Logging

import com.typesafe.config.Config
import org.reactivestreams.Subscriber

/** The backend for DistributedProfile. */
trait DistributedBackend extends RelationalBackend with Logging {
  type This = DistributedBackend
  type Database = DistributedDatabaseDef
  type Session = DistributedSessionDef
  type DatabaseFactory = DistributedDatabaseFactoryDef
  type Context = BasicActionContext
  type StreamingContext = BasicStreamingActionContext

  val Database = new DistributedDatabaseFactoryDef
  val backend: DistributedBackend = this

  def createDatabase(config: Config, path: String): Database =
    throw new SlickException("DistributedBackend cannot be configured with an external config file")

  class DistributedDatabaseDef(val dbs: Vector[BasicBackend#BasicDatabaseDef], val executionContext: ExecutionContext)
    extends BasicDatabaseDef {

    protected[this] def createDatabaseActionContext[T](_useSameThread: Boolean): Context =
      new BasicActionContext { val useSameThread = _useSameThread }

    protected[this] def createStreamingDatabaseActionContext[T](s: Subscriber[_ >: T],
                                                                useSameThread: Boolean): StreamingContext =
      new BasicStreamingActionContext(s, useSameThread, this)

    def createSession(): Session = {
      val sessions = new ArrayBuffer[BasicBackend#Session]
      for(db <- dbs)
        sessions += Try(db.createSession()).recoverWith { case ex =>
          sessions.reverseIterator.foreach { s => Try(s.close()) }
          Failure(ex)
        }.get
      new DistributedSessionDef(sessions.toVector)
    }

    protected[this] val synchronousExecutionContext: ExecutionContext = new ExecutionContext {
      def reportFailure(t: Throwable): Unit = executionContext.reportFailure(t)
      def execute(runnable: Runnable): Unit = executionContext.execute(new Runnable {
        def run(): Unit = blocking(runnable.run())
      })
    }

    override def shutdown: Future[Unit] = Future.successful(())
    def close: Unit = ()
  }

  class DistributedDatabaseFactoryDef {
    /** Create a new distributed database instance that uses the supplied ExecutionContext for
      * asynchronous execution of database actions. */
    def apply(dbs: IterableOnce[BasicBackend#BasicDatabaseDef], executionContext: ExecutionContext): Database =
      new DistributedDatabaseDef(Vector.from(dbs), executionContext)
  }

  class DistributedSessionDef(val sessions: Vector[BasicBackend#Session]) extends BasicSessionDef {
    def close(): Unit = {
      sessions.map(s => Try(s.close())).collectFirst{ case Failure(t) => t }.foreach(throw _)
    }

    def rollback() =
      throw new SlickException("DistributedBackend does not currently support transactions")

    def force(): Unit = {
      sessions.foreach(_.force())
    }

    def withTransaction[T](f: => T) =
      throw new SlickException("DistributedBackend does not currently support transactions")
  }
}

object DistributedBackend extends DistributedBackend
