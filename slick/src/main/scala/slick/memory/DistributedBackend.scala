package slick.memory

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Try}

import cats.effect.{Async, IO, Resource}
import cats.effect.std.Semaphore
import cats.effect.unsafe.implicits.global

import slick.SlickException
import slick.basic.BasicBackend
import slick.relational.RelationalBackend
import slick.util.Logging

import com.typesafe.config.Config

/** The backend for DistributedProfile. */
trait DistributedBackend extends RelationalBackend with Logging {
  type Database[F[_]] = DistributedDatabaseDef[F]
  type Session = DistributedSessionDef
  type DatabaseFactory = DistributedDatabaseFactoryDef
  type Context = BasicActionContext
  type StreamingContext = BasicStreamingActionContext

  val Database = new DistributedDatabaseFactoryDef
  val backend: DistributedBackend = this

  def createDatabase[F[_]: Async](config: Config, path: String, classLoader: ClassLoader = slick.util.ClassLoaderUtil.defaultClassLoader): Resource[F, Database[F]] =
    Resource.eval(Async[F].raiseError(new SlickException("DistributedBackend cannot be configured with an external config file")))

  class DistributedDatabaseDef[F[_]](val dbs: Vector[BasicBackend#AnyDatabaseDef], override val semaphore: Semaphore[F])(implicit override val asyncF: cats.effect.Async[F])
    extends BasicDatabaseDef[F] {

    /** DistributedBackend always uses cats.effect.IO as its effect type. */

    override protected def sessionAsContext(session: Session, state: ExecState): Context = {
      val s = session
      val depth = state.transactionDepth
      val pinned = state.pinned
      new BasicActionContext {
        override def session: Session = s
        override def transactionDepth: Int = depth
        override def isPinned: Boolean = pinned
      }
    }

    def createSession(): Session = {
      val sessions = new ArrayBuffer[BasicBackend#BasicSessionDef]
      for(db <- dbs)
        sessions += Try(db.createSession()).recoverWith { case ex =>
          sessions.reverseIterator.foreach { s => Try(s.close()) }
          Failure(ex)
        }.get
      new DistributedSessionDef(sessions.toVector)
    }

    def close(): Unit = ()

    override protected def streamFromSDA[T](
      a: slick.dbio.SynchronousDatabaseAction[?, slick.dbio.Streaming[T], Context, StreamingContext, Nothing],
      session: Session,
      state: ExecState
    ): fs2.Stream[F, T] =
      fs2.Stream.raiseError[F](new SlickException("DistributedBackend does not support streaming"))
  }

  class DistributedDatabaseFactoryDef {
    /** Create a new distributed database instance. */
    def apply(dbs: IterableOnce[BasicBackend#AnyDatabaseDef]): Database[IO] = {
      val sem = Semaphore[IO](Long.MaxValue).unsafeRunSync()
      new DistributedDatabaseDef[IO](Vector.from(dbs), sem)
    }
  }

  class DistributedSessionDef(val sessions: Vector[BasicBackend#BasicSessionDef]) extends BasicSessionDef {
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
