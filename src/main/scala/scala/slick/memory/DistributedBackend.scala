package scala.slick.memory

import scala.concurrent.{ExecutionContext, Future, blocking}
import scala.slick.SlickException
import scala.slick.action._
import scala.slick.backend.{RelationalBackend, DatabaseComponent}
import scala.slick.util.Logging
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Try}

/** The backend for DistributedDriver */
trait DistributedBackend extends RelationalBackend with Logging {
  type This = DistributedBackend
  type Database = DatabaseDef
  type Session = SessionDef
  type DatabaseFactory = DatabaseFactoryDef
  type Effects = Effect.Read with Effect.Write with Effect.Schema with Effect.BackendType[This]

  val Database = new DatabaseFactoryDef
  val backend: DistributedBackend = this

  class DatabaseDef(val dbs: Vector[DatabaseComponent#DatabaseDef]) extends super.DatabaseDef {
    def createSession(): Session = {
      val sessions = new ArrayBuffer[DatabaseComponent#Session]
      for(db <- dbs)
        sessions += Try(db.createSession()).recoverWith { case ex =>
          sessions.reverseIterator.foreach { s => Try(s.close()) }
          Failure(ex)
        }.get
      new SessionDef(sessions.toVector)
    }

    protected[this] def scheduleSynchronousDatabaseAction[R](f: => R): Future[R] =
      Future(blocking(f))(ExecutionContext.global)

    def close(): Unit = ()
  }

  class DatabaseFactoryDef extends super.DatabaseFactoryDef {
    def apply(dbs: DatabaseComponent#DatabaseDef*): Database = new DatabaseDef(dbs.toVector)
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
