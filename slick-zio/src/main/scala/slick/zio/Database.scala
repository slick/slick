package slick.zio

import cats.effect.Resource

import zio.{Scope, Task, ZIO}
import zio.interop.catz.*
import zio.stream.ZStream

import slick.ControlStatus
import slick.DatabaseConfig
import slick.dbio.{DBIOAction, NoStream, Streaming}

/** ZIO facade for Slick's effect-polymorphic database API. */
trait Database extends slick.Database[Task, Database.StreamTask] {
  /**
    * Run a non-streaming action as `Task[R]`.
    *
    * The returned `Task` is lazy: the underlying [[slick.dbio.DBIOAction]] starts when the `Task` is run,
    * not when it is created.
    *
    * If the same `Task` value is run multiple times, each run performs a fresh, independent
    * execution of the action.
    */
  override def run[R](a: DBIOAction[R, NoStream, Nothing]): Task[R]

  /**
    * Open a streaming action as a `ZStream[Any, Throwable, T]`.
    *
    * The returned stream is lazy: the underlying [[slick.dbio.DBIOAction]] starts when the stream is
    * consumed, not when it is created.
    *
    * If the same stream value is consumed multiple times, each consumption performs a fresh,
    * independent execution of the action.
    */
  override def stream[T](a: DBIOAction[?, Streaming[T], Nothing]): ZStream[Any, Throwable, T]
}

object Database {
  type StreamTask[A] = ZStream[Any, Throwable, A]

  /**
    * Create a [[slick.zio.Database]] from an already-open core Slick database.
    *
    * This is a low-level escape hatch intended for integration points that already
    * have a `BasicBackend#BasicDatabaseDef[Task]`.
    *
    * In regular application code, prefer `scoped`.
    */
  def fromCore(db: slick.basic.BasicBackend#BasicDatabaseDef[Task]): Database =
    new Database {
      override def run[R](a: DBIOAction[R, NoStream, Nothing]): Task[R] =
        db.run(a)

      override def stream[T](a: DBIOAction[?, Streaming[T], Nothing]): ZStream[Any, Throwable, T] =
        ZStream.unwrapScoped(db.stream(a).toScopedZIO.map(fromIteratorNoBuffering))

      override def controlStatus: Task[ControlStatus] =
        db.controlStatus

      override def close(): Unit =
        db.close()
    }

  // Pull one element per stream step.
  // Mutator actions require that the underlying iterator does not advance beyond
  // what the mutator object represents, so we intentionally do not buffer here.
  private def fromIteratorNoBuffering[T](it: Iterator[T]): ZStream[Any, Throwable, T] =
    ZStream.unfoldZIO(it) { i =>
      ZIO.attemptBlocking(i.hasNext).flatMap { hasNext =>
        if (hasNext) ZIO.attemptBlocking(Some((i.next(), i)))
        else ZIO.succeed(None)
      }
    }

  /**
    * Create a new database instance from a database configuration.
    *
    * The returned `zio.Task` yields a fresh [[slick.zio.Database]].
    * The caller owns the lifecycle and must call `db.close()` when done.
    *
    * If you want automatic lifecycle management, prefer `scoped`.
    */
  def make(config: DatabaseConfig): Task[Database] =
    config.makeDatabase[Task]().map(fromCore)

  /**
    * Open a new database under scope and always close it.
    *
    * Each invocation opens a new database instance, so this should usually wrap
    * the whole program that needs a database, not individual queries.
    *
    * Acquire this once at the top-most level and pass it down. In normal
    * application usage, keep only one opened database at a time.
    */
  def scoped(config: DatabaseConfig): ZIO[Scope, Throwable, Database] =
    Resource.make(make(config))(db => ZIO.attemptBlocking(db.close()).ignore).toScopedZIO
}
