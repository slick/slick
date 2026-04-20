package slick.cats

import cats.effect.IO
import cats.effect.Resource

import slick.ControlStatus
import slick.dbio.{DBIOAction, NoStream, Streaming}

/** Cats Effect / fs2 facade for Slick's effect-polymorphic database API. */
trait Database extends slick.Database[IO, Database.StreamIO] {
  /**
    * Run a non-streaming action as `IO[R]`.
    *
    * The returned `IO` is lazy: the underlying `DBIOAction` starts when the `IO` is run,
    * not when it is created.
    *
    * If the same `IO` value is run multiple times, each run performs a fresh, independent
    * execution of the action.
    */
  override def run[R](a: DBIOAction[R, NoStream, Nothing]): IO[R]

  /**
    * Open a streaming action as an `fs2.Stream[IO, T]`.
    *
    * The returned stream is lazy: the underlying `DBIOAction` starts when the stream is
    * consumed, not when it is created.
    *
    * If the same stream value is consumed multiple times, each consumption performs a fresh,
    * independent execution of the action.
    */
  override def stream[T](a: DBIOAction[?, Streaming[T], Nothing]): fs2.Stream[IO, T]
}

object Database {
  type StreamIO[A] = fs2.Stream[IO, A]

  /**
    * Create a [[slick.cats.Database]] from an already-open core Slick database.
    *
    * This is a low-level escape hatch intended for integration points that already
    * have a `BasicBackend#BasicDatabaseDef[IO]`.
    *
    * In regular application code, prefer `resource`.
    */
  def fromCore(db: slick.basic.BasicBackend#BasicDatabaseDef[IO]): Database =
    new Database {
      override def run[R](a: DBIOAction[R, NoStream, Nothing]): IO[R] =
        db.run(a)

      override def stream[T](a: DBIOAction[?, Streaming[T], Nothing]): fs2.Stream[IO, T] =
        fs2.Stream.resource(db.stream(a)).flatMap { it =>
          // chunkSize = 1 is required by mutators since they require the underlying
          // iterator doesn't advance beyond what the mutator object represents
          fs2.Stream.fromIterator[IO](it, chunkSize = 1)
        }

      override def controlStatus: IO[ControlStatus] =
        db.controlStatus

      override def close(): Unit =
        db.close()
    }

  /**
    * Create a new database instance from a basic profile configuration.
    *
    * The returned `cats.effect.IO` yields a fresh [[slick.cats.Database]].
    * The caller owns the lifecycle and must call `db.close()` when done.
    *
    * If you want automatic lifecycle management, prefer `resource`.
    */
  def make[P <: slick.basic.BasicProfile](config: slick.basic.BasicDatabaseConfig[P]): IO[Database] =
    config.profile.backend.makeDatabase[IO](config).map(fromCore)

  /**
    * Create a new database instance from a JDBC profile configuration.
    *
    * The returned `cats.effect.IO` yields a fresh [[slick.cats.Database]].
    * The caller owns the lifecycle and must call `db.close()` when done.
    *
    * If you want automatic lifecycle management, prefer `resource`.
    */
  def make[P <: slick.jdbc.JdbcProfile](config: slick.jdbc.JdbcDatabaseConfig[P]): IO[Database] =
    config.profile.backend.makeDatabase[IO](config).map(fromCore)

  /**
    * Open a new database as a `Resource` and always close it.
    *
    * Each invocation opens a new database instance, so this should usually wrap
    * the whole program that needs a database, not individual queries.
    *
    * Acquire this once at the top-most level and pass it down. In normal
    * application usage, keep only one opened database at a time.
    */
  def resource[P <: slick.basic.BasicProfile](config: slick.basic.BasicDatabaseConfig[P]): Resource[IO, Database] =
    Resource.make(make(config))(db => IO.blocking(db.close()).attempt.void)

  /**
    * Open a new JDBC database as a `Resource` and always close it.
    *
    * Each invocation opens a new database instance, so this should usually wrap
    * the whole program that needs a database, not individual queries.
    *
    * Acquire this once at the top-most level and pass it down. In normal
    * application usage, keep only one opened database at a time.
    */
  def resource[P <: slick.jdbc.JdbcProfile](config: slick.jdbc.JdbcDatabaseConfig[P]): Resource[IO, Database] =
    Resource.make(make(config))(db => IO.blocking(db.close()).attempt.void)
}
