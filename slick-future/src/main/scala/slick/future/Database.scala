package slick.future

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

import cats.effect.IO

import slick.ControlStatus
import slick.DatabaseConfig
import slick.dbio.{DBIOAction, NoStream, Streaming}

/** Future/Reactive Streams facade for Slick's effect-polymorphic database API. */
trait Database extends slick.Database[Future, DatabasePublisher] {
  /**
    * Run a non-streaming action as `Future[R]`.
    *
    * Calling `run` executes the underlying [[slick.dbio.DBIOAction]] immediately and returns a `Future`
    * for its completion.
    *
    * Calling `run` multiple times executes the action multiple times.
    */
  override def run[R](a: DBIOAction[R, NoStream, Nothing]): Future[R]

  /**
    * Open a streaming action as a Reactive Streams [[slick.future.DatabasePublisher]]`[T]`.
    *
    * The underlying [[slick.dbio.DBIOAction]] does not start until a `Subscriber` subscribes
    * to the returned publisher.  Each subscription triggers an independent
    * execution of the action — subscribing to the same publisher twice runs the
    * action twice and produces two independent result streams.
    *
    * === LOB columns, mutators, and cursor safety ===
    *
    * Some streamed row values are only valid while the JDBC cursor is positioned
    * on that row:
    *
    *   - '''LOB references''' — `java.sql.Blob` and `java.sql.Clob` objects
    *     returned by JDBC are live handles tied to the current cursor position.
    *     On many drivers (DB2 in particular) they become invalid the moment the
    *     cursor advances.
    *
    *   - '''`ResultSetMutator` values''' — a [[slick.jdbc.ResultSetMutator]]
    *     wraps the live cursor directly; calling `row`, `row_=`, `+=`, or
    *     `delete` on a mutator after the cursor has moved yields undefined
    *     behaviour or silent data corruption.
    *
    * The publisher emits strictly one element at a time: the JDBC cursor does not advance
    * to the next row until the subscriber's `onNext` call returns and the subscriber
    * subsequently signals new demand.  This keeps cursor-bound values valid throughout
    * each `onNext` invocation regardless of how many elements the subscriber requests.
    */
  override def stream[T](a: DBIOAction[?, Streaming[T], Nothing]): DatabasePublisher[T]
}

object Database {
  import cats.effect.unsafe.implicits.global

  /** Create a [[slick.future.Database]] from an already-open core Slick database.
    *
    * This is a low-level escape hatch intended for integration points that already
    * have a `BasicBackend#BasicDatabaseDef[IO]`.
    *
    * In regular application code, prefer `open` or `use`.
    */
  def fromCore(db: slick.basic.BasicBackend#BasicDatabaseDef[IO]): Database =
    new Database {
      override def run[R](a: DBIOAction[R, NoStream, Nothing]): Future[R] =
        db.run(a).unsafeToFuture()

      override def stream[T](a: DBIOAction[?, Streaming[T], Nothing]): DatabasePublisher[T] =
        lazyPublisher(() => {
          val (it, release) = db.stream(a).allocated.unsafeRunSync()
          new DatabasePublisherImpl[T](it, () => release.unsafeRunSync())
        })

      private def lazyPublisher[T](acquire: () => DatabasePublisher[T]): DatabasePublisher[T] =
        new DatabasePublisher[T] {
          override def subscribe(s: org.reactivestreams.Subscriber[? >: T]): Unit = {
            val pub =
              try acquire()
              catch {
                case t: Throwable =>
                  s.onSubscribe(new org.reactivestreams.Subscription {
                    override def request(n: Long): Unit = ()
                    override def cancel(): Unit = ()
                  })
                  s.onError(t)
                  return
              }
            pub.subscribe(s)
          }
          override def foreach[U](f: T => U)(implicit ec: ExecutionContext): Future[Unit] =
            Future(acquire()).flatMap(_.foreach(f))
          override def mapResult[U](f: T => U): DatabasePublisher[U] =
            lazyPublisher(() => acquire().mapResult(f))
        }

      override def controlStatus: Future[ControlStatus] =
        db.controlStatus.unsafeToFuture()

      override def close(): Unit = db.close()
    }

  /** Open a new database instance from a database configuration.
    *
    * The returned `scala.concurrent.Future` completes with a fresh
    * [[slick.future.Database]]. The caller is responsible for closing it by
    * calling `close()` when done.
    *
    * If you want automatic lifecycle management, prefer `use`.
    */
  def open(config: DatabaseConfig): Future[Database] =
    config.makeDatabase[IO]().map(fromCore).unsafeToFuture()

  /** Open a new database, run a program, and always close the database.
    *
    * Each invocation opens a new database instance, so this should usually wrap the
    * whole program that needs a database, not individual queries.
    *
    * Acquire this once at the top-most level and pass it down. In normal
    * application usage, keep only one opened database at a time.
    */
  def use[T](config: DatabaseConfig)(f: Database => Future[T])(implicit ec: ExecutionContext): Future[T] =
    withOpened(open(config))(f)

  private def withOpened[T](opened: => Future[Database])(f: Database => Future[T])(implicit ec: ExecutionContext): Future[T] =
    opened.flatMap { db =>
      def tryClose(): Unit = try db.close() catch { case _: Throwable => () }
      val fut =
        try f(db)
        catch { case t: Throwable => Future.failed(t) }
      fut.andThen { case _ => tryClose() }
    }
}
