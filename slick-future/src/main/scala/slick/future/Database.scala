package slick.future

import java.sql.Driver
import java.util.Properties
import javax.sql.DataSource

import scala.concurrent.Future

import cats.effect.{IO, Resource}
import cats.effect.std.Dispatcher
import cats.effect.unsafe.IORuntime
import fs2.interop.reactivestreams.StreamUnicastPublisher
import org.reactivestreams.Publisher

import com.typesafe.config.Config

import slick.dbio.{DBIOAction, NoStream, Streaming}
import slick.basic.BasicBackend
import slick.jdbc.JdbcBackend
import slick.util.ClassLoaderUtil

/** A Slick database that runs actions as `scala.concurrent.Future` rather than
  * `cats.effect.IO`. It provides a Future-based API surface for Play / Akka / Pekko
  * applications that use `Future` at their framework boundary and want to migrate to Slick 4
  * with minimal changes.
  *
  * Obtain via the companion object factory methods, which mirror the Slick 3 `Database.forConfig`
  * / `Database.forDataSource` / `Database.forURL` API exactly — no `Resource`, no type parameter:
  *
  * {{{
  * // Construction — once, e.g. in a Play Module or Akka extension
  * import cats.effect.unsafe.implicits.runtime   // or inject a custom IORuntime via DI
  * val db = slick.future.Database.forConfig("mydb")
  *
  * // Usage — identical to Slick 3
  * import slick.future.*
  * val result: Future[Seq[User]] = db.run(users.result)
  * val publisher: Publisher[User] = db.stream(users.result)
  * val action: DBIO[String]       = DBIO.from(callExternalService()) // Future overload
  *
  * // Shutdown — identical to Slick 3
  * db.close()
  * }}}
  *
  * For CE3-based code that needs a `Database[IO]` directly, use the `wrap` factory method
  * to convert an already-constructed `Database[IO]` and `Dispatcher[IO]` into a
  * `slick.future.Database`.
  *
  * @see [[slick.future]] for the `DBIO.from(future)` enrichment and the
  *      `DatabasePublisher[T]` type alias.
  */
final class Database private[future] (
  /** The underlying `Database[IO]`. Exposed so that callers who need full CE3 control can
    * access `run`, `stream`, and `io` on it directly. */
  val underlying: BasicBackend#BasicDatabaseDef[IO],
  private val dispatcher: Dispatcher[IO],
  private val releaseAll: IO[Unit]
)(implicit private val runtime: IORuntime) {

  /** Run a non-streaming action and return the result as a `scala.concurrent.Future`. */
  def run[R](a: DBIOAction[R, NoStream, Nothing]): Future[R] =
    underlying.run(a).unsafeToFuture()

  /** Stream results as a Reactive Streams `org.reactivestreams.Publisher`.
    *
    * Each subscriber triggers an independent database query execution (unicast publisher).
    * The publisher is backed by an FS2 stream obtained from the underlying `Database[IO]`.
    */
  def stream[T](a: DBIOAction[?, Streaming[T], Nothing]): Publisher[T] = {
    implicit val asyncIO: cats.effect.Async[IO] = underlying.asyncF
    // Construct StreamUnicastPublisher directly with the already-managed dispatcher rather than
    // going through toUnicastPublisher, which would allocate a new Dispatcher.parallel[IO] per
    // call and leak it (there is no safe place to run its Resource finalizer from a method that
    // returns a plain Publisher[T]).  Each subscriber drives its own independent FS2 stream
    // execution; connection acquisition and release are bracketed inside the stream itself via
    // BasicBackend.withSessionStream's Stream.bracketCase.
    new StreamUnicastPublisher(underlying.stream(a), dispatcher)
  }

  /** Close the database and release all resources (connection pool, internal dispatcher).
    *
    * When the `Database` was created via a factory method (`forConfig`, `forDataSource`, etc.)
    * this shuts everything down. When created via `Database.wrap(Resource[...])`, this is a no-op — the
    * caller is responsible for closing the underlying resources.
    */
  def close(): Unit = releaseAll.unsafeRunSync()
}

object Database {

  // ---------------------------------------------------------------------------
  // Public wrap — caller owns lifecycle
  // ---------------------------------------------------------------------------

  /** Wrap an already-constructed `Database[IO]` and `Dispatcher[IO]` into a
    * [[slick.future.Database]].
    *
    * Use this when you need full control over the underlying `Database[IO]` lifecycle — for
    * example when constructing it inside a CE3 `Resource` and handing a Future-facing handle
    * to a framework component (a Play controller, an Akka extension, etc.).
    *
    * The caller is responsible for keeping `dispatcher` open for the lifetime of the returned
    * [[Database]] and for closing both `db` and `dispatcher` independently. Calling `close()`
    * on the returned [[Database]] is a no-op.
    */
  def wrap(
    db: BasicBackend#BasicDatabaseDef[IO],
    dispatcher: Dispatcher[IO]
  )(implicit runtime: IORuntime): Database =
    new Database(db, dispatcher, IO.unit)

  /** Wrap an existing `Database[IO]` and let this module own the publisher dispatcher lifecycle.
    *
    * `close()` will close both the wrapped database and the internal dispatcher.
    */
  def wrap(
    db: Resource[IO, BasicBackend#BasicDatabaseDef[IO]]
  )(implicit runtime: IORuntime): Database = {
    val combined: Resource[IO, (BasicBackend#BasicDatabaseDef[IO], Dispatcher[IO])] =
      db.flatMap(d => Dispatcher.parallel[IO].map(disp => (d, disp)))
    val ((d, disp), releaseAll) = combined.allocated.unsafeRunSync()
    new Database(d, disp, releaseAll)
  }

  // ---------------------------------------------------------------------------
  // Factory methods — slick-future owns lifecycle
  // ---------------------------------------------------------------------------

  private def fromResource(
    resource: Resource[IO, JdbcBackend#JdbcDatabaseDef[IO]]
  )(implicit runtime: IORuntime): Database = {
    val combined: Resource[IO, (JdbcBackend#JdbcDatabaseDef[IO], Dispatcher[IO])] =
      resource.flatMap(db => Dispatcher.parallel[IO].map(disp => (db, disp)))
    val ((db, disp), releaseAll) = combined.allocated.unsafeRunSync()
    new Database(db, disp, releaseAll)
  }

  /** Create a [[Database]] from [[https://github.com/lightbend/config Typesafe Config]].
    *
    * Mirrors Slick 3's `Database.forConfig(path)`. The connection pool is closed when
    * `close()` is called.
    */
  def forConfig(
    path: String,
    config: Config = null,
    driver: Driver = null,
    classLoader: ClassLoader = ClassLoaderUtil.defaultClassLoader
  )(implicit runtime: IORuntime): Database =
    fromResource(JdbcBackend.Database.forConfig[IO](path, config, driver, classLoader))

  /** Create a [[Database]] from a `javax.sql.DataSource`.
    *
    * Mirrors Slick 3's `Database.forDataSource(ds, maxConnections)`.
    */
  def forDataSource(
    ds: DataSource,
    maxConnections: Option[Int],
    keepAliveConnection: Boolean = false
  )(implicit runtime: IORuntime): Database =
    fromResource(JdbcBackend.Database.forDataSource[IO](ds, maxConnections, keepAliveConnection))

  /** Create a [[Database]] using a JDBC URL via the DriverManager.
    *
    * Mirrors Slick 3's `Database.forURL(url, ...)`.
    */
  def forURL(
    url: String,
    user: String = null,
    password: String = null,
    prop: Properties = null,
    driver: String = null,
    keepAliveConnection: Boolean = false,
    classLoader: ClassLoader = ClassLoaderUtil.defaultClassLoader
  )(implicit runtime: IORuntime): Database =
    fromResource(
      JdbcBackend.Database.forURL[IO](url, user, password, prop, driver, keepAliveConnection, classLoader)
    )

  /** Create a [[Database]] using a `java.sql.Driver` instance directly.
    *
    * Mirrors Slick 3's `Database.forDriver(driver, url, ...)`.
    */
  def forDriver(
    driver: Driver,
    url: String,
    user: String = null,
    password: String = null,
    prop: Properties = null
  )(implicit runtime: IORuntime): Database =
    fromResource(JdbcBackend.Database.forDriver[IO](driver, url, user, password, prop))
}
