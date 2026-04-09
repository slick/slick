package slick

import cats.effect.std.Dispatcher
import fs2.interop.reactivestreams.StreamUnicastPublisher
import org.reactivestreams.Publisher

import slick.basic.BasicBackend
import slick.dbio.{DBIOAction, Streaming}

/** Slick v3 compatibility layer for Play / Akka / Pekko applications.
  *
  * A single `import slick.compat.*` activates two things:
  *
  *   1. [[slick.compat.Database]] — a `Future`-based database wrapper with no type parameter,
  *      mirroring the Slick 3 `Database` API (`run`, `stream`, `close`).
  *
  *   2. `db.streamAsPublisher(action)` — an extension method on any `Database[F]` that converts
  *      a streaming action to a Reactive Streams `Publisher[T]` (for Akka Streams / Pekko Streams
  *      consumers). Requires an implicit `Dispatcher[F]` from the caller's CE3 context.
  *
  * `DBIO.from(future: Future[R])` is available without any import — it is defined directly in
  * the `DBIOAction` companion in slick-core.
  *
  * === Example ===
  * {{{
  * import slick.jdbc.H2Profile.api.*
  * import slick.compat.*
  * import cats.effect.unsafe.implicits.runtime
  *
  * val db = slick.compat.Database.forConfig("mydb")
  *
  * // run — returns Future, identical to Slick 3
  * val result: Future[Seq[User]] = db.run(users.result)
  *
  * // stream — returns Publisher, identical to Slick 3
  * val publisher: Publisher[User] = db.stream(users.result)
  *
  * // DBIO.from accepts a Future directly (no import needed — in slick core)
  * val action: DBIO[String] = DBIO.from(callExternalService())
  *
  * db.close()
  * }}}
  */
package object compat {

  /** Drop-in type alias for the Slick 3 `DatabasePublisher[T]`.
    *
    * `DatabasePublisher[T]` is `org.reactivestreams.Publisher[T]`. */
  type DatabasePublisher[T] = Publisher[T]

  /** Enriches any `Database[F]` with `streamAsPublisher` — for CE3 users who need a Reactive
    * Streams `Publisher[T]` (e.g. to hand off to Akka Streams or Pekko Streams).
    *
    * Requires an implicit `Dispatcher[F]` from the caller's CE3 context:
    * {{{
    * Dispatcher.parallel[IO].use { implicit dispatcher =>
    *   val publisher: Publisher[User] = db.streamAsPublisher(users.result)
    *   // hand to an Akka/Pekko Streams Source.fromPublisher(...)
    *   IO.unit
    * }
    * }}}
    */
  implicit class DatabasePublisherOps[F[_]](val db: BasicBackend#BasicDatabaseDef[F]) {
    def streamAsPublisher[T](
      a: DBIOAction[?, Streaming[T], Nothing]
    )(implicit dispatcher: Dispatcher[F]): Publisher[T] = {
      implicit val asyncF: cats.effect.Async[F] = db.asyncF
      // Construct StreamUnicastPublisher directly with the caller-supplied dispatcher rather than
      // going through toUnicastPublisher, which would allocate a new Dispatcher.parallel[F] per
      // call and leak it (there is no safe place to run its Resource finalizer from a method that
      // returns a plain Publisher[T]).  Each subscriber drives its own independent FS2 stream
      // execution; connection acquisition and release are bracketed inside the stream itself via
      // BasicBackend.withSessionStream's Stream.bracketCase.
      new StreamUnicastPublisher(db.stream(a), dispatcher)
    }
  }
}
