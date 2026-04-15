package slick

import java.io.Closeable
import slick.dbio.{DBIOAction, NoStream, Streaming}

/** Effect-polymorphic database API.
  *
  * `F[_]` is the effect type used for non-streaming execution (for example
  * `cats.effect.IO` or `scala.concurrent.Future`), and `S[_]` is the stream
  * type used for streaming results.
  *
  * Concrete wrappers (for example `slick.cats.Database`, `slick.future.Database`,
  * `slick.zio.Database`) delegate to an underlying Slick backend database and
  * adapt it into their effect/stream abstractions.
  */
trait Database[F[_], S[_]] extends Closeable {
  /** Run a `DBIOAction` and return its result in `F[R]`. */
  def run[R](a: DBIOAction[R, NoStream, Nothing]): F[R]

  /** Open a streaming `DBIOAction` as `S[T]`.
    *
    * Resource acquisition and release semantics are defined by the concrete
    * wrapper implementation, but must ensure backend resources are released
    * when the stream completes, fails, or is canceled.
    */
  def stream[T](a: DBIOAction[?, Streaming[T], Nothing]): S[T]

  /** Inspect current admission and connection-slot control status. */
  def controlStatus: F[ControlStatus]

  /** Free all resources allocated for this database instance.
    *
    * Implementations must be idempotent (safe to call more than once) and
    * thread-safe, as required by the `java.io.Closeable` contract. */
  def close(): Unit
}
