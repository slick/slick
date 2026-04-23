package slick.basic

import slick.dbio.DBIOAction

/** Listener hook for tracing and instrumentation around DBIO node execution.
  *
  * The `exec` effect represents the original execution of `action`. Implementations can run
  * effectful logic before/after `exec` (for example start/finish spans, timing, logging), but
  * they cannot inspect or alter the `H` value itself.
  */
trait ActionListener[F[_]] {
  /** Wrap execution of a single DBIO node.
    *
    * `H` is intentionally opaque: listeners can only sequence instrumentation around `exec`.
    * They cannot construct a replacement result based on `H`.
    */
  def around[R, H](action: DBIOAction[R, _, _], exec: F[H]): F[H]
}

object ActionListener {
  /** Listener that performs no wrapping. */
  def noop[F[_]]: ActionListener[F] =
    new ActionListener[F] {
      override def around[R, H](action: DBIOAction[R, _, _], exec: F[H]): F[H] = exec
    }
}
