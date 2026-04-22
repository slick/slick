package slick

import scala.concurrent.duration.FiniteDuration

/** Configuration for concurrency controls (connection pool sizing, queue limits, and timeouts).
  *
  * Use [[slick.DatabaseConfig#withControls]] to attach a custom config to any database config:
  * {{{
  *   val config = DatabaseConfig.forConfig[H2Profile]("db")
  *     .withControls(ControlsConfig(maxConnections = 10, queueSize = 500))
  * }}}
  *
  * @param maxConnections          Maximum number of concurrent database connections.
  * @param queueSize               Maximum number of actions waiting for a connection slot before
  *                                being rejected.
  * @param maxInflight             Maximum number of actions that may be in-flight at once (i.e.
  *                                admitted past the queue but not yet holding a connection).
  *                                Defaults to `None`, which means `2 * maxConnections`.
  *                                The effective value is always at least `maxConnections`.
  * @param inflightAdmissionTimeout  How long an action may wait to be admitted as in-flight before
  *                                  timing out. `None` means wait indefinitely.
  * @param connectionAcquireTimeout  How long an action may wait to acquire a connection before
  *                                  timing out. `None` means wait indefinitely.
  */
final case class ControlsConfig(
  maxConnections: Int = 20,
  queueSize: Int = 1000,
  maxInflight: Option[Int] = None,
  inflightAdmissionTimeout: Option[FiniteDuration] = None,
  connectionAcquireTimeout: Option[FiniteDuration] = None
)

object ControlsConfig {
  /** Unbounded controls suitable for in-memory backends (Heap, Distributed) that have no real
    * connection pool and should never block on concurrency limits. */
  val unbounded: ControlsConfig =
    ControlsConfig(Int.MaxValue, Int.MaxValue, Some(Int.MaxValue))
}
