package slick.basic

import cats.effect.Sync
import cats.effect.kernel.Outcome
import cats.effect.syntax.all.*
import cats.syntax.all.*
import org.slf4j.LoggerFactory

import slick.dbio.DBIOAction
import slick.sql.SqlAction
import slick.util.SlickLogger

/** Listener hook for tracing and instrumentation around DBIO node execution.
  *
  * The `exec` effect represents the original execution of `action`. Implementations can run
  * effectful logic before/after `exec` (for example start/finish spans, timing, logging), but
  * they cannot inspect or alter the `H` value itself.
  *
  * A listener is installed when the database is created with `dc.profile.backend.makeDatabase[F](dc, listener)`,
  * whose result can be wrapped with `fromCore` of the `slick.cats`, `slick.future` or `slick.zio` `Database`.
  * It replaces the default listener, which logs to the `slick.basic.BasicBackend.action` logger and, through
  * [[ActionListener.timing]], to the `slick.basic.BasicBackend.timing` logger.
  * Use [[ActionListener.combine]] to install more than one.
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

  /** Listener that runs all the given listeners. The first one is the outermost: it wraps the
    * execution as wrapped by the second one, and so on. */
  def combine[F[_]](listeners: ActionListener[F]*): ActionListener[F] =
    new ActionListener[F] {
      override def around[R, H](action: DBIOAction[R, _, _], exec: F[H]): F[H] =
        listeners.foldRight(exec)((l, e) => l.around(action, e))
    }

  private[this] lazy val timingLogger =
    new SlickLogger(LoggerFactory.getLogger("slick.basic.BasicBackend.timing"))

  /** Listener that logs how long each SQL action took, together with its SQL statements, to the
    * `slick.basic.BasicBackend.timing` logger at debug level, e.g.
    * `SQL action took 1.234 ms: select ...`. Failed and canceled actions are logged as
    * `failed after` and `was canceled after`. The time is measured around the whole action, so it
    * can include waiting for a connection. Nothing is measured unless that logger is enabled at debug level.
    *
    * The default listener of every backend already includes this one. Combine it with your own listener
    * to keep the timing log when you install one, e.g. `ActionListener.combine(myListener, ActionListener.timing[IO])`. */
  def timing[F[_]](implicit F: Sync[F]): ActionListener[F] =
    new ActionListener[F] {
      override def around[R, H](action: DBIOAction[R, _, _], exec: F[H]): F[H] = action match {
        case a: SqlAction[?, ?, ?] if timingLogger.isDebugEnabled =>
          F.monotonic.flatMap { t0 =>
            def log(outcome: String) = F.monotonic.flatMap { t1 =>
              F.delay(timingLogger.debug("SQL action " + outcome + " " +
                String.format(java.util.Locale.ROOT, "%.3f", Double.box((t1 - t0).toNanos / 1e6)) + " ms: " +
                a.statements.mkString("; ")))
            }
            exec.guaranteeCase {
              case Outcome.Succeeded(_) => log("took")
              case Outcome.Errored(_)   => log("failed after")
              case Outcome.Canceled()   => log("was canceled after")
            }
          }
        case _ => exec
      }
    }
}
