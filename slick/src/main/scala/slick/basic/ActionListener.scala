package slick.basic

import scala.annotation.tailrec

import cats.effect.Sync
import cats.effect.kernel.Outcome
import cats.effect.syntax.all.*
import cats.syntax.all.*
import org.slf4j.LoggerFactory

import slick.dbio.*
import slick.sql.SqlAction
import slick.util.SlickLogger

/** Listener hook for tracing and instrumentation around DBIO node execution.
  *
  * The `exec` effect represents the original execution of `action`. Implementations can run
  * effectful logic before/after `exec` (for example start/finish spans, timing, logging), but
  * they cannot inspect or alter the `H` value itself.
  *
  * Synchronous actions that are combined with `>>`, `DBIO.seq`, `zip`, `asTry` and similar combinators
  * are fused into one `SynchronousDatabaseAction.Fused` action, which is passed to `around` as one node.
  * For `db.stream`, `exec` of the action that produces the rows covers opening the stream, not reading the rows.
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

  /** Wrap the opening of a stream by `db.stream`: `exec` runs `action`, which produces the rows of
    * the stream, until its iterator is returned. This is called for the action passed to `db.stream`
    * and for each action inside it on the way to the one that opens the stream (e.g. the action wrapped by
    * `transactionally`, or the parts of a fused action, which are run separately when streaming).
    * The default calls [[around]]; `timing` overrides it to log the time to open the stream once, for
    * the SQL action that opens it. */
  private[slick] def aroundStreamOpen[R, H](action: DBIOAction[R, _, _], exec: F[H]): F[H] = around(action, exec)
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
      override private[slick] def aroundStreamOpen[R, H](action: DBIOAction[R, _, _], exec: F[H]): F[H] =
        listeners.foldRight(exec)((l, e) => l.aroundStreamOpen(action, e))
    }

  private[this] lazy val timingLogger =
    new SlickLogger(LoggerFactory.getLogger("slick.basic.BasicBackend.timing"))

  /** The SQL statements of the SQL actions in `action`, in execution order. Actions that are only
    * computed while running (in `flatMap` continuations and `cleanUp` / `andFinally` handlers) are
    * not known in advance and therefore not included. */
  private[slick] def sqlStatements(action: DBIOAction[Any, NoStream, Nothing]): List[String] = {
    // Tail recursive over a worklist, so that deeply nested actions (e.g. DBIO.seq) cannot overflow the stack.
    // The statements are accumulated in reverse and reversed once at the end.
    @tailrec def go(pending: List[DBIOAction[Any, NoStream, Nothing]], statements: List[String]): List[String] =
      pending match {
        case Nil => statements
        case next :: rest =>
          next match {
            case a: SqlAction[?, ?, ?]                          => go(rest, a.statements.foldLeft(statements)((acc, s) => s :: acc))
            case a: SynchronousDatabaseAction.Fused[?, ?, ?, ?] => go(a.nonFusedEquivalentAction :: rest, statements)
            case AndThenAction(as)                              => go(as.toList ::: rest, statements)
            case SequenceAction(as)                             => go(as.toList ::: rest, statements)
            case FlatMapAction(base, _)                         => go(base :: rest, statements)
            case CleanUpAction(base, _, _)                      => go(base :: rest, statements)
            case FailedAction(a)                                => go(a :: rest, statements)
            case AsTryAction(a)                                 => go(a :: rest, statements)
            case NamedAction(a, _)                              => go(a :: rest, statements)
            case PinnedSessionAction(a)                         => go(a :: rest, statements)
            case TransactionalAction(a, _)                      => go(a :: rest, statements)
            case _                                              => go(rest, statements)
          }
      }

    go(action :: Nil, Nil).reverse
  }

  /** Listener that logs how long each SQL action took, together with its SQL statements, to the
    * `slick.basic.BasicBackend.timing` logger at debug level, e.g.
    * `SQL action took 1.234 ms: select ...`. Failed and canceled actions are logged as
    * `failed after` and `was canceled after`. The time is measured around the whole action, so it
    * can include waiting for a connection. Nothing is measured unless that logger is enabled at debug level.
    *
    * Slick fuses synchronous actions that are combined with `>>`, `DBIO.seq`, `zip`, `asTry` and similar
    * combinators into one action that runs in one step. Such an action is logged on one line with its total
    * time and the statements of all SQL actions in it, joined with `; `.
    *
    * For `db.stream`, the line `SQL stream opened after ...` covers opening the stream, that is running the
    * query until the first rows can be read, but not reading the rows.
    *
    * The default listener of every backend already includes this one. Combine it with your own listener
    * to keep the timing log when you install one, e.g. `ActionListener.combine(myListener, ActionListener.timing[IO])`. */
  def timing[F[_]](implicit F: Sync[F]): ActionListener[F] =
    new ActionListener[F] {
      /** Time `exec` and log e.g. `SQL <what> <succeeded> 1.234 ms: <statements>`. */
      private[this] def timed[H](what: String, succeeded: String, statements: Iterable[String], exec: F[H]): F[H] =
        F.monotonic.flatMap { t0 =>
          def log(outcome: String) = F.monotonic.flatMap { t1 =>
            F.delay(timingLogger.debug("SQL " + what + " " + outcome + " " +
              String.format(java.util.Locale.ROOT, "%.3f", Double.box((t1 - t0).toNanos / 1e6)) + " ms: " +
              statements.mkString("; ")))
          }
          exec.guaranteeCase {
            case Outcome.Succeeded(_) => log(succeeded)
            case Outcome.Errored(_)   => log("failed after")
            case Outcome.Canceled()   => log("was canceled after")
          }
        }

      override def around[R, H](action: DBIOAction[R, _, _], exec: F[H]): F[H] =
        if (!timingLogger.isDebugEnabled) exec
        else action match {
          case a: SqlAction[?, ?, ?] => timed("action", "took", a.statements, exec)
          case a: SynchronousDatabaseAction.Fused[?, ?, ?, ?] =>
            // A fused action runs its parts in one step, so it is timed as one unit. To time the parts
            // separately, add a switch that disables fusing, so that each part runs as its own action
            // through `around`, rather than timing inside the fused run.
            val statements = sqlStatements(a)
            if (statements.isEmpty) exec else timed("action", "took", statements, exec)
          case _ => exec
        }

      override private[slick] def aroundStreamOpen[R, H](action: DBIOAction[R, _, _], exec: F[H]): F[H] =
        action match {
          case a: SqlAction[?, ?, ?] if timingLogger.isDebugEnabled => timed("stream", "opened after", a.statements, exec)
          case _ => exec
        }
    }
}
