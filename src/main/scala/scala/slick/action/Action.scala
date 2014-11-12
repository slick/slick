package scala.slick.action

import scala.concurrent.{ExecutionContext, Future}
import scala.slick.SlickException
import scala.slick.backend.DatabaseComponent
import scala.slick.util.{DumpInfo, Dumpable}
import scala.util.control.NonFatal

/** Abstract type for Actions. Allows separation of execution logic and resource usage
  * management logic from composition logic.
  *
  * @tparam E Effect type
  * @tparam R Result type
  */
sealed trait Action[-E <: Effect, +R] extends Dumpable {
  /** Transform the result of a successful execution of this action. If this action fails, the
    * resulting action also fails. */
  def map[R2](f: R => R2)(implicit executor: ExecutionContext): Action[E, R2] =
    flatMap[E, R2](r => ConstantAction[R2](f(r)))

  /** Use the result produced by the successful execution of this action to compute and then
    * run the next action in sequence. The resulting action fails if either this action, the
    * computation, or the computed action fails. */
  def flatMap[E2 <: Effect, R2](f: R => Action[E2, R2])(implicit executor: ExecutionContext): Action[E with E2, R2] =
    FlatMapAction[E with E2, R2, R](this, f, executor)

  /** Run another action after this action, if it completed successfully, and return the result
    * of the second action. If either of the two actions fails, the resulting action also fails. */
  def andThen[E2 <: Effect, R2](a: Action[E2, R2]): Action[E with E2, R2] =
    AndThenAction[E with E2, R2](this, a)

  /** Run another action after this action, if it completed successfully, and return the result
    * of both actions. If either of the two actions fails, the resulting action also fails. */
  def zip[E2 <: Effect, R2](a: Action[E2, R2]): Action[E with E2, (R, R2)] =
    ZipAction[E with E2, R, R2](this, a)

  /** Run another action after this action, whether it succeeds or fails, and then return the
    * result of the first action. If the first action fails, its failure is propagated, whether
    * the second action fails or succeeds. If the first action succeeds, a failure of the second
    * action is propagated. */
  def andFinally[E2 <: Effect, R2](a: Action[E2, R2]): Action[E with E2, R] =
    AndFinallyAction[E with E2, R](this, a)

  /** A shortcut for @andThen. */
  final def >> [E2 <: Effect, R2](a: Action[E2, R2]): Action[E with E2, R2] =
    andThen[E2, R2](a)

  def withFilter(p: R => Boolean)(implicit executor: ExecutionContext): Action[E, R] =
    flatMap(v => if(p(v)) ConstantAction(v) else throw new SlickException("Action.withFilter failed"))

  /** Run this Action with a pinned database session. If this action is composed of multiple
    * database actions, they will all use the same session, even when sequenced with non-database
    * actions. For non-composite or non-database actions, this has no effect. */
  def withPinnedSession: Action[E, R] =
    (Action.Pin andThen this andFinally Action.Unpin).asInstanceOf[Action[E, R]]

  /** Whether or not this is a control flow action. These actions are not logged by default. */
  def isControlFlowAction = false

  /** Get the equivalent non-fused Action if this Action has been fused, otherwise this
    * Action is returned. */
  def nonFusedEquivalentAction: Action[E, R] = this
}

object Action {
  /** Convert a [[scala.concurrent.Future]] to an [[Action]]. */
  def from[R](f: Future[R]): Action[Effect, R] = FutureAction[R](f)

  /** Lift a constant value to an [[Action]]. */
  def const[R](v: R): Action[Effect, R] = ConstantAction[R](v)

  /** An Action that pins the current session */
  private object Pin extends SynchronousDatabaseAction[DatabaseComponent, Effect, Unit] {
    def run(context: ActionContext[DatabaseComponent]): Unit = context.pin
    def getDumpInfo = DumpInfo(name = "SynchronousDatabaseAction.Pin")
  }

  /** An Action that unpins the current session */
  private object Unpin extends SynchronousDatabaseAction[DatabaseComponent, Effect, Unit] {
    def run(context: ActionContext[DatabaseComponent]): Unit = context.unpin
    def getDumpInfo = DumpInfo(name = "SynchronousDatabaseAction.Unpin")
  }
}

/** An Action that represents a database operation. Concrete implementations are backend-specific
  * and therefore carry a `BackendType` effect. */
trait DatabaseAction[B <: DatabaseComponent, -E <: Effect, +R] extends Action[E with Effect.BackendType[B], R]

/** An Action that returns a constant value. */
case class ConstantAction[+R](value: R) extends Action[Effect, R] {
  def getDumpInfo = DumpInfo("const", String.valueOf(value))
}

/** An asynchronous Action that returns the result of a Future. */
case class FutureAction[+R](f: Future[R]) extends Action[Effect, R] {
  def getDumpInfo = DumpInfo("future", String.valueOf(f))
}

/** An Action that represents a `flatMap` operation for sequencing in the Action monad. */
case class FlatMapAction[-E <: Effect, +R, P](base: Action[E, P], f: P => Action[E, R], executor: ExecutionContext) extends Action[E, R] {
  def getDumpInfo = DumpInfo("flatMap", String.valueOf(f), children = Vector(("base", base)))
  override def isControlFlowAction = true
}

/** An Action that represents an `andThen` operation for sequencing in the Action monad. */
case class AndThenAction[-E <: Effect, +R](a1: Action[E, _], a2: Action[E, R]) extends Action[E, R] {
  def getDumpInfo = DumpInfo("andThen", children = Vector(("1", a1), ("2", a2)))
  override def isControlFlowAction = true
}

/** An Action that represents a `zip` operation for sequencing in the Action monad. */
case class ZipAction[-E <: Effect, +R1, +R2](a1: Action[E, R1], a2: Action[E, R2]) extends Action[E, (R1, R2)] {
  def getDumpInfo = DumpInfo("zip", children = Vector(("1", a1), ("2", a2)))
  override def isControlFlowAction = true
}

/** An Action that represents an `andFinally` operation for sequencing in the Action monad. */
case class AndFinallyAction[-E <: Effect, +R](a1: Action[E, R], a2: Action[E, _]) extends Action[E, R] {
  def getDumpInfo = DumpInfo("andFinally", children = Vector(("try", a1), ("finally", a2)))
  override def isControlFlowAction = true
}

/** The context object passed to database actions by the execution engine. */
trait ActionContext[+B <: DatabaseComponent] {
  private[this] var stickiness = 0

  /** Check if the session is pinned */
  final def isPinned = stickiness > 0

  /** Pin the current session. Multiple calls to `pin` may be nested. The same number of calls
    * to `unpin` is required in order to mark the session as not pinned anymore. A pinned
    * session will not be released at the end of a primitive database action. Instead, the same
    * pinned session is passed to all subsequent actions until it is unpinned. Note that pinning
    * does not force an actual database connection to be opened. This still happens on demand. */
  final def pin: Unit = stickiness += 1

  /** Unpin this session once. */
  final def unpin: Unit = stickiness -= 1

  /** Get the current database session for this context. Inside a single action this value will
    * never change. If the session has not been pinned, a subsequent action will see a different
    * session (unless the actions have been fused). */
  def session: B#Session
}

/** A synchronous database action provides a function from an `ActionContext` to the result
  * type. `DatabaseComponent.DatabaseDef.run` supports this kind of action out of the box
  * through `DatabaseComponent.DatabaseDef.runSynchronousDatabaseAction` so that `run` does not
  * need to be extended if all primitive database actions can be expressed in this way. These
  * actions also implement construction-time fusion for the @andThen, @andFinally, @zip and
  * @withPinnedSession operations. */
trait SynchronousDatabaseAction[B <: DatabaseComponent, -E <: Effect, +R] extends DatabaseAction[B, E, R] { self =>
  def run(context: ActionContext[B]): R

  private[this] def superAndThen[E2 <: Effect, R2](a: Action[E2, R2]) = super.andThen[E2, R2](a)
  override def andThen[E2 <: Effect, R2](a: Action[E2, R2]): Action[E with Effect.BackendType[B] with E2, R2] = a match {
    case a: SynchronousDatabaseAction[_, _, _] => new SynchronousDatabaseAction.Fused[B, E with E2, R2] {
      def run(context: ActionContext[B]): R2 = {
        self.run(context)
        a.asInstanceOf[SynchronousDatabaseAction[B, E2, R2]].run(context)
      }
      override def nonFusedEquivalentAction: Action[E with Effect.BackendType[B] with E2, R2] = superAndThen(a)
    }
    case a => superAndThen(a)
  }

  private[this] def superZip[E2 <: Effect, R2](a: Action[E2, R2]) = super.zip[E2, R2](a)
  override def zip[E2 <: Effect, R2](a: Action[E2, R2]): Action[E with Effect.BackendType[B] with E2, (R, R2)] = a match {
    case a: SynchronousDatabaseAction[_, _, _] => new SynchronousDatabaseAction.Fused[B, E with E2, (R, R2)] {
      def run(context: ActionContext[B]): (R, R2) = {
        val r1 = self.run(context)
        val r2 = a.asInstanceOf[SynchronousDatabaseAction[B, E2, R2]].run(context)
        (r1, r2)
      }
      override def nonFusedEquivalentAction: Action[E with Effect.BackendType[B] with E2, (R, R2)] = superZip(a)
    }
    case a => superZip(a)
  }

  private[this] def superAndFinally[E2 <: Effect, R2](a: Action[E2, R2]) = super.andFinally[E2, R2](a)
  override def andFinally[E2 <: Effect, R2](a: Action[E2, R2]): Action[E with Effect.BackendType[B] with E2, R] = a match {
    case a: SynchronousDatabaseAction[_, _, _] => new SynchronousDatabaseAction.Fused[B, E with E2, R] {
      def run(context: ActionContext[B]): R = {
        val res = try self.run(context) catch {
          case NonFatal(ex) =>
            try a.asInstanceOf[SynchronousDatabaseAction[B, E2, R2]].run(context) catch { case NonFatal(_) => }
            throw ex
        }
        a.asInstanceOf[SynchronousDatabaseAction[B, E2, R2]].run(context)
        res
      }
      override def nonFusedEquivalentAction: Action[E with Effect.BackendType[B] with E2, R] = superAndFinally(a)
    }
    case a => superAndFinally(a)
  }

  private[this] def superWithPinnedSession = super.withPinnedSession
  override def withPinnedSession: Action[E with Effect.BackendType[B], R] = new SynchronousDatabaseAction.Fused[B, E, R] {
    def run(context: ActionContext[B]): R = {
      context.pin
      val res = try self.run(context) catch {
        case NonFatal(ex) =>
          context.unpin
          throw ex
      }
      context.unpin
      res
    }
    override def nonFusedEquivalentAction = superWithPinnedSession
  }
}

object SynchronousDatabaseAction {
  /** A fused SynchronousDatabaseAction */
  trait Fused[B <: DatabaseComponent, -E <: Effect, +R] extends SynchronousDatabaseAction[B, E, R] {
    def getDumpInfo = DumpInfo(name = "SynchronousDatabaseAction.Fused", children = Vector(("non-fused", nonFusedEquivalentAction)))
  }
}

/** A phantom type for annotating database Actions with specific effects (e.g. `Write` or
  * `Transactional`). Effects can be composed through intersection types (e.g.
  * `Write with Transactional`. Backends declare all supported types to prevent execution
  * of unsupported features (e.g. [[scala.slick.memory.HeapBackend]] does not support
  * transactions). Further restrictions (like ensuring that all writes go to a master
  * database but reads can also be performed by a slave) can be enforced in user-level code. */
trait Effect

object Effect {
  /** Effect for Actions that read from the database ("DQL") */
  trait Read extends Effect
  /** Effect for Actions that write to the database ("DML") */
  trait Write extends Effect
  /** Effect for Actions that manipulate a database schema ("DDL") */
  trait Schema extends Effect
  /** Effect for transactional Actions ("DTL") */
  trait Transactional extends Effect
  /** This effect ties an Action to a specific backend. All primitive database Actions carry this
    * effect to prevent execution on the wrong backend. The generic Action combinators do not have
    * this effect because they must be supported by all backends. */
  trait BackendType[+B <: DatabaseComponent] extends Effect
}
