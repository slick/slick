package scala.slick.action

import scala.language.higherKinds

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.slick.SlickException
import scala.slick.backend.DatabaseComponent
import scala.slick.util.{DumpInfo, Dumpable}
import scala.util.{Try, Failure, Success}
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
    flatMap[E, R2](r => SuccessAction[R2](f(r)))

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

  /** A shortcut for `andThen`. */
  final def >> [E2 <: Effect, R2](a: Action[E2, R2]): Action[E with E2, R2] =
    andThen[E2, R2](a)

  /** Filter the result of this Action with the given predicate If the predicate matches, the
    * original result is returned, otherwise the resulting Action fails with a
    * NoSuchElementException. */
  final def filter(p: R => Boolean)(implicit executor: ExecutionContext): Action[E, R] =
    withFilter(p)

  def withFilter(p: R => Boolean)(implicit executor: ExecutionContext): Action[E, R] =
    flatMap(v => if(p(v)) SuccessAction(v) else throw new NoSuchElementException("Action.withFilter failed"))

  /** Return an Action which contains the Throwable with which this Action failed as its result.
    * If this Action succeeded, the resulting Action fails with a NoSuchElementException. */
  def failed: Action[E, Throwable] = FailedAction[E](this)

  /** Convert a successful result `v` of this Action into a successful result `Success(v)` and a
    * failure `t` into a successful result `Failure(t)` */
  def asTry: Action[E, Try[R]] = AsTryAction[E, R](this)

  /** Run this Action with a pinned database session. If this action is composed of multiple
    * database actions, they will all use the same session, even when sequenced with non-database
    * actions. For non-composite or non-database actions, this has no effect. */
  def withPinnedSession: Action[E, R] =
    (Action.Pin andThen this andFinally Action.Unpin).asInstanceOf[Action[E, R]]

  /** Get a wrapping Action which has a name that will be included in log output. */
  def named(name: String): Action[E, R] =
    NamedAction[E, R](this, name)

  /** Get the equivalent non-fused Action if this Action has been fused, otherwise this
    * Action is returned. */
  def nonFusedEquivalentAction: Action[E, R] = this

  /** Whether of not this action should be included in log output by default. */
  def isLogged: Boolean = false
}

object Action {
  /** Convert a [[scala.concurrent.Future]] to an [[Action]]. */
  def from[R](f: Future[R]): Action[Effect, R] = FutureAction[R](f)

  /** Lift a constant value to an [[Action]]. */
  def successful[R](v: R): Action[Effect, R] = SuccessAction[R](v)

  /** Create an [[Action]] that always fails. */
  def failed(t: Throwable): Action[Effect, Nothing] = FailureAction(t)

  /** Transform a `TraversableOnce[Action[E, R]]` into an `Action[E, TraversableOnce[R]]`. */
  def sequence[E <: Effect, R, M[_] <: TraversableOnce[_]](in: M[Action[E, R]])(implicit cbf: CanBuildFrom[M[Action[E, R]], R, M[R]]): Action[E, M[R]] = {
    implicit val ec = Action.sameThreadExecutionContext
    in.foldLeft(Action.successful(cbf(in)): Action[E, mutable.Builder[R, M[R]]]) { (ar, ae) =>
      for (r <- ar; e <- ae.asInstanceOf[Action[E, R]]) yield (r += e)
    } map (_.result)
  }

  /** A simpler version of `sequence` that takes a number of Actions with any return type as
    * varargs and returns an Action that performs the individual Actions in sequence (using
    * `andThen`), returning `()` in the end. */
  def seq[E <: Effect](actions: Action[E, _]*): Action[E, Unit] =
    (actions :+ Action.successful(())).reduceLeft(_ andThen _).asInstanceOf[Action[E, Unit]]

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

  /** An ExecutionContext used internally for executing plumbing operations during Action
    * composition. */
  private[slick] object sameThreadExecutionContext extends ExecutionContext {
    override def execute(runnable: Runnable): Unit = runnable.run()
    override def reportFailure(t: Throwable): Unit = throw t
  }
}

/** An Action that represents a database operation. Concrete implementations are backend-specific
  * and therefore carry a `BackendType` effect. */
trait DatabaseAction[B <: DatabaseComponent, -E <: Effect, +R] extends Action[E with Effect.BackendType[B], R] {
  override def isLogged = true
}

/** An Action that returns a constant value. */
case class SuccessAction[+R](value: R) extends Action[Effect, R] {
  def getDumpInfo = DumpInfo("success", String.valueOf(value))
}

/** An Action that fails. */
case class FailureAction(t: Throwable) extends Action[Effect, Nothing] {
  def getDumpInfo = DumpInfo("failure", String.valueOf(t))
}

/** An asynchronous Action that returns the result of a Future. */
case class FutureAction[+R](f: Future[R]) extends Action[Effect, R] {
  def getDumpInfo = DumpInfo("future", String.valueOf(f))
  override def isLogged = true
}

/** An Action that represents a `flatMap` operation for sequencing in the Action monad. */
case class FlatMapAction[-E <: Effect, +R, P](base: Action[E, P], f: P => Action[E, R], executor: ExecutionContext) extends Action[E, R] {
  def getDumpInfo = DumpInfo("flatMap", String.valueOf(f), children = Vector(("base", base)))
}

/** An Action that represents an `andThen` operation for sequencing in the Action monad. */
case class AndThenAction[-E <: Effect, +R](a1: Action[E, _], a2: Action[E, R]) extends Action[E, R] {
  def getDumpInfo = DumpInfo("andThen", children = Vector(("1", a1), ("2", a2)))
}

/** An Action that represents a `zip` operation for sequencing in the Action monad. */
case class ZipAction[-E <: Effect, +R1, +R2](a1: Action[E, R1], a2: Action[E, R2]) extends Action[E, (R1, R2)] {
  def getDumpInfo = DumpInfo("zip", children = Vector(("1", a1), ("2", a2)))
}

/** An Action that represents an `andFinally` operation for sequencing in the Action monad. */
case class AndFinallyAction[-E <: Effect, +R](a1: Action[E, R], a2: Action[E, _]) extends Action[E, R] {
  def getDumpInfo = DumpInfo("andFinally", children = Vector(("try", a1), ("finally", a2)))
}

/** An Action that represents a `failed` operation. */
case class FailedAction[-E <: Effect](a: Action[E, _]) extends Action[E, Throwable] {
  def getDumpInfo = DumpInfo("failed", children = Vector(("base", a)))
}

/** An Action that represents an `asTry` operation. */
case class AsTryAction[-E <: Effect, +R](a: Action[E, R]) extends Action[E, Try[R]] {
  def getDumpInfo = DumpInfo("asTry")
}

/** An Action that attaches a name for logging purposes to another action. */
case class NamedAction[-E <: Effect, +R](a: Action[E, R], name: String) extends Action[E, R] {
  def getDumpInfo = DumpInfo("named", mainInfo = DumpInfo.highlight(name))
  override def isLogged = true
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
  * actions also implement construction-time fusion for the `andThen`, `andFinally`, `zip`,
  * `failed`, `asTry` and `withPinnedSession` operations. */
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

  private[this] def superFailed: Action[E with Effect.BackendType[B], Throwable] = super.failed
  override def failed: Action[E with Effect.BackendType[B], Throwable] = new SynchronousDatabaseAction.Fused[B, E, Throwable] {
    def run(context: ActionContext[B]): Throwable = {
      var ok = false
      try {
        self.run(context)
        ok = true
        throw new NoSuchElementException("Action.failed (fused) not completed with a Throwable")
      } catch {
        case NonFatal(ex) if !ok => ex
      }
    }
    override def nonFusedEquivalentAction = superFailed
  }

  private[this] def superAsTry: Action[E with Effect.BackendType[B], Try[R]] = super.asTry
  override def asTry: Action[E with Effect.BackendType[B], Try[R]] = new SynchronousDatabaseAction.Fused[B, E, Try[R]] {
    def run(context: ActionContext[B]): Try[R] = {
      try Success(self.run(context)) catch {
        case NonFatal(ex) => Failure(ex)
      }
    }
    override def nonFusedEquivalentAction = superAsTry
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
