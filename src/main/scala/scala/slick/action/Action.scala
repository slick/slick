package scala.slick.action

import scala.language.higherKinds

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.slick.SlickException
import scala.slick.backend.DatabaseComponent
import scala.slick.util.{DumpInfo, Dumpable, ignoreFollowOnError}
import scala.util.{Try, Failure, Success}
import scala.util.control.NonFatal

/** An Action that can be executed on a database. The Action type allows a separation of execution
  * logic and resource usage management logic from composition logic. Actions can be composed with
  * methods such as `andThen`, `andFinally` and `flatMap`. Individual parts of a composite Action
  * are always executed serially on a single database, but possibly in different database sessions,
  * unless the session is pinned either explicitly (using `withPinnedSession`) or implicitly (e.g.
  * through a transaction).
  *
  * @tparam E The Action's effect type, e.g. `Effect.Read with Effect.Write`. When composing
  *           Actions, the correct combined effect type will be inferred. Effects are used to tie
  *           an Action to a specific back-end type and they can also be used in user code, e.g.
  *           to automatically direct all read-only Actions to a slave database and write Actions
  *           to the master copy.
  * @tparam R The result type when executing the Action and fully materializing the result.
  * @tparam S An encoding of the result type for streaming results. If this action is capable of
  *           streaming, it is `Streaming[T]` for an element type `T`. For non-streaming
  *           Actions it is `NoStream`.
  */
sealed trait Action[-E <: Effect, +R, +S <: NoStream] extends Dumpable {
  /** Transform the result of a successful execution of this action. If this action fails, the
    * resulting action also fails. */
  def map[R2](f: R => R2)(implicit executor: ExecutionContext): Action[E, R2, NoStream] =
    flatMap[E, R2, NoStream](r => SuccessAction[R2](f(r)))

  /** Use the result produced by the successful execution of this action to compute and then
    * run the next action in sequence. The resulting action fails if either this action, the
    * computation, or the computed action fails. */
  def flatMap[E2 <: Effect, R2, S2 <: NoStream](f: R => Action[E2, R2, S2])(implicit executor: ExecutionContext): Action[E with E2, R2, S2] =
    FlatMapAction[E with E2, R2, S2, R](this, f, executor)

  /** Run another action after this action, if it completed successfully, and return the result
    * of the second action. If either of the two actions fails, the resulting action also fails. */
  def andThen[E2 <: Effect, R2, S2 <: NoStream](a: Action[E2, R2, S2]): Action[E with E2, R2, S2] =
    AndThenAction[E with E2, R2, S2](this, a)

  /** Run another action after this action, if it completed successfully, and return the result
    * of both actions. If either of the two actions fails, the resulting action also fails. */
  def zip[E2 <: Effect, R2](a: Action[E2, R2, NoStream]): Action[E with E2, (R, R2), NoStream] =
    ZipAction[E with E2, R, R2](this, a)

  /** Run another action after this action, whether it succeeds or fails, and then return the
    * result of the first action. If the first action fails, its failure is propagated, whether
    * the second action fails or succeeds. If the first action succeeds, a failure of the second
    * action is propagated. */
  def andFinally[E2 <: Effect](a: Action[E2, _, NoStream]): Action[E with E2, R, S] =
    cleanUp[E2](_ => a)(Action.sameThreadExecutionContext)

  /** Run another action after this action, whether it succeeds or fails, in order to clean up or
    * transform an error produced by this action. The clean-up action is computed from the failure
    * of this action, wrapped in `Some`, or `None` if this action succeeded.
    *
    * @param keepFailure If this action returns successfully, the resulting action also returns
    *                    successfully unless the clean-up action fails. If this action fails and
    *                    `keepFailure` is set to `true` (the default), the resulting action fails
    *                    with the same error, no matter whether the clean-up action succeeds or
    *                    fails. If the clean-up action. If `keepFailure` is set to `false`, an
    *                    error from the clean-up action will override the error from this action. */
  def cleanUp[E2 <: Effect](f: Option[Throwable] => Action[E2, _, NoStream], keepFailure: Boolean = true)(implicit executor: ExecutionContext): Action[E with E2, R, S] =
    CleanUpAction[E with E2, R, S](this, f, keepFailure, executor)

  /** A shortcut for `andThen`. */
  final def >> [E2 <: Effect, R2, S2 <: NoStream](a: Action[E2, R2, S2]): Action[E with E2, R2, S2] =
    andThen[E2, R2, S2](a)

  /** Filter the result of this Action with the given predicate. If the predicate matches, the
    * original result is returned, otherwise the resulting Action fails with a
    * NoSuchElementException. */
  final def filter(p: R => Boolean)(implicit executor: ExecutionContext): Action[E, R, NoStream] =
    withFilter(p)

  def withFilter(p: R => Boolean)(implicit executor: ExecutionContext): Action[E, R, NoStream] =
    flatMap(v => if(p(v)) SuccessAction(v) else throw new NoSuchElementException("Action.withFilter failed"))

  /** Return an Action which contains the Throwable with which this Action failed as its result.
    * If this Action succeeded, the resulting Action fails with a NoSuchElementException. */
  def failed: Action[E, Throwable, NoStream] = FailedAction[E](this)

  /** Convert a successful result `v` of this Action into a successful result `Success(v)` and a
    * failure `t` into a successful result `Failure(t)` */
  def asTry: Action[E, Try[R], NoStream] = AsTryAction[E, R](this)

  /** Run this Action with a pinned database session. If this action is composed of multiple
    * database actions, they will all use the same session, even when sequenced with non-database
    * actions. For non-composite or non-database actions, this has no effect. */
  def withPinnedSession: Action[E, R, S] =
    (Action.Pin andThen this andFinally Action.Unpin).asInstanceOf[Action[E, R, S]]

  /** Get a wrapping Action which has a name that will be included in log output. */
  def named(name: String): Action[E, R, S] =
    NamedAction[E, R, S](this, name)

  /** Get the equivalent non-fused Action if this Action has been fused, otherwise this
    * Action is returned. */
  def nonFusedEquivalentAction: Action[E, R, S] = this

  /** Whether or not this Action should be included in log output by default. */
  def isLogged: Boolean = false
}

object Action {
  /** Convert a `Future` to an [[Action]]. */
  def from[R](f: Future[R]): Action[Effect, R, NoStream] = FutureAction[R](f)

  /** Lift a constant value to an [[Action]]. */
  def successful[R](v: R): Action[Effect, R, NoStream] = SuccessAction[R](v)

  /** Create an [[Action]] that always fails. */
  def failed(t: Throwable): Action[Effect, Nothing, NoStream] = FailureAction(t)

  /** Transform a `TraversableOnce[Action[E, R, NoStream]]` into an `Action[E, TraversableOnce[R], NoStream]`. */
  def sequence[E <: Effect, R, M[_] <: TraversableOnce[_]](in: M[Action[E, R, NoStream]])(implicit cbf: CanBuildFrom[M[Action[E, R, NoStream]], R, M[R]]): Action[E, M[R], NoStream] = {
    implicit val ec = Action.sameThreadExecutionContext
    in.foldLeft(Action.successful(cbf(in)): Action[E, mutable.Builder[R, M[R]], NoStream]) { (ar, ae) =>
      for (r <- ar; e <- ae.asInstanceOf[Action[E, R, NoStream]]) yield (r += e)
    } map (_.result)
  }

  /** A simpler version of `sequence` that takes a number of Actions with any return type as
    * varargs and returns an Action that performs the individual Actions in sequence (using
    * `andThen`), returning `()` in the end. */
  def seq[E <: Effect](actions: Action[E, _, NoStream]*): Action[E, Unit, NoStream] =
    (actions :+ Action.successful(())).reduceLeft(_ andThen _).asInstanceOf[Action[E, Unit, NoStream]]

  /** An Action that pins the current session */
  private[slick] object Pin extends SynchronousDatabaseAction[DatabaseComponent, Effect, Unit, NoStream] {
    def run(context: ActionContext[DatabaseComponent]): Unit = context.pin
    def getDumpInfo = DumpInfo(name = "SynchronousDatabaseAction.Pin")
  }

  /** An Action that unpins the current session */
  private[slick] object Unpin extends SynchronousDatabaseAction[DatabaseComponent, Effect, Unit, NoStream] {
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
trait DatabaseAction[B <: DatabaseComponent, -E <: Effect, +R, +S <: NoStream] extends Action[E with Effect.BackendType[B], R, S] {
  override def isLogged = true
}

/** An Action that returns a constant value. */
case class SuccessAction[+R](value: R) extends Action[Effect, R, NoStream] {
  def getDumpInfo = DumpInfo("success", String.valueOf(value))
}

/** An Action that fails. */
case class FailureAction(t: Throwable) extends Action[Effect, Nothing, NoStream] {
  def getDumpInfo = DumpInfo("failure", String.valueOf(t))
}

/** An asynchronous Action that returns the result of a Future. */
case class FutureAction[+R](f: Future[R]) extends Action[Effect, R, NoStream] {
  def getDumpInfo = DumpInfo("future", String.valueOf(f))
  override def isLogged = true
}

/** An Action that represents a `flatMap` operation for sequencing in the Action monad. */
case class FlatMapAction[-E <: Effect, +R, +S <: NoStream, P](base: Action[E, P, NoStream], f: P => Action[E, R, S], executor: ExecutionContext) extends Action[E, R, S] {
  def getDumpInfo = DumpInfo("flatMap", String.valueOf(f), children = Vector(("base", base)))
}

/** An Action that represents an `andThen` operation for sequencing in the Action monad. */
case class AndThenAction[-E <: Effect, +R, +S <: NoStream](a1: Action[E, _, NoStream], a2: Action[E, R, S]) extends Action[E, R, S] {
  def getDumpInfo = DumpInfo("andThen", children = Vector(("1", a1), ("2", a2)))
}

/** An Action that represents a `zip` operation for sequencing in the Action monad. */
case class ZipAction[-E <: Effect, +R1, +R2](a1: Action[E, R1, NoStream], a2: Action[E, R2, NoStream]) extends Action[E, (R1, R2), NoStream] {
  def getDumpInfo = DumpInfo("zip", children = Vector(("1", a1), ("2", a2)))
}

/** An Action that represents a `cleanUp` operation for sequencing in the Action monad. */
case class CleanUpAction[-E <: Effect, +R, +S <: NoStream](base: Action[E, R, S], f: Option[Throwable] => Action[E, _, NoStream], keepFailure: Boolean, executor: ExecutionContext) extends Action[E, R, S] {
  def getDumpInfo = DumpInfo("cleanUp", children = Vector(("try", base)))
}

/** An Action that represents a `failed` operation. */
case class FailedAction[-E <: Effect](a: Action[E, _, NoStream]) extends Action[E, Throwable, NoStream] {
  def getDumpInfo = DumpInfo("failed", children = Vector(("base", a)))
}

/** An Action that represents an `asTry` operation. */
case class AsTryAction[-E <: Effect, +R](a: Action[E, R, NoStream]) extends Action[E, Try[R], NoStream] {
  def getDumpInfo = DumpInfo("asTry")
}

/** An Action that attaches a name for logging purposes to another action. */
case class NamedAction[-E <: Effect, +R, +S <: NoStream](a: Action[E, R, S], name: String) extends Action[E, R, S] {
  def getDumpInfo = DumpInfo("named", mainInfo = DumpInfo.highlight(name))
  override def isLogged = true
}

/** The context object passed to database actions by the execution engine. */
trait ActionContext[+B <: DatabaseComponent] {
  private[this] var stickiness = 0

  /** Check if the session is pinned. May only be called from a synchronous action context. */
  final def isPinned = stickiness > 0

  /** Pin the current session. Multiple calls to `pin` may be nested. The same number of calls
    * to `unpin` is required in order to mark the session as not pinned anymore. A pinned
    * session will not be released at the end of a primitive database action. Instead, the same
    * pinned session is passed to all subsequent actions until it is unpinned. Note that pinning
    * does not force an actual database connection to be opened. This still happens on demand.
    * May only be called from a synchronous action context. */
  final def pin: Unit = stickiness += 1

  /** Unpin this session once. May only be called from a synchronous action context. */
  final def unpin: Unit = stickiness -= 1

  /** Get the current database session for this context. Inside a single action this value will
    * never change. If the session has not been pinned, a subsequent action will see a different
    * session (unless the actions have been fused). May only be called from a synchronous action
    * context. */
  def session: B#Session
}

/** An ActionContext with extra functionality required for streaming Actions. */
trait StreamingActionContext[+B <: DatabaseComponent] extends ActionContext[B] {
  /** Emit a single result of the stream. Any Exception thrown by this method should be passed on
    * to the caller. */
  def emit(v: Any)
}

/** A synchronous database action provides a function from an `ActionContext` to the result
  * type. `DatabaseComponent.DatabaseDef.run` supports this kind of action out of the box
  * through `DatabaseComponent.DatabaseDef.runSynchronousDatabaseAction` so that `run` does not
  * need to be extended if all primitive database actions can be expressed in this way. These
  * actions also implement construction-time fusion for the `andFinally`, `andThen`, `asTry`,
  * `failed`, `withPinnedSession` and `zip` operations.
  *
  * The execution engine ensures that an [[ActionContext]] is never used concurrently and that
  * all state changes performed by one invocation of a SynchronousDatabaseAction are visible
  * to the next invocation of the same or a different SynchronousDatabaseAction. */
trait SynchronousDatabaseAction[B <: DatabaseComponent, -E <: Effect, +R, +S <: NoStream] extends DatabaseAction[B, E, R, S] { self =>
  /** The type used by this action for the state of a suspended stream. A call to `emitStream`
    * produces such a state which is then fed back into the next call. */
  type StreamState >: Null <: AnyRef

  /** Run this action synchronously and produce a result, or throw an Exception to indicate a
    * failure. */
  def run(context: ActionContext[B]): R

  /** Run this action synchronously and emit results to the context. This methods may throw an
    * Exception to indicate a failure.
    *
    * @param limit The maximum number of results to emit, or Long.MaxValue for no limit.
    * @param state The state returned by a previous invocation of this method, or `null` if
    *             a new stream should be produced.
    * @return A stream state if there are potentially more results available, or null if the
    *         stream is finished. */
  def emitStream(context: StreamingActionContext[B], limit: Long, state: StreamState): StreamState =
    throw new SlickException("Internal error: Streaming is not supported by this Action")

  /** Dispose of a `StreamState` when a streaming action is cancelled. Whenever `emitStream`
    * returns `null` or throws an Exception, it needs to dispose of the state itself. This
    * method will not be called in these cases. */
  def cancelStream(context: StreamingActionContext[B], state: StreamState): Unit = ()

  /** Whether or not this Action supports streaming results. An Action with a `Streaming` result
    * type must either support streaming directly or have a [[nonFusedEquivalentAction]] which
    * supports streaming. This flag is ignored if the Action has a `NoStream` result type. */
  def supportsStreaming: Boolean = true

  private[this] def superAndThen[E2 <: Effect, R2, S2 <: NoStream](a: Action[E2, R2, S2]) = super.andThen[E2, R2, S2](a)
  override def andThen[E2 <: Effect, R2, S2 <: NoStream](a: Action[E2, R2, S2]): Action[E with Effect.BackendType[B] with E2, R2, S2] = a match {
    case a: SynchronousDatabaseAction[_, _, _, _] => new SynchronousDatabaseAction.Fused[B, E with E2, R2, S2] {
      def run(context: ActionContext[B]): R2 = {
        self.run(context)
        a.asInstanceOf[SynchronousDatabaseAction[B, E2, R2, S2]].run(context)
      }
      override def nonFusedEquivalentAction: Action[E with Effect.BackendType[B] with E2, R2, S2] = superAndThen(a)
    }
    case a => superAndThen(a)
  }

  private[this] def superZip[E2 <: Effect, R2](a: Action[E2, R2, NoStream]) = super.zip[E2, R2](a)
  override def zip[E2 <: Effect, R2](a: Action[E2, R2, NoStream]): Action[E with Effect.BackendType[B] with E2, (R, R2), NoStream] = a match {
    case a: SynchronousDatabaseAction[_, _, _, _] => new SynchronousDatabaseAction.Fused[B, E with E2, (R, R2), NoStream] {
      def run(context: ActionContext[B]): (R, R2) = {
        val r1 = self.run(context)
        val r2 = a.asInstanceOf[SynchronousDatabaseAction[B, E2, R2, NoStream]].run(context)
        (r1, r2)
      }
      override def nonFusedEquivalentAction: Action[E with Effect.BackendType[B] with E2, (R, R2), NoStream] = superZip(a)
    }
    case a => superZip(a)
  }

  private[this] def superAndFinally[E2 <: Effect](a: Action[E2, _, NoStream]) = super.andFinally[E2](a)
  override def andFinally[E2 <: Effect](a: Action[E2, _, NoStream]): Action[E with Effect.BackendType[B] with E2, R, S] = a match {
    case a: SynchronousDatabaseAction[_, _, _, _] => new SynchronousDatabaseAction.Fused[B, E with E2, R, S] {
      def run(context: ActionContext[B]): R = {
        val res = try self.run(context) catch {
          case NonFatal(ex) =>
            try a.asInstanceOf[SynchronousDatabaseAction[B, E2, Any, NoStream]].run(context) catch ignoreFollowOnError
            throw ex
        }
        a.asInstanceOf[SynchronousDatabaseAction[B, E2, Any, S]].run(context)
        res
      }
      override def nonFusedEquivalentAction: Action[E with Effect.BackendType[B] with E2, R, S] = superAndFinally(a)
    }
    case a => superAndFinally(a)
  }

  private[this] def superWithPinnedSession = super.withPinnedSession
  override def withPinnedSession: Action[E with Effect.BackendType[B], R, S] = new SynchronousDatabaseAction.Fused[B, E, R, S] {
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

  private[this] def superFailed: Action[E with Effect.BackendType[B], Throwable, NoStream] = super.failed
  override def failed: Action[E with Effect.BackendType[B], Throwable, NoStream] = new SynchronousDatabaseAction.Fused[B, E, Throwable, NoStream] {
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

  private[this] def superAsTry: Action[E with Effect.BackendType[B], Try[R], NoStream] = super.asTry
  override def asTry: Action[E with Effect.BackendType[B], Try[R], NoStream] = new SynchronousDatabaseAction.Fused[B, E, Try[R], NoStream] {
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
  trait Fused[B <: DatabaseComponent, -E <: Effect, +R, +S <: NoStream] extends SynchronousDatabaseAction[B, E, R, S] {
    def getDumpInfo = DumpInfo(name = "SynchronousDatabaseAction.Fused", children = Vector(("non-fused", nonFusedEquivalentAction)))
    override def supportsStreaming: Boolean = false
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

/** A phantom type used as the streaming result type for Actions that do not support streaming.
  * Note that this is a supertype of `Streaming` (and it is used in covariant position),
  * so that any streaming Action can be used where a non-streaming Action is expected. */
sealed trait NoStream

/** A phantom type used as the streaming result type for Actions that do support streaming. */
sealed trait Streaming[+T] extends NoStream
