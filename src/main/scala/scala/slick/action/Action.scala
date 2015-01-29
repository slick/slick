package scala.slick.action

import org.reactivestreams.Subscription

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
  * The actual implementation base type for all Actions is `EffectfulAction`. `StreamingAction` and
  * `Action` are type aliases which discard the effect type (and the streaming result type in the
  * latter case) to make Action types easier to write when these features are not needed. All
  * primitive Actions and all Actions produced by the standard combinators in Slick have correct
  * Effect types and are streaming (if possible).
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
sealed trait EffectfulAction[-E <: Effect, +R, +S <: NoStream] extends Dumpable {
  /** Transform the result of a successful execution of this action. If this action fails, the
    * resulting action also fails. */
  def map[R2](f: R => R2)(implicit executor: ExecutionContext): EffectfulAction[E, R2, NoStream] =
    flatMap[E, R2, NoStream](r => SuccessAction[R2](f(r)))

  /** Use the result produced by the successful execution of this action to compute and then
    * run the next action in sequence. The resulting action fails if either this action, the
    * computation, or the computed action fails. */
  def flatMap[E2 <: Effect, R2, S2 <: NoStream](f: R => EffectfulAction[E2, R2, S2])(implicit executor: ExecutionContext): EffectfulAction[E with E2, R2, S2] =
    FlatMapAction[E with E2, R2, S2, R](this, f, executor)

  /** Run another action after this action, if it completed successfully, and return the result
    * of the second action. If either of the two actions fails, the resulting action also fails. */
  def andThen[E2 <: Effect, R2, S2 <: NoStream](a: EffectfulAction[E2, R2, S2]): EffectfulAction[E with E2, R2, S2] =
    AndThenAction[E with E2, R2, S2](this, a)

  /** Run another action after this action, if it completed successfully, and return the result
    * of both actions. If either of the two actions fails, the resulting action also fails. */
  def zip[E2 <: Effect, R2](a: EffectfulAction[E2, R2, NoStream]): EffectfulAction[E with E2, (R, R2), NoStream] =
    ZipAction[E with E2, R, R2](this, a)

  /** Run another action after this action, whether it succeeds or fails, and then return the
    * result of the first action. If the first action fails, its failure is propagated, whether
    * the second action fails or succeeds. If the first action succeeds, a failure of the second
    * action is propagated. */
  def andFinally[E2 <: Effect](a: EffectfulAction[E2, _, NoStream]): EffectfulAction[E with E2, R, S] =
    cleanUp[E2](_ => a)(Action.sameThreadExecutionContext)

  /** Run another action after this action, whether it succeeds or fails, in order to clean up or
    * transform an error produced by this action. The clean-up action is computed from the failure
    * of this action, wrapped in `Some`, or `None` if this action succeeded.
    *
    * @param keepFailure If this action returns successfully, the resulting action also returns
    *                    successfully unless the clean-up action fails. If this action fails and
    *                    `keepFailure` is set to `true` (the default), the resulting action fails
    *                    with the same error, no matter whether the clean-up action succeeds or
    *                    fails. If `keepFailure` is set to `false`, an error from the clean-up
    *                    action will override the error from this action. */
  def cleanUp[E2 <: Effect](f: Option[Throwable] => EffectfulAction[E2, _, NoStream], keepFailure: Boolean = true)(implicit executor: ExecutionContext): EffectfulAction[E with E2, R, S] =
    CleanUpAction[E with E2, R, S](this, f, keepFailure, executor)

  /** A shortcut for `andThen`. */
  final def >> [E2 <: Effect, R2, S2 <: NoStream](a: EffectfulAction[E2, R2, S2]): EffectfulAction[E with E2, R2, S2] =
    andThen[E2, R2, S2](a)

  /** Filter the result of this Action with the given predicate. If the predicate matches, the
    * original result is returned, otherwise the resulting Action fails with a
    * NoSuchElementException. */
  final def filter(p: R => Boolean)(implicit executor: ExecutionContext): EffectfulAction[E, R, NoStream] =
    withFilter(p)

  def withFilter(p: R => Boolean)(implicit executor: ExecutionContext): EffectfulAction[E, R, NoStream] =
    flatMap(v => if(p(v)) SuccessAction(v) else throw new NoSuchElementException("Action.withFilter failed"))

  /** Return an Action which contains the Throwable with which this Action failed as its result.
    * If this Action succeeded, the resulting Action fails with a NoSuchElementException. */
  def failed: EffectfulAction[E, Throwable, NoStream] = FailedAction[E](this)

  /** Convert a successful result `v` of this Action into a successful result `Success(v)` and a
    * failure `t` into a successful result `Failure(t)`. This is the most generic combinator that
    * can be used for error recovery. If possible, use [[andFinally]] or [[cleanUp]] instead,
    * because those combinators, unlike `asTry`, support streaming. */
  def asTry: EffectfulAction[E, Try[R], NoStream] = AsTryAction[E, R](this)

  /** Run this Action with a pinned database session. If this action is composed of multiple
    * database actions, they will all use the same session, even when sequenced with non-database
    * actions. For non-composite or non-database actions, this has no effect. */
  def withPinnedSession: EffectfulAction[E, R, S] = Action.Pin andThen this andFinally Action.Unpin

  /** Get a wrapping Action which has a name that will be included in log output. */
  def named(name: String): EffectfulAction[E, R, S] =
    NamedAction[E, R, S](this, name)

  /** Get the equivalent non-fused Action if this Action has been fused, otherwise this
    * Action is returned. */
  def nonFusedEquivalentAction: EffectfulAction[E, R, S] = this

  /** Whether or not this Action should be included in log output by default. */
  def isLogged: Boolean = false
}

object Action {
  /** Convert a `Future` to an [[Action]]. */
  def from[R](f: Future[R]): EffectfulAction[Effect, R, NoStream] = FutureAction[R](f)

  /** Lift a constant value to an [[Action]]. */
  def successful[R](v: R): EffectfulAction[Effect, R, NoStream] = SuccessAction[R](v)

  /** Create an [[Action]] that always fails. */
  def failed(t: Throwable): EffectfulAction[Effect, Nothing, NoStream] = FailureAction(t)

  /** Transform a `TraversableOnce[EffectfulAction[E, R, NoStream]]` into an `EffectfulAction[E, TraversableOnce[R], NoStream]`. */
  def sequence[E <: Effect, R, M[+_] <: TraversableOnce[_]](in: M[EffectfulAction[E, R, NoStream]])(implicit cbf: CanBuildFrom[M[EffectfulAction[E, R, NoStream]], R, M[R]]): EffectfulAction[E, M[R], NoStream] = {
    implicit val ec = Action.sameThreadExecutionContext
    in.foldLeft(Action.successful(cbf(in)): EffectfulAction[E, mutable.Builder[R, M[R]], NoStream]) { (ar, ae) =>
      for (r <- ar; e <- ae.asInstanceOf[EffectfulAction[E, R, NoStream]]) yield (r += e)
    } map (_.result)
  }

  /** A simpler version of `sequence` that takes a number of Actions with any return type as
    * varargs and returns an Action that performs the individual Actions in sequence (using
    * `andThen`), returning `()` in the end. */
  def seq[E <: Effect](actions: EffectfulAction[E, _, NoStream]*): EffectfulAction[E, Unit, NoStream] =
    (actions :+ SuccessAction(())).reduceLeft(_ andThen _).asInstanceOf[EffectfulAction[E, Unit, NoStream]]

  /** Create an Action that runs some other actions in sequence and combines their results
    * with the given function. */
  def fold[E <: Effect, T](actions: Seq[EffectfulAction[E, T, NoStream]], zero: T)(f: (T, T) => T)(implicit ec: ExecutionContext): EffectfulAction[E, T, NoStream] =
    actions.foldLeft[EffectfulAction[E, T, NoStream]](Action.successful(zero)) { (za, va) => za.flatMap(z => va.map(v => f(z, v))) }

  /** An Action that pins the current session */
  private[slick] object Pin extends SynchronousDatabaseAction[DatabaseComponent, Effect, Unit, NoStream] {
    def run(context: DatabaseComponent#Context): Unit = context.pin
    def getDumpInfo = DumpInfo(name = "SynchronousDatabaseAction.Pin")
  }

  /** An Action that unpins the current session */
  private[slick] object Unpin extends SynchronousDatabaseAction[DatabaseComponent, Effect, Unit, NoStream] {
    def run(context: DatabaseComponent#Context): Unit = context.unpin
    def getDumpInfo = DumpInfo(name = "SynchronousDatabaseAction.Unpin")
  }

  /** An ExecutionContext used internally for executing plumbing operations during Action
    * composition. */
  private[slick] object sameThreadExecutionContext extends ExecutionContext {
    override def execute(runnable: Runnable): Unit = runnable.run()
    override def reportFailure(t: Throwable): Unit = throw t
  }
}

/** An Action that represents a database operation. Concrete implementations are backend-specific. */
trait DatabaseAction[-E <: Effect, +R, +S <: NoStream] extends EffectfulAction[E, R, S] {
  override def isLogged = true
}

/** An Action that returns a constant value. */
case class SuccessAction[+R](value: R) extends SynchronousDatabaseAction[DatabaseComponent, Effect, R, NoStream] {
  def getDumpInfo = DumpInfo("success", String.valueOf(value))
  def run(ctx: DatabaseComponent#Context): R = value
}

/** An Action that fails. */
case class FailureAction(t: Throwable) extends SynchronousDatabaseAction[DatabaseComponent, Effect, Nothing, NoStream] {
  def getDumpInfo = DumpInfo("failure", String.valueOf(t))
  def run(ctx: DatabaseComponent#Context): Nothing = throw t
}

/** An asynchronous Action that returns the result of a Future. */
case class FutureAction[+R](f: Future[R]) extends EffectfulAction[Effect, R, NoStream] {
  def getDumpInfo = DumpInfo("future", String.valueOf(f))
  override def isLogged = true
}

/** An Action that represents a `flatMap` operation for sequencing in the Action monad. */
case class FlatMapAction[-E <: Effect, +R, +S <: NoStream, P](base: EffectfulAction[E, P, NoStream], f: P => EffectfulAction[E, R, S], executor: ExecutionContext) extends EffectfulAction[E, R, S] {
  def getDumpInfo = DumpInfo("flatMap", String.valueOf(f), children = Vector(("base", base)))
}

/** An Action that represents an `andThen` operation for sequencing in the Action monad. */
case class AndThenAction[-E <: Effect, +R, +S <: NoStream](a1: EffectfulAction[E, _, NoStream], a2: EffectfulAction[E, R, S]) extends EffectfulAction[E, R, S] {
  def getDumpInfo = DumpInfo("andThen", children = Vector(("1", a1), ("2", a2)))
}

/** An Action that represents a `zip` operation for sequencing in the Action monad. */
case class ZipAction[-E <: Effect, +R1, +R2](a1: EffectfulAction[E, R1, NoStream], a2: EffectfulAction[E, R2, NoStream]) extends EffectfulAction[E, (R1, R2), NoStream] {
  def getDumpInfo = DumpInfo("zip", children = Vector(("1", a1), ("2", a2)))
}

/** An Action that represents a `cleanUp` operation for sequencing in the Action monad. */
case class CleanUpAction[-E <: Effect, +R, +S <: NoStream](base: EffectfulAction[E, R, S], f: Option[Throwable] => EffectfulAction[E, _, NoStream], keepFailure: Boolean, executor: ExecutionContext) extends EffectfulAction[E, R, S] {
  def getDumpInfo = DumpInfo("cleanUp", children = Vector(("try", base)))
}

/** An Action that represents a `failed` operation. */
case class FailedAction[-E <: Effect](a: EffectfulAction[E, _, NoStream]) extends EffectfulAction[E, Throwable, NoStream] {
  def getDumpInfo = DumpInfo("failed", children = Vector(("base", a)))
}

/** An Action that represents an `asTry` operation. */
case class AsTryAction[-E <: Effect, +R](a: EffectfulAction[E, R, NoStream]) extends EffectfulAction[E, Try[R], NoStream] {
  def getDumpInfo = DumpInfo("asTry")
}

/** An Action that attaches a name for logging purposes to another action. */
case class NamedAction[-E <: Effect, +R, +S <: NoStream](a: EffectfulAction[E, R, S], name: String) extends EffectfulAction[E, R, S] {
  def getDumpInfo = DumpInfo("named", mainInfo = DumpInfo.highlight(name))
  override def isLogged = true
}

/** The base trait for the context object passed to synchronous database actions by the execution
  * engine. */
trait ActionContext {
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
}

/** An ActionContext with extra functionality required for streaming Actions. */
trait StreamingActionContext extends ActionContext {
  /** Emit a single result of the stream. Any Exception thrown by this method should be passed on
    * to the caller. */
  def emit(v: Any)

  /** Get the Subscription for this stream. */
  def subscription: Subscription
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
trait SynchronousDatabaseAction[-B <: DatabaseComponent, -E <: Effect, +R, +S <: NoStream] extends DatabaseAction[E, R, S] { self =>
  /** The type used by this action for the state of a suspended stream. A call to `emitStream`
    * produces such a state which is then fed back into the next call. */
  type StreamState >: Null <: AnyRef

  /** Run this action synchronously and produce a result, or throw an Exception to indicate a
    * failure. */
  def run(context: B#Context): R

  /** Run this action synchronously and emit results to the context. This methods may throw an
    * Exception to indicate a failure.
    *
    * @param limit The maximum number of results to emit, or Long.MaxValue for no limit.
    * @param state The state returned by a previous invocation of this method, or `null` if
    *             a new stream should be produced.
    * @return A stream state if there are potentially more results available, or null if the
    *         stream is finished. */
  def emitStream(context: B#StreamingContext, limit: Long, state: StreamState): StreamState =
    throw new SlickException("Internal error: Streaming is not supported by this Action")

  /** Dispose of a `StreamState` when a streaming action is cancelled. Whenever `emitStream`
    * returns `null` or throws an Exception, it needs to dispose of the state itself. This
    * method will not be called in these cases. */
  def cancelStream(context: B#StreamingContext, state: StreamState): Unit = ()

  /** Whether or not this Action supports streaming results. An Action with a `Streaming` result
    * type must either support streaming directly or have a [[nonFusedEquivalentAction]] which
    * supports streaming. This flag is ignored if the Action has a `NoStream` result type. */
  def supportsStreaming: Boolean = true

  private[this] def superAndThen[E2 <: Effect, R2, S2 <: NoStream](a: EffectfulAction[E2, R2, S2]) = super.andThen[E2, R2, S2](a)
  override def andThen[E2 <: Effect, R2, S2 <: NoStream](a: EffectfulAction[E2, R2, S2]): EffectfulAction[E with E2, R2, S2] = a match {
    case a: SynchronousDatabaseAction[_, _, _, _] => new SynchronousDatabaseAction.Fused[B, E with E2, R2, S2] {
      def run(context: B#Context): R2 = {
        self.run(context)
        a.asInstanceOf[SynchronousDatabaseAction[B, E2, R2, S2]].run(context)
      }
      override def nonFusedEquivalentAction: EffectfulAction[E with E2, R2, S2] = superAndThen(a)
    }
    case a => superAndThen(a)
  }

  private[this] def superZip[E2 <: Effect, R2](a: EffectfulAction[E2, R2, NoStream]) = super.zip[E2, R2](a)
  override def zip[E2 <: Effect, R2](a: EffectfulAction[E2, R2, NoStream]): EffectfulAction[E with E2, (R, R2), NoStream] = a match {
    case a: SynchronousDatabaseAction[_, _, _, _] => new SynchronousDatabaseAction.Fused[B, E with E2, (R, R2), NoStream] {
      def run(context: B#Context): (R, R2) = {
        val r1 = self.run(context)
        val r2 = a.asInstanceOf[SynchronousDatabaseAction[B, E2, R2, NoStream]].run(context)
        (r1, r2)
      }
      override def nonFusedEquivalentAction: EffectfulAction[E with E2, (R, R2), NoStream] = superZip(a)
    }
    case a => superZip(a)
  }

  private[this] def superAndFinally[E2 <: Effect](a: EffectfulAction[E2, _, NoStream]) = super.andFinally[E2](a)
  override def andFinally[E2 <: Effect](a: EffectfulAction[E2, _, NoStream]): EffectfulAction[E with E2, R, S] = a match {
    case a: SynchronousDatabaseAction[_, _, _, _] => new SynchronousDatabaseAction.Fused[B, E with E2, R, S] {
      def run(context: B#Context): R = {
        val res = try self.run(context) catch {
          case NonFatal(ex) =>
            try a.asInstanceOf[SynchronousDatabaseAction[B, E2, Any, NoStream]].run(context) catch ignoreFollowOnError
            throw ex
        }
        a.asInstanceOf[SynchronousDatabaseAction[B, E2, Any, S]].run(context)
        res
      }
      override def nonFusedEquivalentAction: EffectfulAction[E with E2, R, S] = superAndFinally(a)
    }
    case a => superAndFinally(a)
  }

  private[this] def superWithPinnedSession = super.withPinnedSession
  override def withPinnedSession: EffectfulAction[E, R, S] = new SynchronousDatabaseAction.Fused[B, E, R, S] {
    def run(context: B#Context): R = {
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

  private[this] def superFailed: EffectfulAction[E, Throwable, NoStream] = super.failed
  override def failed: EffectfulAction[E, Throwable, NoStream] = new SynchronousDatabaseAction.Fused[B, E, Throwable, NoStream] {
    def run(context: B#Context): Throwable = {
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

  private[this] def superAsTry: EffectfulAction[E, Try[R], NoStream] = super.asTry
  override def asTry: EffectfulAction[E, Try[R], NoStream] = new SynchronousDatabaseAction.Fused[B, E, Try[R], NoStream] {
    def run(context: B#Context): Try[R] = {
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

  /** Fuse `flatMap` / `map`, `cleanUp` and `filter` / `withFilter` combinators if they use
    * `Action.sameThreadExecutionContext` and produce a `SynchronousDatabaseAction` in their
    * evaluation function (where applicable). This cannot be verified at fusion time, so a wrongly
    * fused Action can fail with a `ClassCastException` during evaluation. */
  private[slick] def fuseUnsafe[E <: Effect, R, S <: NoStream](a: EffectfulAction[E, R, S]): EffectfulAction[E, R, S] = {
    a match {
      case FlatMapAction(base: SynchronousDatabaseAction[_, _, _, _], f, ec) if ec eq Action.sameThreadExecutionContext =>
        new SynchronousDatabaseAction.Fused[DatabaseComponent, E, R, S] {
          def run(context: DatabaseComponent#Context): R = {
            val b = base.asInstanceOf[SynchronousDatabaseAction[DatabaseComponent, Effect, Any, NoStream]].run(context)
            val a2 = f(b)
            a2.asInstanceOf[SynchronousDatabaseAction[DatabaseComponent, E, R, S]].run(context)
          }
          override def nonFusedEquivalentAction = a
        }

      case CleanUpAction(base: SynchronousDatabaseAction[_, _, _, _], f, keepFailure, ec) if ec eq Action.sameThreadExecutionContext =>
        new SynchronousDatabaseAction.Fused[DatabaseComponent, E, R, S] {
          def run(context: DatabaseComponent#Context): R = {
            val res = try {
              base.asInstanceOf[SynchronousDatabaseAction[DatabaseComponent, Effect, R, S]].run(context)
            } catch { case NonFatal(ex) =>
              try {
                val a2 = f(Some(ex))
                a2.asInstanceOf[SynchronousDatabaseAction[DatabaseComponent, Effect, Any, NoStream]].run(context)
              } catch { case NonFatal(_) if keepFailure => () }
              throw ex
            }
            val a2 = f(None)
            a2.asInstanceOf[SynchronousDatabaseAction[DatabaseComponent, Effect, Any, NoStream]].run(context)
            res
          }
          override def nonFusedEquivalentAction = a
        }

      case a => a
    }
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

  /** The bottom type of all standard effects. It is used by the `Action` and `StreamingAction`
    * type aliases instead of `Nothing` because the compiler does not properly infer `Nothing`
    * where needed. You can still introduce your own custom effect types but they will not be
    * used by `Action` and `StreamingAction`, so you either have to define your own type aliases
    * or spell out the proper `EffectfulAction` types in type annotations. */
  trait All extends Read with Write with Schema with Transactional
}

/** A phantom type used as the streaming result type for Actions that do not support streaming.
  * Note that this is a supertype of `Streaming` (and it is used in covariant position),
  * so that any streaming Action can be used where a non-streaming Action is expected. */
sealed trait NoStream

/** A phantom type used as the streaming result type for Actions that do support streaming. */
sealed trait Streaming[+T] extends NoStream
