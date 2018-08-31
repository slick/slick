package slick.dbio

import org.reactivestreams.Subscription

import scala.collection.mutable.ArrayBuffer
import scala.language.higherKinds

import scala.collection.generic.{CanBuild, CanBuildFrom}
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import slick.SlickException
import slick.basic.BasicBackend
import slick.util.{DumpInfo, Dumpable, ignoreFollowOnError}
import scala.util.{Try, Failure, Success}
import scala.util.control.NonFatal

/** A Database I/O Action that can be executed on a database. The DBIOAction type allows a
  * separation of execution logic and resource usage management logic from composition logic.
  * DBIOActions can be composed with methods such as `andThen`, `andFinally` and `flatMap`.
  * Individual parts of a composite DBIOAction are always executed serially on a single database,
  * but possibly in different database sessions, unless the session is pinned either explicitly
  * (using `withPinnedSession`) or implicitly (e.g. through a transaction).
  *
  * The actual implementation base type for all Actions is `DBIOAction`. `StreamingDBIO` and
  * `DBIO` are type aliases which discard the effect type (and the streaming result type in the
  * latter case) to make DBIOAction types easier to write when these features are not needed. All
  * primitive DBIOActions and all DBIOActions produced by the standard combinators in Slick have
  * correct Effect types and are streaming (if possible).
  *
  * @tparam R The result type when executing the DBIOAction and fully materializing the result.
  * @tparam S An encoding of the result type for streaming results. If this action is capable of
  *           streaming, it is `Streaming[T]` for an element type `T`. For non-streaming
  *           DBIOActions it is `NoStream`.
  * @tparam E The DBIOAction's effect type, e.g. `Effect.Read with Effect.Write`. When composing
  *           actions, the correct combined effect type will be inferred. Effects can be used in
  *           user code, e.g. to automatically direct all read-only Actions to a slave database
  *           and write Actions to the master copy.
  */
sealed trait DBIOAction[+R, +S <: NoStream, -E <: Effect] extends Dumpable {
  /** Transform the result of a successful execution of this action. If this action fails, the
    * resulting action also fails. */
  def map[R2](f: R => R2)(implicit executor: ExecutionContext): DBIOAction[R2, NoStream, E] =
    flatMap[R2, NoStream, E](r => SuccessAction[R2](f(r)))

  /** Use the result produced by the successful execution of this action to compute and then
    * run the next action in sequence. The resulting action fails if either this action, the
    * computation, or the computed action fails. */
  def flatMap[R2, S2 <: NoStream, E2 <: Effect](f: R => DBIOAction[R2, S2, E2])(implicit executor: ExecutionContext): DBIOAction[R2, S2, E with E2] =
    FlatMapAction[R2, S2, R, E with E2](this, f, executor)

  /** Creates a new DBIOAction with one level of nesting flattened, this method is equivalent
    * to `flatMap(identity)`.
    */
  def flatten[R2, S2 <: NoStream, E2 <: Effect](implicit ev : R <:< DBIOAction[R2,S2,E2]) = flatMap(ev)(DBIO.sameThreadExecutionContext)

  /** Run another action after this action, if it completed successfully, and return the result
    * of the second action. If either of the two actions fails, the resulting action also fails. */
  def andThen[R2, S2 <: NoStream, E2 <: Effect](a: DBIOAction[R2, S2, E2]): DBIOAction[R2, S2, E with E2] = a match {
    case AndThenAction(as2) => AndThenAction[R2, S2, E with E2](this +: as2)
    case a => AndThenAction[R2, S2, E with E2](Vector(this, a))
  }

  /** Run another action after this action, if it completed successfully, and return the result
    * of both actions. If either of the two actions fails, the resulting action also fails. */
  def zip[R2, E2 <: Effect](a: DBIOAction[R2, NoStream, E2]): DBIOAction[(R, R2), NoStream, E with E2] =
    SequenceAction[Any, ArrayBuffer[Any], E with E2](Vector(this, a)).map { r =>
      (r(0).asInstanceOf[R], r(1).asInstanceOf[R2])
    } (DBIO.sameThreadExecutionContext)

  /** Run another action after this action, if it completed successfully, and zip the result
    * of both actions with a function `f`, then create a new DBIOAction holding this result,
    * If either of the two actions fails, the resulting action also fails. */
  def zipWith[R2, E2 <: Effect,R3](a: DBIOAction[R2, NoStream, E2])(f:(R,R2) =>R3)(implicit executor: ExecutionContext): DBIOAction[R3, NoStream, E with E2] =
    SequenceAction[Any, ArrayBuffer[Any], E with E2](Vector(this, a)).map { r =>
      f(r(0).asInstanceOf[R], r(1).asInstanceOf[R2])
    } (executor)

  /** Run another action after this action, whether it succeeds or fails, and then return the
    * result of the first action. If the first action fails, its failure is propagated, whether
    * the second action fails or succeeds. If the first action succeeds, a failure of the second
    * action is propagated. */
  def andFinally[E2 <: Effect](a: DBIOAction[_, NoStream, E2]): DBIOAction[R, S, E with E2] =
    cleanUp[E2](_ => a)(DBIO.sameThreadExecutionContext)

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
  def cleanUp[E2 <: Effect](f: Option[Throwable] => DBIOAction[_, NoStream, E2], keepFailure: Boolean = true)(implicit executor: ExecutionContext): DBIOAction[R, S, E with E2] =
    CleanUpAction[R, S, E with E2](this, f, keepFailure, executor)

  /** A shortcut for `andThen`. */
  final def >> [R2, S2 <: NoStream, E2 <: Effect](a: DBIOAction[R2, S2, E2]): DBIOAction[R2, S2, E with E2] =
    andThen[R2, S2, E2](a)

  /** Filter the result of this action with the given predicate. If the predicate matches, the
    * original result is returned, otherwise the resulting action fails with a
    * NoSuchElementException. */
  final def filter(p: R => Boolean)(implicit executor: ExecutionContext): DBIOAction[R, NoStream, E] =
    withFilter(p)

  def withFilter(p: R => Boolean)(implicit executor: ExecutionContext): DBIOAction[R, NoStream, E] =
    flatMap(v => if(p(v)) SuccessAction(v) else throw new NoSuchElementException("Action.withFilter failed"))

  /** Transform the result of a successful execution of this action, if the given partial function is defined at that value,
    * otherwise, the result DBIOAction will fail with a `NoSuchElementException`.
    *
    * If this action fails, the resulting action also fails. */
  def collect[R2](pf: PartialFunction[R,R2])(implicit executor: ExecutionContext): DBIOAction[R2, NoStream, E] =
    map(r1 => pf.applyOrElse(r1,(r:R) => throw new NoSuchElementException(s"DBIOAction.collect partial function is not defined at: $r")))

  /** Return an action which contains the Throwable with which this action failed as its result.
    * If this action succeeded, the resulting action fails with a NoSuchElementException. */
  def failed: DBIOAction[Throwable, NoStream, E] = FailedAction[E](this)

  /** Convert a successful result `v` of this action into a successful result `Success(v)` and a
    * failure `t` into a successful result `Failure(t)`. This is the most generic combinator that
    * can be used for error recovery. If possible, use [[andFinally]] or [[cleanUp]] instead,
    * because those combinators, unlike `asTry`, support streaming. */
  def asTry: DBIOAction[Try[R], NoStream, E] = AsTryAction[R, E](this)

  /** Use a pinned database session when running this action. If it is composed of multiple
    * database actions, they will all use the same session, even when sequenced with non-database
    * actions. For non-composite or non-database actions, this has no effect. */
  def withPinnedSession: DBIOAction[R, S, E] = DBIO.Pin andThen this andFinally DBIO.Unpin

  /** Get a wrapping action which has a name that will be included in log output. */
  def named(name: String): DBIOAction[R, S, E] =
    NamedAction[R, S, E](this, name)

  /** Get the equivalent non-fused action if this action has been fused, otherwise this
    * action is returned. */
  def nonFusedEquivalentAction: DBIOAction[R, S, E] = this

  /** Whether or not this action should be included in log output by default. */
  def isLogged: Boolean = false
}

object DBIOAction {
  /** Convert a `Future` to a [[DBIOAction]]. */
  def from[R](f: Future[R]): DBIOAction[R, NoStream, Effect] = FutureAction[R](f)

  /** Lift a constant value to a [[DBIOAction]]. */
  def successful[R](v: R): DBIOAction[R, NoStream, Effect] = SuccessAction[R](v)

  /** Create a [[DBIOAction]] that always fails. */
  def failed(t: Throwable): DBIOAction[Nothing, NoStream, Effect] = FailureAction(t)

  private[this] def groupBySynchronicity[R, E <: Effect](in: TraversableOnce[DBIOAction[R, NoStream, E]]): Vector[Vector[DBIOAction[R, NoStream, E]]] = {
    var state = 0 // no current = 0, sync = 1, async = 2
    var current: mutable.Builder[DBIOAction[R, NoStream, E], Vector[DBIOAction[R, NoStream, E]]] = null
    val total = Vector.newBuilder[Vector[DBIOAction[R, NoStream, E]]]
    (in: TraversableOnce[Any]).foreach { a =>
      val msgState = if(a.isInstanceOf[SynchronousDatabaseAction[_, _, _, _]]) 1 else 2
      if(msgState != state) {
        if(state != 0) total += current.result()
        current = Vector.newBuilder
        state = msgState
      }
      current += a.asInstanceOf[DBIOAction[R, NoStream, E]]
    }
    if(state != 0) total += current.result()
    total.result()
  }

  /** Transform a `Option[ DBIO[R] ]` into a `DBIO[ Option[R] ]`. */
  def sequenceOption[R, E <: Effect](in: Option[DBIOAction[R, NoStream, E]]): DBIOAction[Option[R], NoStream, E] = {
    implicit val ec = DBIO.sameThreadExecutionContext
    sequence(in.toList).map(_.headOption)
  }

  /** Transform a `TraversableOnce[ DBIO[R] ]` into a `DBIO[ TraversableOnce[R] ]`. */
  def sequence[R, M[+_] <: TraversableOnce[_], E <: Effect](in: M[DBIOAction[R, NoStream, E]])(implicit cbf: CanBuildFrom[M[DBIOAction[R, NoStream, E]], R, M[R]]): DBIOAction[M[R], NoStream, E] = {
    implicit val ec = DBIO.sameThreadExecutionContext
    def sequenceGroupAsM(g: Vector[DBIOAction[R, NoStream, E]]): DBIOAction[M[R], NoStream, E] = {
      if(g.head.isInstanceOf[SynchronousDatabaseAction[_, _, _, _]]) { // fuse synchronous group
        new SynchronousDatabaseAction.Fused[M[R], NoStream, BasicBackend, E] {
          def run(context: BasicBackend#Context) = {
            val b = cbf()
            g.foreach(a => b += a.asInstanceOf[SynchronousDatabaseAction[R, NoStream, BasicBackend, E]].run(context))
            b.result()
          }
          override def nonFusedEquivalentAction = SequenceAction[R, M[R], E](g)
        }
      } else SequenceAction[R, M[R], E](g)
    }
    def sequenceGroupAsSeq(g: Vector[DBIOAction[R, NoStream, E]]): DBIOAction[Seq[R], NoStream, E] = {
      if(g.length == 1) {
        if(g.head.isInstanceOf[SynchronousDatabaseAction[_, _, _, _]]) { // fuse synchronous group
          new SynchronousDatabaseAction.Fused[Seq[R], NoStream, BasicBackend, E] {
            def run(context: BasicBackend#Context) =
              g.head.asInstanceOf[SynchronousDatabaseAction[R, NoStream, BasicBackend, E]].run(context) :: Nil
            override def nonFusedEquivalentAction = g.head.map(_ :: Nil)
          }
        } else g.head.map(_ :: Nil)
      } else {
        if(g.head.isInstanceOf[SynchronousDatabaseAction[_, _, _, _]]) { // fuse synchronous group
          new SynchronousDatabaseAction.Fused[Seq[R], NoStream, BasicBackend, E] {
            def run(context: BasicBackend#Context) = {
              val b = new ArrayBuffer[R](g.length)
              g.foreach(a => b += a.asInstanceOf[SynchronousDatabaseAction[R, NoStream, BasicBackend, E]].run(context))
              b
            }
            override def nonFusedEquivalentAction = SequenceAction[R, Seq[R], E](g)
          }
        } else SequenceAction[R, Seq[R], E](g)
      }
    }
    val grouped = groupBySynchronicity[R, E](in.asInstanceOf[TraversableOnce[DBIOAction[R, NoStream, E]]])
    grouped.length match {
      case 0 => DBIO.successful(cbf().result())
      case 1 => sequenceGroupAsM(grouped.head)
      case n =>
        grouped.foldLeft(DBIO.successful(cbf(in)): DBIOAction[mutable.Builder[R, M[R]], NoStream, E]) { (ar, g) =>
          for (r <- ar; ge <- sequenceGroupAsSeq(g)) yield r ++= ge
        } map (_.result)
    }
  }

  /** A simpler version of `sequence` that takes a number of DBIOActions with any return type as
    * varargs and returns a DBIOAction that performs the individual actions in sequence, returning
    * `()` in the end. */
  def seq[E <: Effect](actions: DBIOAction[_, NoStream, E]*): DBIOAction[Unit, NoStream, E] = {
    def sequenceGroup(g: Vector[DBIOAction[Any, NoStream, E]], forceUnit: Boolean): DBIOAction[Any, NoStream, E] = {
      if(g.length == 1 && !forceUnit) g.head
      else if(g.head.isInstanceOf[SynchronousDatabaseAction[_, _, _, _]]) sequenceSync(g)
      else if(forceUnit) AndThenAction[Any, NoStream, E](g :+ DBIO.successful(()))
      else AndThenAction[Any, NoStream, E](g)
    }
    def sequenceSync(g: Vector[DBIOAction[Any, NoStream, E]]): DBIOAction[Unit, NoStream, E] = {
      new SynchronousDatabaseAction.Fused[Unit, NoStream, BasicBackend, E] {
        def run(context: BasicBackend#Context) = {
          g.foreach(_.asInstanceOf[SynchronousDatabaseAction[Any, NoStream, BasicBackend, E]].run(context))
        }
        override def nonFusedEquivalentAction = AndThenAction[Unit, NoStream, E](g)
      }
    }
    if(actions.isEmpty) DBIO.successful(()) else {
      val grouped = groupBySynchronicity[Any, E](actions :+ DBIO.successful(()))
      grouped.length match {
        case 1 => sequenceGroup(grouped.head, true).asInstanceOf[DBIOAction[Unit, NoStream, E]]
        case n =>
          val last = grouped.length - 1
          val as = grouped.iterator.zipWithIndex.map { case (g, i) => sequenceGroup(g, i == last) }.toVector
          AndThenAction[Unit, NoStream, E](as)
      }
    }
  }

  /** Create a DBIOAction that runs some other actions in sequence and combines their results
    * with the given function. */
  def fold[T, E <: Effect](actions: Seq[DBIOAction[T, NoStream, E]], zero: T)(f: (T, T) => T)(implicit ec: ExecutionContext): DBIOAction[T, NoStream, E] =
    actions.foldLeft[DBIOAction[T, NoStream, E]](DBIO.successful(zero)) { (za, va) => za.flatMap(z => va.map(v => f(z, v))) }

  /** A DBIOAction that pins the current session */
  private[slick] object Pin extends SynchronousDatabaseAction[Unit, NoStream, BasicBackend, Effect] {
    def run(context: BasicBackend#Context): Unit = context.pin
    def getDumpInfo = DumpInfo(name = "SynchronousDatabaseAction.Pin")
  }

  /** A DBIOAction that unpins the current session */
  private[slick] object Unpin extends SynchronousDatabaseAction[Unit, NoStream, BasicBackend, Effect] {
    def run(context: BasicBackend#Context): Unit = context.unpin
    def getDumpInfo = DumpInfo(name = "SynchronousDatabaseAction.Unpin")
  }

  /** An ExecutionContext used internally for executing plumbing operations during DBIOAction
    * composition. */
  private[slick] object sameThreadExecutionContext extends ExecutionContext {
    private[this] val trampoline = new ThreadLocal[List[Runnable]]

    private[this] def runTrampoline(first: Runnable): Unit = {
      trampoline.set(Nil)
      try {
        var err: Throwable = null
        var r = first
        while(r ne null) {
          try r.run() catch { case t: Throwable => err = t }
          trampoline.get() match {
            case r2 :: rest =>
              trampoline.set(rest)
              r = r2
            case _ => r = null
          }
        }
        if(err ne null) throw err
      } finally trampoline.set(null)
    }

    override def execute(runnable: Runnable): Unit = trampoline.get() match {
      case null => runTrampoline(runnable)
      case r => trampoline.set(runnable :: r)
    }

    override def reportFailure(t: Throwable): Unit = throw t
  }
}

/** A DBIOAction that represents a database operation. Concrete implementations are backend-specific. */
trait DatabaseAction[+R, +S <: NoStream, -E <: Effect] extends DBIOAction[R, S, E] {
  override def isLogged = true
}

/** A DBIOAction that returns a constant value. */
case class SuccessAction[+R](value: R) extends SynchronousDatabaseAction[R, NoStream, BasicBackend, Effect] {
  def getDumpInfo = DumpInfo("success", String.valueOf(value))
  def run(ctx: BasicBackend#Context): R = value
}

/** A DBIOAction that fails. */
case class FailureAction(t: Throwable) extends SynchronousDatabaseAction[Nothing, NoStream, BasicBackend, Effect] {
  def getDumpInfo = DumpInfo("failure", String.valueOf(t))
  def run(ctx: BasicBackend#Context): Nothing = throw t
}

/** An asynchronous DBIOAction that returns the result of a Future. */
case class FutureAction[+R](f: Future[R]) extends DBIOAction[R, NoStream, Effect] {
  def getDumpInfo = DumpInfo("future", String.valueOf(f))
  override def isLogged = true
}

/** A DBIOAction that represents a `flatMap` operation for sequencing in the DBIOAction monad. */
case class FlatMapAction[+R, +S <: NoStream, P, -E <: Effect](base: DBIOAction[P, NoStream, E], f: P => DBIOAction[R, S, E], executor: ExecutionContext) extends DBIOAction[R, S, E] {
  def getDumpInfo = DumpInfo("flatMap", String.valueOf(f), children = Vector(("base", base)))
}

/** A DBIOAction that represents a `seq` or `andThen` operation for sequencing in the DBIOAction
  * monad. Unlike `SequenceAction` it only keeps the last result. */
case class AndThenAction[R, +S <: NoStream, -E <: Effect](as: IndexedSeq[DBIOAction[Any, NoStream, E]]) extends DBIOAction[R, S, E] {
  def getDumpInfo = DumpInfo("andThen", children = as.zipWithIndex.map { case (a, i) => (String.valueOf(i+1), a) })

  override def andThen[R2, S2 <: NoStream, E2 <: Effect](a: DBIOAction[R2, S2, E2]): DBIOAction[R2, S2, E with E2] = a match {
    case AndThenAction(as2) => AndThenAction[R2, S2, E with E2](as ++ as2)
    case a => AndThenAction[R2, S2, E with E2](as :+ a)
  }
}

/** A DBIOAction that represents a `sequence` or operation for sequencing in the DBIOAction monad. */
case class SequenceAction[R, +R2, -E <: Effect](as: IndexedSeq[DBIOAction[R, NoStream, E]])(implicit val cbf: CanBuild[R, R2]) extends DBIOAction[R2, NoStream, E] {
  def getDumpInfo = DumpInfo("sequence", children = as.zipWithIndex.map { case (a, i) => (String.valueOf(i+1), a) })
}

/** A DBIOAction that represents a `cleanUp` operation for sequencing in the DBIOAction monad. */
case class CleanUpAction[+R, +S <: NoStream, -E <: Effect](base: DBIOAction[R, S, E], f: Option[Throwable] => DBIOAction[_, NoStream, E], keepFailure: Boolean, executor: ExecutionContext) extends DBIOAction[R, S, E] {
  def getDumpInfo = DumpInfo("cleanUp", children = Vector(("try", base)))
}

/** A DBIOAction that represents a `failed` operation. */
case class FailedAction[-E <: Effect](a: DBIOAction[_, NoStream, E]) extends DBIOAction[Throwable, NoStream, E] {
  def getDumpInfo = DumpInfo("failed", children = Vector(("base", a)))
}

/** A DBIOAction that represents an `asTry` operation. */
case class AsTryAction[+R, -E <: Effect](a: DBIOAction[R, NoStream, E]) extends DBIOAction[Try[R], NoStream, E] {
  def getDumpInfo = DumpInfo("asTry")
}

/** A DBIOAction that attaches a name for logging purposes to another action. */
case class NamedAction[+R, +S <: NoStream, -E <: Effect](a: DBIOAction[R, S, E], name: String) extends DBIOAction[R, S, E] {
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

/** An ActionContext with extra functionality required for streaming DBIOActions. */
trait StreamingActionContext extends ActionContext {
  /** Emit a single result of the stream. Any Exception thrown by this method should be passed on
    * to the caller. */
  def emit(v: Any): Unit

  /** Get the Subscription for this stream. */
  def subscription: Subscription
}

/** A synchronous database action provides a function from an `ActionContext` to the result
  * type. `BasicBackend.DatabaseDef.run` supports this kind of action out of the box
  * through `BasicBackend.DatabaseDef.runSynchronousDatabaseAction` so that `run` does not
  * need to be extended if all primitive database actions can be expressed in this way. These
  * actions also implement construction-time fusion for the `andFinally`, `andThen`, `asTry`,
  * `failed`, `withPinnedSession` and `zip` operations.
  *
  * The execution engine ensures that an [[ActionContext]] is never used concurrently and that
  * all state changes performed by one invocation of a SynchronousDatabaseAction are visible
  * to the next invocation of the same or a different SynchronousDatabaseAction. */
trait SynchronousDatabaseAction[+R, +S <: NoStream, -B <: BasicBackend, -E <: Effect] extends DatabaseAction[R, S, E] { self =>
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

  /** Whether or not this action supports streaming results. An action with a `Streaming` result
    * type must either support streaming directly or have a [[nonFusedEquivalentAction]] which
    * supports streaming. This flag is not used if the Action has a `NoStream` result type. */
  def supportsStreaming: Boolean = true

  override def andThen[R2, S2 <: NoStream, E2 <: Effect](a: DBIOAction[R2, S2, E2]): DBIOAction[R2, S2, E with E2] = a match {
    case a: SynchronousDatabaseAction.FusedAndThenAction[_, _, _, _] =>
      new SynchronousDatabaseAction.FusedAndThenAction[R2, S2, B, E with E2](
        self.asInstanceOf[SynchronousDatabaseAction[Any, S2, B, E with E2]] +:
          a.as.asInstanceOf[IndexedSeq[SynchronousDatabaseAction[Any, S2, B, E with E2]]])
    case a: SynchronousDatabaseAction[_, _, _, _] =>
      new SynchronousDatabaseAction.FusedAndThenAction[R2, S2, B, E with E2](
        Vector(self.asInstanceOf[SynchronousDatabaseAction[Any, S2, B, E with E2]],
          a.asInstanceOf[SynchronousDatabaseAction[Any, S2, B, E with E2]]))
    case a => super.andThen[R2, S2, E2](a)
  }

  private[this] def superZip[R2, E2 <: Effect](a: DBIOAction[R2, NoStream, E2]) = super.zip[R2, E2](a)
  override def zip[R2, E2 <: Effect](a: DBIOAction[R2, NoStream, E2]): DBIOAction[(R, R2), NoStream, E with E2] = a match {
    case a: SynchronousDatabaseAction[_, _, _, _] => new SynchronousDatabaseAction.Fused[(R, R2), NoStream, B, E with E2] {
      def run(context: B#Context): (R, R2) = {
        val r1 = self.run(context)
        val r2 = a.asInstanceOf[SynchronousDatabaseAction[R2, NoStream, B, E2]].run(context)
        (r1, r2)
      }
      override def nonFusedEquivalentAction: DBIOAction[(R, R2), NoStream, E with E2] = superZip(a)
    }
    case a => superZip(a)
  }

  private[this] def superAndFinally[E2 <: Effect](a: DBIOAction[_, NoStream, E2]) = super.andFinally[E2](a)
  override def andFinally[E2 <: Effect](a: DBIOAction[_, NoStream, E2]): DBIOAction[R, S, E with E2] = a match {
    case a: SynchronousDatabaseAction[_, _, _, _] => new SynchronousDatabaseAction.Fused[R, S, B, E with E2] {
      def run(context: B#Context): R = {
        val res = try self.run(context) catch {
          case NonFatal(ex) =>
            try a.asInstanceOf[SynchronousDatabaseAction[Any, NoStream, B, E2]].run(context) catch ignoreFollowOnError
            throw ex
        }
        a.asInstanceOf[SynchronousDatabaseAction[Any, S, B, E2]].run(context)
        res
      }
      override def nonFusedEquivalentAction: DBIOAction[R, S, E with E2] = superAndFinally(a)
    }
    case a => superAndFinally(a)
  }

  private[this] def superWithPinnedSession = super.withPinnedSession
  override def withPinnedSession: DBIOAction[R, S, E] = new SynchronousDatabaseAction.Fused[R, S, B, E] {
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

  private[this] def superFailed: DBIOAction[Throwable, NoStream, E] = super.failed
  override def failed: DBIOAction[Throwable, NoStream, E] = new SynchronousDatabaseAction.Fused[Throwable, NoStream, B, E] {
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

  private[this] def superAsTry: DBIOAction[Try[R], NoStream, E] = super.asTry
  override def asTry: DBIOAction[Try[R], NoStream, E] = new SynchronousDatabaseAction.Fused[Try[R], NoStream, B, E] {
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
  trait Fused[+R, +S <: NoStream, B <: BasicBackend, -E <: Effect] extends SynchronousDatabaseAction[R, S, B, E] {
    def getDumpInfo = DumpInfo(name = "SynchronousDatabaseAction.Fused", children = Vector(("non-fused", nonFusedEquivalentAction)))
    override def supportsStreaming: Boolean = false
  }

  class FusedAndThenAction[+R, +S <: NoStream, B <: BasicBackend, -E <: Effect](val as: IndexedSeq[SynchronousDatabaseAction[Any, S, B, E]]) extends Fused[R, S, B, E] {
    def run(context: B#Context): R = {
      var res: Any = null
      as.foreach(a => res = a.run(context))
      res.asInstanceOf[R]
    }
    override def nonFusedEquivalentAction: DBIOAction[R, S, E] = AndThenAction[R, S, E](as)
    override def andThen[R2, S2 <: NoStream, E2 <: Effect](a: DBIOAction[R2, S2, E2]): DBIOAction[R2, S2, E with E2] = a match {
      case a: SynchronousDatabaseAction.FusedAndThenAction[_, _, _, _] =>
        new SynchronousDatabaseAction.FusedAndThenAction[R2, S2, B, E with E2](
          as.asInstanceOf[IndexedSeq[SynchronousDatabaseAction[Any, S2, B, E with E2]]] ++
            a.as.asInstanceOf[IndexedSeq[SynchronousDatabaseAction[Any, S2, B, E with E2]]])
      case a: SynchronousDatabaseAction[_, _, _, _] =>
        new SynchronousDatabaseAction.FusedAndThenAction[R2, S2, B, E with E2](
          as.asInstanceOf[IndexedSeq[SynchronousDatabaseAction[Any, S2, B, E with E2]]] :+
            a.asInstanceOf[SynchronousDatabaseAction[Any, S2, B, E with E2]])
      case a => super.andThen(a)
    }
  }

  /** Fuse `flatMap` / `map`, `cleanUp` and `filter` / `withFilter` combinators if they use
    * `DBIO.sameThreadExecutionContext` and produce a `SynchronousDatabaseAction` in their
    * evaluation function (where applicable). This cannot be verified at fusion time, so a wrongly
    * fused action can fail with a `ClassCastException` during evaluation. */
  private[slick] def fuseUnsafe[R, S <: NoStream, E <: Effect](a: DBIOAction[R, S, E]): DBIOAction[R, S, E] = {
    a match {
      case FlatMapAction(base: SynchronousDatabaseAction[_, _, _, _], f, ec) if ec eq DBIO.sameThreadExecutionContext =>
        new SynchronousDatabaseAction.Fused[R, S, BasicBackend, E] {
          def run(context: BasicBackend#Context): R = {
            val b = base.asInstanceOf[SynchronousDatabaseAction[Any, NoStream, BasicBackend, Effect]].run(context)
            val a2 = f(b)
            a2.asInstanceOf[SynchronousDatabaseAction[R, S, BasicBackend, E]].run(context)
          }
          override def nonFusedEquivalentAction = a
        }

      case CleanUpAction(base: SynchronousDatabaseAction[_, _, _, _], f, keepFailure, ec) if ec eq DBIO.sameThreadExecutionContext =>
        new SynchronousDatabaseAction.Fused[R, S, BasicBackend, E] {
          def run(context: BasicBackend#Context): R = {
            val res = try {
              base.asInstanceOf[SynchronousDatabaseAction[R, S, BasicBackend, Effect]].run(context)
            } catch { case NonFatal(ex) =>
              try {
                val a2 = f(Some(ex))
                a2.asInstanceOf[SynchronousDatabaseAction[Any, NoStream, BasicBackend, Effect]].run(context)
              } catch { case NonFatal(_) if keepFailure => () }
              throw ex
            }
            val a2 = f(None)
            a2.asInstanceOf[SynchronousDatabaseAction[Any, NoStream, BasicBackend, Effect]].run(context)
            res
          }
          override def nonFusedEquivalentAction = a
        }

      case a => a
    }
  }
}
