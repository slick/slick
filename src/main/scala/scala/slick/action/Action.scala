package scala.slick.action

import scala.concurrent.{ExecutionContext, Future}
import scala.slick.backend.DatabaseComponent

/** Abstract type for Actions. Allows separation of execution logic and resource usage
  * management logic from composition logic.
  *
  * @tparam E Effect type
  * @tparam R Result type
  */
sealed trait Action[-E <: Effect, +R] {
  def map[R2](f: R => R2)(implicit executor: ExecutionContext): Action[E, R2] =
    flatMap[E, R2](r => ConstantAction[E, R2](f(r)))

  def flatMap[E2 <: Effect, R2](f: R => Action[E2, R2])(implicit executor: ExecutionContext): Action[E with E2, R2] =
    FlatMapAction[E with E2, R2, R](this, f, executor)

  def andThen[E2 <: Effect, R2](a: Action[E2, R2]): Action[E with E2, R2] =
    AndThenAction[E with E2, R2](this, a)

  final def >> [E2 <: Effect, R2](a: Action[E2, R2]): Action[E with E2, R2] =
    andThen[E2, R2](a)
}

object Action {
  def fromFuture[R](f: Future[R]): Action[Effect, R] =
    FutureAction[R](f)
}

/** An Action that represents a database operation. Concrete implementations are backend-specific
  * and therefore carry a `BackendType` effect. */
trait DatabaseAction[B <: DatabaseComponent, -E <: Effect, +R] extends Action[E with Effect.BackendType[B], R]

/** An Action that returns a constant value */
case class ConstantAction[-E <: Effect, +R](value: R) extends Action[E, R]

/** An asynchronous Action that returns the result of a Future */
case class FutureAction[+R](f: Future[R]) extends Action[Effect, R]

/** An Action that represents a `flatMap` operation for sequencing in the Action monad */
case class FlatMapAction[-E <: Effect, +R, P](base: Action[E, P], f: P => Action[E, R], executor: ExecutionContext) extends Action[E, R]

/** An Action that represents an `andThen` operation for sequencing in the Action monad */
case class AndThenAction[-E <: Effect, +R](a1: Action[E, _], a2: Action[E, R]) extends Action[E, R]

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
