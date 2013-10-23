package scala.slick.jdbc

import scala.language.higherKinds
import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.collection.immutable.Map
import scala.collection.generic.CanBuildFrom
import scala.slick.util.CloseableIterator
import scala.slick.util.iter._

/**
 * Base trait for all statement invokers, using parameter type P and result type R.
 */
trait Invoker[-P, +R] extends Function1[P, UnitInvoker[R]] { self =>

  /**
   * Execute the statement and return a CloseableIterator of the converted results.
   * The iterator must either be fully read or closed explicitly.
   */
  final def iterator(param: P)(implicit session: JdbcBackend#Session) = iteratorTo(param, 0)
  @deprecated("Use .iterator instead of .elements", "2.0.0-M3")
  final def elements(param: P)(implicit session: JdbcBackend#Session) = iterator(param)(session)

  /**
   * Execute the statement and return a CloseableIterator of the converted results.
   * The iterator must either be fully read or closed explicitly.
   *
   * @param maxRows Maximum number of rows to read from the result (0 for unlimited).
   */
  def iteratorTo(param: P, maxRows: Int)(implicit session: JdbcBackend#Session): CloseableIterator[R]
  @deprecated("Use .iteratorTo instead of .elementsTo", "2.0.0-M3")
  def elementsTo(param: P, maxRows: Int)(implicit session: JdbcBackend#Session) = iteratorTo(param, maxRows)(session)

  /**
   * Execute the statement and ignore the results.
   */
  final def execute(param: P)(implicit session: JdbcBackend#Session): Unit = iterator(param)(session).close()

  /**
   * Execute the statement and return the first row of the result set wrapped in
   * Some, or None if the result set is empty.
   */
  final def firstOption(param: P)(implicit session: JdbcBackend#Session): Option[R] = {
    var res: Option[R] = None
    foreach(param, { x => res = Some(x) }, 1)
    res
  }

  /**
   * Execute the statement and return the first row of the result set.
   * If the result set is empty, a NoSuchElementException is thrown.
   */
  final def first(param: P)(implicit session: JdbcBackend#Session): R =
    firstOption(param).getOrElse(throw new NoSuchElementException("Invoker.first"))

  /**
   * Execute the statement and return an immutable and fully
   * materialized list of the results.
   */
  final def list(param: P)(implicit session: JdbcBackend#Session) = build[List[R]](param)

  final def toMap[T, U](param: P)(implicit session: JdbcBackend#Session, ev: R <:< (T, U)): Map[T, U] =
    build[Map[T, U]](param)(session, implicitly[CanBuildFrom[Nothing, (T, U), Map[T, U]]].asInstanceOf[CanBuildFrom[Nothing, R, Map[T, U]]])

  /**
   * Execute the statement and return a fully materialized collection of the specified type.
   */
  final def build[To](param: P)(implicit session: JdbcBackend#Session, canBuildFrom: CanBuildFrom[Nothing, R, To]): To = {
    val b = canBuildFrom()
    foreach(param, { x => b += x }, 0)
    b.result()
  }

  /**
   * Execute the statement and return a fully materialized collection.
   */
  final def to[C[_]](param: P)(implicit session: JdbcBackend#Session, canBuildFrom: CanBuildFrom[Nothing, R, C[R @uV]]): C[R @uV] =
    build[C[R]](param)(session, canBuildFrom)

  /**
   * Execute the statement and call f for each converted row of the result set.
   */
  final def foreach(param: P, f: R => Unit)(implicit session: JdbcBackend#Session) {
    val it = iterator(param)
    try { it.foreach(f) } finally { it.close() }
  }

  /**
   * Execute the statement and call f for each converted row of the result set.
   *
   * @param maxRows Maximum number of rows to read from the result (0 for unlimited).
   */
  final def foreach(param: P, f: R => Unit, maxRows: Int)(implicit session: JdbcBackend#Session) {
    val it = iteratorTo(param, maxRows)
    try { it.foreach(f) } finally { it.close() }
  }

  /**
   * Execute the statement and left-fold the converted rows of the result set.
   */
  final def foldLeft[B](param: P, z: B)(op: (B, R) => B)(implicit session: JdbcBackend#Session): B = {
    var _z = z
    foreach(param, { e => _z = op(_z, e) })(session)
    _z
  }

  /**
   * Execute the statement and feed the converted rows of the result set into an iteratee.
   */
  final def enumerate[B, RR >: R](param: P, iter: IterV[RR,B])(implicit session: JdbcBackend#Session): IterV[RR, B] = {
    var _iter = iter
    val it = iterator(param)(session)
    try {
      while(it.hasNext && !_iter.isInstanceOf[Done[_,_]]) {
        val cont = _iter.asInstanceOf[Cont[RR,B]]
        _iter = cont.k(El(it.next))
      }
    } finally it.close()
    _iter
  }

  /**
   * Apply the parameter for this Invoker, creating a parameterless UnitInvoker.
   */
  override def apply(parameter: P): UnitInvoker[R] = new AppliedInvoker[P,R] {
    protected val appliedParameter = parameter
    protected val delegate = self
  }

  /**
   * Create a new Invoker which applies the mapping function f to each row
   * of the result set.
   */
  def mapResult[U](f: (R => U)): Invoker[P, U] = new MappedInvoker(this, f)

  /**
   * If the result type of this Invoker is of the form Option[T], execute the statement
   * and return the first row of the result set, or None if the result set is empty.
   */
  def firstFlatten[B](param: P)(implicit session: JdbcBackend#Session, ev: R <:< Option[B]): Option[B] =
    firstOption(param)/*.map(ev.apply _)*/.getOrElse(None).asInstanceOf[Option[B]]
}

/**
 * An invoker for a Unit parameter with additional parameterless methods.
 */
trait UnitInvoker[+R] extends Invoker[Unit, R] {
  protected type Param
  protected val appliedParameter: Param
  protected val delegate: Invoker[Param, R]

  final def firstOption(implicit session: JdbcBackend#Session): Option[R] = delegate.firstOption(appliedParameter)
  final def first()(implicit session: JdbcBackend#Session): R = delegate.first(appliedParameter)
  final def list()(implicit session: JdbcBackend#Session): List[R] = delegate.list(appliedParameter)
  final def to[C[_]](implicit session: JdbcBackend#Session, canBuildFrom: CanBuildFrom[Nothing, R, C[R @uV]]): C[R @uV] =
    delegate.to(appliedParameter)
  final def toMap[T, U](implicit session: JdbcBackend#Session, ev: R <:< (T, U)): Map[T, U] = delegate.toMap(appliedParameter)
  final def foreach(f: R => Unit)(implicit session: JdbcBackend#Session): Unit = delegate.foreach(appliedParameter, f)
  final def foreach(f: R => Unit, maxRows: Int)(implicit session: JdbcBackend#Session): Unit = delegate.foreach(appliedParameter, f, maxRows)
  final def iterator()(implicit session: JdbcBackend#Session): CloseableIterator[R] = delegate.iterator(appliedParameter)
  @deprecated("Use .iterator instead of .elements", "2.0.0-M3")
  final def elements()(implicit session: JdbcBackend#Session): CloseableIterator[R] = delegate.iterator(appliedParameter)
  final def iteratorTo(maxRows: Int)(implicit session: JdbcBackend#Session): CloseableIterator[R] = delegate.iteratorTo(appliedParameter, maxRows)
  @deprecated("Use .iteratorTo instead of .elementsTo", "2.0.0-M3")
  final def elementsTo(maxRows: Int)(implicit session: JdbcBackend#Session): CloseableIterator[R] = delegate.iteratorTo(appliedParameter, maxRows)
  final def execute()(implicit session: JdbcBackend#Session): Unit = delegate.execute(appliedParameter)
  final def foldLeft[B](z: B)(op: (B, R) => B)(implicit session: JdbcBackend#Session): B = delegate.foldLeft(appliedParameter, z)(op)
  final def enumerate[B, RR >: R](iter: IterV[RR,B])(implicit session: JdbcBackend#Session): IterV[RR, B] = delegate.enumerate(appliedParameter, iter)

  def firstFlatten[B](implicit session: JdbcBackend#Session, ev: R <:< Option[B]): Option[B] =
    firstOption/*.map(ev.apply _)*/.getOrElse(None).asInstanceOf[Option[B]]
  override def mapResult[U](f: (R => U)): UnitInvoker[U] = new MappedInvoker(this, f) with UnitInvokerMixin[U]
}

object UnitInvoker {
  val empty: UnitInvoker[Nothing] = new UnitInvokerMixin[Nothing] {
    def iteratorTo(param: Unit, maxRows: Int)(implicit session: JdbcBackend#Session) = CloseableIterator.empty
  }
}

trait UnitInvokerMixin[+R] extends UnitInvoker[R] {
  final protected val appliedParameter = ()
  protected val delegate = this
  protected type Param = Unit
}

/**
 * Base trait for applied invokers
 */
trait AppliedInvoker[P, +R] extends UnitInvoker[R] {
  protected type Param = P
  def iteratorTo(param: Unit, maxRows: Int)(implicit session: JdbcBackend#Session): CloseableIterator[R] = delegate.iteratorTo(appliedParameter, maxRows)
}

/**
 * An Invoker which applies a mapping function to all results of another Invoker.
 */
class MappedInvoker[-P, U, +R](parent: Invoker[P, U], mapper: (U => R)) extends Invoker[P, R] {
  def iteratorTo(param: P, maxRows: Int)(implicit session: JdbcBackend#Session) =
    parent.iteratorTo(param, maxRows).map(mapper)
}
