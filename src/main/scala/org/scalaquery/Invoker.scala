package org.scalaquery

import scala.collection.mutable.ListBuffer
import org.scalaquery.session.Session
import org.scalaquery.util.CloseableIterator

/**
 * Base trait for all statement invokers, using parameter type P and result type R.
 */
trait Invoker[-P, +R] { self =>

  /**
   * Execute the statement and call f for each converted row of the result set.
   * 
   * @param maxRows Maximum number of rows to read from the result (0 for unlimited).
   */
  def foreach(param: P, f: R => Unit, maxRows: Int)(implicit session: Session): Unit

  /**
   * Execute the statement and return a CloseableIterator of the converted results.
   * The iterator must either be fully read or closed explicitly.
   */
  def elements(param: P)(implicit session: Session): CloseableIterator[R]

  /**
   * Execute the statement and ignore the results.
   */
  def execute(param: P)(implicit session: Session): Unit = elements(param)(session).close()

  /**
   * Execute the statement and return the first row of the result set wrapped in
   * Some, or None if the result set is empty.
   */
  final def firstOption(param: P)(implicit session: Session): Option[R] = {
    var res: Option[R] = None
    foreach(param, { x => res = Some(x) }, 1)
    res
  }

  /**
   * Execute the statement and return the first row of the result set.
   * If the result set is empty, a NoSuchElementException is thrown.
   */
  final def first(param: P)(implicit session: Session): R =
    firstOption(param).getOrElse(throw new NoSuchElementException("Invoker.first"))

  /**
   * Execute the statement and return an immutable and fully
   * materialized list of the results.
   */
  final def list(param: P)(implicit session: Session): List[R] = {
    val b = new ListBuffer[R]
    foreach(param, { x => b += x }, 0)
    b.toList
  }

  /**
   * Execute the statement and call f for each converted row of the result set.
   */
  final def foreach(param: P, f: R => Unit)(implicit session: Session): Unit = foreach(param, f, 0)

  /**
   * Execute the statement and left-fold the converted rows of the result set.
   */
  final def foldLeft[B](param: P, z: B)(op: (B, R) => B)(implicit session: Session): B = {
    var _z = z
    foreach(param, { e => _z = op(_z, e) })(session)
    _z
  }

  /**
   * Apply the parameter for this Invoker, creating a parameterless UnitInvoker.
   */
  def apply(parameter: P): UnitInvoker[R] = new AppliedInvoker[P,R] {
    protected val appliedParameter = parameter
    protected val delegate = self
  }

  /**
   * Create a new Invoker which applies the mapping function f to each row
   * of the result set.
   */
  def mapResult[U](f: (R => U)): Invoker[P, U] = new MappedInvoker(this, f) with Invoker[P, U]

  /**
   * If the result type of this Invoker is of the form Option[T], execute the statement
   * and return the first row of the result set, or None if the result set is empty.
   */
  def firstFlatten[B](param: P)(implicit session: Session, ev: R <:< Option[B]): Option[B] =
    firstOption(param)/*.map(ev.apply _)*/.getOrElse(None).asInstanceOf[Option[B]]
}

/**
 * An invoker for a Unit parameter with additional parameterless methods.
 */
trait UnitInvoker[+R] extends Invoker[Unit, R] {
  def firstOption(implicit session: Session): Option[R]
  def first()(implicit session: Session): R
  def list()(implicit session: Session): List[R]
  def foreach(f: R => Unit)(implicit session: Session): Unit
  def foreach(f: R => Unit, maxRows: Int)(implicit session: Session): Unit
  def elements()(implicit session: Session): CloseableIterator[R]
  def execute()(implicit session: Session): Unit
  def foldLeft[B](z: B)(op: (B, R) => B)(implicit session: Session): B
  def firstFlatten[B](implicit session: Session, ev: R <:< Option[B]): Option[B] =
    firstOption/*.map(ev.apply _)*/.getOrElse(None).asInstanceOf[Option[B]]
  override def mapResult[U](f: (R => U)): UnitInvoker[U] = new MappedInvoker(this, f) with UnitInvokerMixin[U]
}

object UnitInvoker {
  val empty: UnitInvoker[Nothing] = new UnitInvokerMixin[Nothing] {
    def foreach(param: Unit, f: Nothing => Unit, maxRows: Int)(implicit session: Session) {}
    def elements(param: Unit)(implicit session: Session) = CloseableIterator.empty
  }
}

trait DelegatingUnitInvoker[P, +R] extends UnitInvoker[R] {
  protected val appliedParameter: P
  protected val delegate: Invoker[P, R]

  final def firstOption(implicit session: Session): Option[R] = delegate.firstOption(appliedParameter)
  final def first()(implicit session: Session): R = delegate.first(appliedParameter)
  final def list()(implicit session: Session): List[R] = delegate.list(appliedParameter)
  final def foreach(f: R => Unit)(implicit session: Session): Unit = delegate.foreach(appliedParameter, f)
  final def foreach(f: R => Unit, maxRows: Int)(implicit session: Session): Unit = delegate.foreach(appliedParameter, f, maxRows)
  final def elements()(implicit session: Session): CloseableIterator[R] = delegate.elements(appliedParameter)
  final def execute()(implicit session: Session): Unit = delegate.execute(appliedParameter)
  final def foldLeft[B](z: B)(op: (B, R) => B)(implicit session: Session): B = delegate.foldLeft(appliedParameter, z)(op)
}

trait UnitInvokerMixin[+R] extends DelegatingUnitInvoker[Unit, R] {
  final protected val appliedParameter = ()
  protected val delegate = this
}

/**
 * Base trait for applied invokers
 */
trait AppliedInvoker[P, +R] extends DelegatingUnitInvoker[P, R] {
  def foreach(param: Unit, f: R => Unit, maxRows: Int)(implicit session: Session): Unit = delegate.foreach(appliedParameter, f, maxRows)
  def elements(param: Unit)(implicit session: Session): CloseableIterator[R] = delegate.elements(appliedParameter)
}
