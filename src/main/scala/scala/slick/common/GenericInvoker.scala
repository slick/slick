package scala.slick.common

import scala.language.higherKinds
import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.collection.immutable.Map
import scala.collection.generic.CanBuildFrom
import scala.slick.util.CloseableIterator
import scala.slick.util.iter._
import scala.slick.backend.DatabaseComponent

/** Base trait for all statement invokers of result element type R. */
trait GenericInvoker[+R] { self =>
  type Session <: DatabaseComponent#SessionDef

  /** Execute the statement and return a CloseableIterator of the converted
    * results. The iterator must either be fully read or closed explicitly. */
  def iterator(implicit session: Session): CloseableIterator[R]

  /** Execute the statement and return a CloseableIterator of the converted
    * results. The iterator must either be fully read or closed explicitly.
    * @param maxRows Maximum number of rows to read from the result (0 for unlimited). */
  def iteratorTo(maxRows: Int)(implicit session: Session): CloseableIterator[R]

  /** Execute the statement and ignore the results. */
  final def execute(implicit session: Session): Unit = iterator(session).close()

  /** Execute the statement and return the first row of the result set wrapped
    * in Some, or None if the result set is empty. */
  def firstOption(implicit session: Session): Option[R]

  /** Execute the statement and return the first row of the result set.
    * If the result set is empty, a NoSuchElementException is thrown. */
  final def first(implicit session: Session): R =
    firstOption.getOrElse(throw new NoSuchElementException("Invoker.first"))

  /** Execute the statement and return an immutable and fully materialized list of the results. */
  final def list(implicit session: Session) = build[List[R]]

  final def toMap[T, U](implicit session: Session, ev: R <:< (T, U)): Map[T, U] =
    build[Map[T, U]](session, implicitly[CanBuildFrom[Nothing, (T, U), Map[T, U]]].asInstanceOf[CanBuildFrom[Nothing, R, Map[T, U]]])

  /** Execute the statement and return a fully materialized collection of the specified type. */
  final def build[To](implicit session: Session, canBuildFrom: CanBuildFrom[Nothing, R, To]): To = {
    val b = canBuildFrom()
    foreach({ x => b += x }, 0)
    b.result()
  }

  /** Execute the statement and return a fully materialized collection. */
  final def buildColl[C[_]](implicit session: Session, canBuildFrom: CanBuildFrom[Nothing, R, C[R @uV]]): C[R @uV] =
    build[C[R]](session, canBuildFrom)

  /** Execute the statement and call f for each converted row of the result set. */
  final def foreach(f: R => Unit)(implicit session: Session) {
    val it = iterator
    try { it.foreach(f) } finally { it.close() }
  }

  /** Execute the statement and call f for each converted row of the result set.
    * @param maxRows Maximum number of rows to read from the result (0 for unlimited). */
  final def foreach(f: R => Unit, maxRows: Int)(implicit session: Session) {
    val it = iteratorTo(maxRows)
    try { it.foreach(f) } finally { it.close() }
  }

  /** Execute the statement and left-fold the converted rows of the result set. */
  final def foldLeft[B](z: B)(op: (B, R) => B)(implicit session: Session): B = {
    var _z = z
    foreach({ e => _z = op(_z, e) })(session)
    _z
  }

  /** Execute the statement and feed the converted rows of the result set into an iteratee. */
  final def enumerate[B, RR >: R](iter: IterV[RR,B])(implicit session: Session): IterV[RR, B] = {
    var _iter = iter
    val it = iterator(session)
    try {
      while(it.hasNext && !_iter.isInstanceOf[Done[_,_]]) {
        val cont = _iter.asInstanceOf[Cont[RR,B]]
        _iter = cont.k(El(it.next()))
      }
    } finally it.close()
    _iter
  }
}