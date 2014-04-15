package scala.slick.jdbc

import scala.language.higherKinds
import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.collection.immutable.Map
import scala.collection.generic.CanBuildFrom
import scala.slick.util.CloseableIterator
import scala.slick.util.iter._

/** Base trait for all statement invokers of result element type R. */
trait Invoker[+R] { self =>

  /** Execute the statement and return a CloseableIterator of the converted
    * results. The iterator must either be fully read or closed explicitly. */
  final def iterator(implicit session: JdbcBackend#Session) = iteratorTo(0)

  /** Execute the statement and return a CloseableIterator of the converted
    * results. The iterator must either be fully read or closed explicitly.
    * @param maxRows Maximum number of rows to read from the result (0 for unlimited). */
  def iteratorTo(maxRows: Int)(implicit session: JdbcBackend#Session): CloseableIterator[R]

  /** Execute the statement and ignore the results. */
  final def execute(implicit session: JdbcBackend#Session): Unit = iterator(session).close()

  /** Execute the statement and return the first row of the result set wrapped
    * in Some, or None if the result set is empty. */
  final def firstOption(implicit session: JdbcBackend#Session): Option[R] = {
    var res: Option[R] = None
    foreach({ x => res = Some(x) }, 1)
    res
  }

  /** Execute the statement and return the first row of the result set.
    * If the result set is empty, a NoSuchElementException is thrown. */
  final def first(implicit session: JdbcBackend#Session): R =
    firstOption.getOrElse(throw new NoSuchElementException("Invoker.first"))

  /** Execute the statement and return an immutable and fully materialized list of the results. */
  final def list(implicit session: JdbcBackend#Session) = build[List[R]]

  final def toMap[T, U](implicit session: JdbcBackend#Session, ev: R <:< (T, U)): Map[T, U] =
    build[Map[T, U]](session, implicitly[CanBuildFrom[Nothing, (T, U), Map[T, U]]].asInstanceOf[CanBuildFrom[Nothing, R, Map[T, U]]])

  /** Execute the statement and return a fully materialized collection of the specified type. */
  final def build[To](implicit session: JdbcBackend#Session, canBuildFrom: CanBuildFrom[Nothing, R, To]): To = {
    val b = canBuildFrom()
    foreach({ x => b += x }, 0)
    b.result()
  }

  /** Execute the statement and return a fully materialized collection. */
  final def buildColl[C[_]](implicit session: JdbcBackend#Session, canBuildFrom: CanBuildFrom[Nothing, R, C[R @uV]]): C[R @uV] =
    build[C[R]](session, canBuildFrom)

  /** Execute the statement and call f for each converted row of the result set. */
  final def foreach(f: R => Unit)(implicit session: JdbcBackend#Session) {
    val it = iterator
    try { it.foreach(f) } finally { it.close() }
  }

  /** Execute the statement and call f for each converted row of the result set.
   * @param maxRows Maximum number of rows to read from the result (0 for unlimited). */
  final def foreach(f: R => Unit, maxRows: Int)(implicit session: JdbcBackend#Session) {
    val it = iteratorTo(maxRows)
    try { it.foreach(f) } finally { it.close() }
  }

  /** Execute the statement and left-fold the converted rows of the result set. */
  final def foldLeft[B](z: B)(op: (B, R) => B)(implicit session: JdbcBackend#Session): B = {
    var _z = z
    foreach({ e => _z = op(_z, e) })(session)
    _z
  }

  /** Execute the statement and feed the converted rows of the result set into an iteratee. */
  final def enumerate[B, RR >: R](iter: IterV[RR,B])(implicit session: JdbcBackend#Session): IterV[RR, B] = {
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

  /** Create a new Invoker which applies the mapping function f to each row of the result set. */
  def mapResult[U](f: (R => U)): Invoker[U] = new Invoker[U] {
    def iteratorTo(maxRows: Int)(implicit session: JdbcBackend#Session) = self.iteratorTo(maxRows).map(f)
  }

  /** If the result type of this Invoker is of the form Option[T], execute the statement
    * and return the first row of the result set, or None if the result set is empty. */
  @deprecated("Use .first.flatten instead of .firstFlatten", "2.1")
  def firstFlatten[B](implicit session: JdbcBackend#Session, ev: R <:< Option[B]): Option[B] =
    firstOption/*.map(ev.apply _)*/.getOrElse(None).asInstanceOf[Option[B]]
}

object Invoker {
  val empty: Invoker[Nothing] = new Invoker[Nothing] {
    def iteratorTo(maxRows: Int)(implicit session: JdbcBackend#Session) = CloseableIterator.empty
  }
}

/** A special kind of invoker that allows the result data to be mutated .*/
trait MutatingInvoker[R] extends Invoker[R] { self =>
  /** Transform a query's results with an updatable result set. */
  def mutate(f: ResultSetMutator[R] => Unit, end: ResultSetMutator[R] => Unit = null)(implicit session: JdbcBackend#Session): Unit
}

trait ResultSetMutator[T] {
  /** Get the current row's value. */
  def row: T
  /** Update the current row. */
  def row_=(value: T)
  /** Insert a new row. */
  def insert(value: T)
  /** Delete the current row. */
  def delete(): Unit
}
