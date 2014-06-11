package scala.slick.jdbc

import scala.language.higherKinds
import scala.slick.util.CloseableIterator
import scala.slick.common.GenericInvoker

/** Base trait for all statement invokers of result element type R. */
trait Invoker[+R] extends GenericInvoker[R] { self =>
  type Session = JdbcBackend#Session

  /** Execute the statement and return a CloseableIterator of the converted
    * results. The iterator must either be fully read or closed explicitly. */
  final def iterator(implicit session: Session) = iteratorTo(0)


  /** Execute the statement and return the first row of the result set wrapped
    * in Some, or None if the result set is empty. */
  final def firstOption(implicit session: Session): Option[R] = {
    var res: Option[R] = None
    foreach({ x => res = Some(x) }, 1)
    res
  }

  /** Create a new Invoker which applies the mapping function f to each row of the result set. */
  def mapResult[U](f: (R => U)): Invoker[U] = new Invoker[U] {
    def iteratorTo(maxRows: Int)(implicit session: Session) = self.iteratorTo(maxRows).map(f)
  }

  /** If the result type of this Invoker is of the form Option[T], execute the statement
    * and return the first row of the result set, or None if the result set is empty. */
  @deprecated("Use .first.flatten instead of .firstFlatten", "2.1")
  def firstFlatten[B](implicit session: Session, ev: R <:< Option[B]): Option[B] =
    firstOption/*.map(ev.apply _)*/.getOrElse(None).asInstanceOf[Option[B]]
}

object Invoker {
  val empty: Invoker[Nothing] = new Invoker[Nothing] {
    def iteratorTo(maxRows: Int)(implicit session: Session) = CloseableIterator.empty
  }
}

/** A special kind of invoker that allows the result data to be mutated .*/
trait MutatingInvoker[R] extends Invoker[R] { self =>
  /** Transform a query's results with an updatable result set. */
  def mutate(f: ResultSetMutator[R] => Unit, end: ResultSetMutator[R] => Unit = null)(implicit session: Session): Unit
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
