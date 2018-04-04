package slick.jdbc

import scala.language.higherKinds
import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.collection.generic.CanBuildFrom
import slick.util.CloseableIterator

/** Base trait for all statement invokers of result element type R. */
trait Invoker[+R] { self =>

  /** Execute the statement and return a CloseableIterator of the converted
    * results. The iterator must either be fully read or closed explicitly.
    * @param maxRows Maximum number of rows to read from the result (0 for unlimited). */
  def iteratorTo(maxRows: Int)(implicit session: JdbcBackend#Session): CloseableIterator[R]

  /** Execute the statement and ignore the results. */
  final def execute(implicit session: JdbcBackend#Session): Unit = iteratorTo(0)(session).close()

  /** Execute the statement and return the first row of the result set wrapped
    * in Some, or None if the result set is empty. */
  final def firstOption(implicit session: JdbcBackend#Session): Option[R] = {
    var res: Option[R] = None
    foreach({ x => res = Some(x) }, 1)
    res
  }

  /** Execute the statement and return the first row of the result set.
    * If the result set is empty, a NoSuchElementException is thrown. */
  final def first(implicit session: JdbcBackend#Session): R = {
    val it = iteratorTo(0)
    try {
      if(it.hasNext) it.next()
      else throw new NoSuchElementException("Invoker.first")
    } finally it.close
  }

  /** Execute the statement and return a fully materialized collection. */
  final def buildColl[C[_]](implicit session: JdbcBackend#Session, canBuildFrom: CanBuildFrom[Nothing, R, C[R @uV]]): C[R @uV] = {
    val b = canBuildFrom()
    foreach({ x => b += x }, 0)
    b.result()
  }

  /** Execute the statement and call f for each converted row of the result set.
   * @param maxRows Maximum number of rows to read from the result (0 for unlimited). */
  final def foreach(f: R => Unit, maxRows: Int = 0)(implicit session: JdbcBackend#Session): Unit = {
    val it = iteratorTo(maxRows)
    try { it.foreach(f) } finally { it.close() }
  }
}

trait ResultSetMutator[T] {
  /** Get the current row's value. Throws a [[slick.SlickException]] when positioned after
    * the end of the result set. */
  def row: T
  /** Update the current row. */
  def row_=(value: T)
  /** Insert a new row. */
  def += (value: T): Unit
  /** Insert multiple new rows. */
  def ++= (values: Seq[T]): Unit = values.foreach(v => += (v))
  /** Delete the current row. */
  def delete: Unit
  /** Check if the end of the result set has been reached. */
  def end: Boolean
}
