package org.scalaquery

import org.scalaquery.session.Session
import org.scalaquery.util.CloseableIterator

/**
 * An Invoker which applies a mapping function to all results of another Invoker.
 */
class MappedInvoker[-P, U, +R](parent: Invoker[P, U], mapper: (U => R)) extends Invoker[P, R] {

  def foreach(param: P, f: R => Unit, maxRows: Int)(implicit session: Session) =
    parent.foreach(param, { r:U => f(mapper(r)) }, maxRows)

  def elements(param: P)(implicit session: Session): CloseableIterator[R] = {
    val it = parent.elements(param)
    new CloseableIterator[R] {
      def hasNext = it.hasNext
      def next() = mapper(it.next())
      def close = it.close
    }
  }
}
