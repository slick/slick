package org.scalaquery

import java.sql.PreparedStatement
import org.scalaquery.session._
import org.scalaquery.util.CloseableIterator

/**
 * An invoker which executes an SQL statement.
 */
abstract class StatementInvoker[-P, +R] extends Invoker[P, R] { self =>

  protected def getStatement: String

  protected def setParam(param: P, st: PreparedStatement): Unit

  protected def extractValue(rs: PositionedResult): R

  def foreach(param: P, f: R => Unit, maxRows: Int)(implicit session: Session): Unit = results(param, maxRows) match {
    case Left(r) => f(r.asInstanceOf[R])
    case Right(rs) => try {
      var count = 0
      while(rs.next && (maxRows == 0 || count < maxRows)) {
        f(extractValue(rs))
        count += 1
      }
    } finally { rs.close() }
  }

  def elements(param: P)(implicit session: Session): CloseableIterator[R] = results(param, 0) match {
    case Left(r) => new CloseableIterator[R] {
      private var hasnext = true
      def hasNext: Boolean = hasnext
      def next(): R =
        if (hasnext) { hasnext = false; r.asInstanceOf[R] }
        else throw new NoSuchElementException("next on empty iterator")
      def close {}
    }
    case Right(rs) => new ReadAheadIterator[R] with CloseableIterator[R] {
      def close() = rs.close()
      protected def fetchNext() = {
        if(rs.next) Some(extractValue(rs))
        else { close(); None }
      }
    }
  }

  override def execute(param: P)(implicit session: Session): Unit = results(param, 1).right.foreach(_.close())

  /**
   * Invoke the statement and return the raw results.
   */
  def results(param: P, maxRows: Int,
        defaultType: ResultSetType = ResultSetType.ForwardOnly,
        defaultConcurrency: ResultSetConcurrency = ResultSetConcurrency.ReadOnly,
        defaultHoldability: ResultSetHoldability = ResultSetHoldability.Default)
      (implicit session: Session): Either[Int, PositionedResult] = {
    //TODO Support multiple results
    val statement = getStatement
    val st = session.prepareStatement(statement, defaultType, defaultConcurrency, defaultHoldability)
    setParam(param, st)
    var doClose = true
    try {
      st.setMaxRows(maxRows)
      if(st.execute) {
        val rs = new PositionedResult(st.getResultSet) { def close() = st.close() }
        doClose = false
        Right(rs)
      } else Left(st.getUpdateCount)
    } finally if(doClose) st.close()
  }
}
