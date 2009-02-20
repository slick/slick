package com.novocode.squery

import java.sql.PreparedStatement
import com.novocode.squery.session.{Session, PositionedResult, ReadAheadIterator, CloseableIterator}

/**
 * An invoker which executes an SQL statement.
 */
abstract class StatementInvoker[-P, +R] extends Invoker[P, R] {

  protected def getStatement: String

  protected def setParam(param: P, st: PreparedStatement): Unit

  protected def extractValue(rs: PositionedResult): R

  def foreach(param: P, f: R => Unit, maxRows: Int)(implicit session: Session): Unit = {
    //TODO Support multiple results
    val statement = getStatement
    val st = session.allocPS(statement)
    setParam(param, st)
    try {
      st.setMaxRows(maxRows)
      if(st.execute) {
        var count = 0
        val rs = new PositionedResult(st.getResultSet)
        while(rs.next && (maxRows == 0 || count < maxRows)) {
          f(extractValue(rs))
          count += 1
        }
      } else f(st.getUpdateCount.asInstanceOf[R])
    } finally session.freePS(statement, st)
  }

  def elements(param: P)(implicit session: Session): CloseableIterator[R] = {
    //TODO Support multiple results
    val statement = getStatement
    val st = session.allocPS(statement)
    setParam(param, st)
    var doClose = true
    try {
      st.setMaxRows(0)
      if(st.execute) {
        val rs = new PositionedResult(st.getResultSet)
        doClose = false
        new ReadAheadIterator[R] with CloseableIterator[R] {
          def close() = session.freePS(statement, st)
          protected def fetchNext() = {
            if(rs.next) Some(extractValue(rs))
            else { close(); None }
          }
        }
      } else {
        val r = st.getUpdateCount.asInstanceOf[R]
        new CloseableIterator[R] {
          private var hasnext = true
          def hasNext: Boolean = hasnext
          def next(): R =
            if (hasnext) { hasnext = false; r }
            else throw new NoSuchElementException("next on empty iterator")
          def close {}
        }
      }
    } finally if(doClose) session.freePS(statement, st)
  }
}
