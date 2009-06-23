package com.novocode.squery

import java.sql.PreparedStatement
import com.novocode.squery.session.{Session, PositionedResult, ReadAheadIterator, CloseableIterator}

/**
 * Base trait for all query invokers, using bare result type T, mapped result type R and parameter type P
 */
trait QueryInvoker[T,+R,-P] {

  protected def getStatement: String

  protected def setParam(param: P, st: PreparedStatement)

  protected val mapper: (T => R)

  protected def extractValue(rs: PositionedResult): T

  private def convertResult(rs: PositionedResult): R = mapper(extractValue(rs))

  def mapResult[U](f: (R => U))

  def apply(param: P)(implicit session: Session): R = first(param)

  def firstOption(param: P)(implicit session: Session): Option[R] = {
    var res: Option[R] = None
    foreach(param, { x => res = Some(x) }, 1)
    res
  }

  def first(param: P)(implicit session: Session): R = firstOption(param) match {
    case None => throw new Predef.NoSuchElementException
    case Some(res) => res
  }

  def list(param: P)(implicit session: Session): List[R] = {
    var xs:List[R] = Nil
    foreach(param, { x => xs = x :: xs }, 0)
    xs
  }

  def foreach(param: P, f: R => Unit)(implicit session: Session): Unit = foreach(param, f, 0)

  private[this] def foreach(param: P, f: R => Unit, maxRows: Int)(implicit session: Session): Unit = {
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
          f(convertResult(rs))
          count += 1
        }
      } else f(mapper(st.getUpdateCount.asInstanceOf[T]))
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
            if(rs.next) Some(convertResult(rs))
            else { close(); None }
          }
        }
      } else {
        val r = mapper(st.getUpdateCount.asInstanceOf[T])
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
