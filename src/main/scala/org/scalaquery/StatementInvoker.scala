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

  def elementsTo(param: P, maxRows: Int)(implicit session: Session): CloseableIterator[R] =
    results(param, maxRows).fold(r => new CloseableIterator.Single[R](r.asInstanceOf[R]), identity)

  /**
   * Invoke the statement and return the raw results.
   */
  def results(param: P, maxRows: Int,
        defaultType: ResultSetType = ResultSetType.ForwardOnly,
        defaultConcurrency: ResultSetConcurrency = ResultSetConcurrency.ReadOnly,
        defaultHoldability: ResultSetHoldability = ResultSetHoldability.Default)
      (implicit session: Session): Either[Int, PositionedResultIterator[R]] = {
    //TODO Support multiple results
    val statement = getStatement
    val st = session.prepareStatement(statement, defaultType, defaultConcurrency, defaultHoldability)
    setParam(param, st)
    var doClose = true
    try {
      st.setMaxRows(maxRows)
      if(st.execute) {
        val rs = new PositionedResultIterator[R](st.getResultSet, maxRows) {
          def closeUnderlying() = st.close()
          def extractValue() = self.extractValue(this)
        }
        doClose = false
        Right(rs)
      } else Left(st.getUpdateCount)
    } finally if(doClose) st.close()
  }

  protected def extractValue(pr: PositionedResult): R
}
