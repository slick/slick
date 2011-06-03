package org.scalaquery

import java.sql.PreparedStatement
import org.scalaquery.session._
import org.scalaquery.util.CloseableIterator

/**
 * An invoker which executes an SQL statement.
 */
abstract class StatementInvoker[-P, +R] extends Invoker[P, R] with ResultSetInvokerMixin[R] { self =>

  protected def getStatement: String

  protected def setParam(param: P, st: PreparedStatement): Unit

  def foreach(param: P, f: R => Unit, maxRows: Int)(implicit session: Session) =
    results(param, maxRows).fold(r => f(r.asInstanceOf[R]), prForeach(_, f, maxRows))

  def elements(param: P)(implicit session: Session): CloseableIterator[R] =
    results(param, 0).fold(r => new CloseableIterator.Single[R](r.asInstanceOf[R]), prElements)

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
