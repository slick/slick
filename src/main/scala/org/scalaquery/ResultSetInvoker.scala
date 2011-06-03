package org.scalaquery

import java.sql.ResultSet
import org.scalaquery.session._
import org.scalaquery.util.CloseableIterator

/**
 * An invoker which calls a function to retrieve a ResultSet. This can be used
 * for reading information from a java.sql.DatabaseMetaData object which has
 * many methods that return ResultSets.
 * 
 * For convenience, if the function returns null, this is treated like an
 * empty ResultSet.
 */
abstract class ResultSetInvoker[+R] extends UnitInvokerMixin[R] with ResultSetInvokerMixin[R] { self =>

  protected def createResultSet(session: Session): ResultSet

  def foreach(param: Unit, f: R => Unit, maxRows: Int)(implicit session: Session) =
    createPR(session).foreach(prForeach(_, f, maxRows))

  def elements(param: Unit)(implicit session: Session): CloseableIterator[R] = {
    createPR(session) map prElements getOrElse CloseableIterator.empty
  }

  private[this] def createPR(session: Session) =
    Option(createResultSet(session)) map { new PositionedResult(_) { def close() = rs.close() } }
}

object ResultSetInvoker {
  def apply[R](f: Session => ResultSet)(implicit conv: PositionedResult => R): UnitInvoker[R] = new ResultSetInvoker[R] {
    def createResultSet(session: Session) = f(session)
    def extractValue(pr: PositionedResult) = conv (pr)
  }
}

trait ResultSetInvokerMixin[+R] {

  protected final def prForeach(pr: PositionedResult, f: R => Unit, maxRows: Int): Unit = try {
    var count = 0
    while(pr.next && (maxRows == 0 || count < maxRows)) {
      f(extractValue(pr))
      count += 1
    }
  } finally { pr.close() }

  protected final def prElements(pr: PositionedResult): CloseableIterator[R] = new CloseableIterator.ReadAhead[R] {
    def close() = pr.close()
    protected def fetchNext() = {
      if(pr.next) Some(extractValue(pr))
      else { close(); None }
    }
  }

  protected def extractValue(pr: PositionedResult): R
}
