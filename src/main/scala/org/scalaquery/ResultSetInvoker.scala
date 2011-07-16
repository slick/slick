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
abstract class ResultSetInvoker[+R] extends UnitInvokerMixin[R] { self =>

  protected def createResultSet(session: Session): ResultSet

  def elementsTo(param: Unit, maxRows: Int)(implicit session: Session): CloseableIterator[R] = {
    val rs = createResultSet(session)
    if(rs eq null) CloseableIterator.empty
    else new PositionedResultIterator[R](rs, maxRows) {
      def closeUnderlying() = rs.close()
      def extractValue() = self.extractValue(this)
    }
  }

  protected def extractValue(pr: PositionedResult): R
}

object ResultSetInvoker {
  def apply[R](f: Session => ResultSet)(implicit conv: PositionedResult => R): UnitInvoker[R] = new ResultSetInvoker[R] {
    def createResultSet(session: Session) = f(session)
    def extractValue(pr: PositionedResult) = conv (pr)
  }
}
