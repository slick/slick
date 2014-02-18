package scala.slick.jdbc

import java.sql.ResultSet
import scala.slick.util.CloseableIterator

/**
 * An invoker which calls a function to retrieve a ResultSet. This can be used
 * for reading information from a java.sql.DatabaseMetaData object which has
 * many methods that return ResultSets.
 * 
 * For convenience, if the function returns null, this is treated like an
 * empty ResultSet.
 */
abstract class ResultSetInvoker[+R] extends Invoker[R] { self =>

  protected def createResultSet(session: JdbcBackend#Session): ResultSet

  def iteratorTo(maxRows: Int)(implicit session: JdbcBackend#Session): CloseableIterator[R] = {
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
  def apply[R](f: JdbcBackend#Session => ResultSet)(implicit conv: PositionedResult => R): Invoker[R] = new ResultSetInvoker[R] {
    def createResultSet(session: JdbcBackend#Session) = f(session)
    def extractValue(pr: PositionedResult) = conv (pr)
  }
}
