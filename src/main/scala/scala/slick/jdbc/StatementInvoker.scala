package scala.slick.jdbc

import java.sql.PreparedStatement
import scala.slick.util.CloseableIterator
import scala.slick.SlickException

/** An invoker which executes an SQL statement through JDBC. */
abstract class StatementInvoker[+R] extends Invoker[R] { self =>

  protected def getStatement: String
  protected def setParam(st: PreparedStatement): Unit

  def iteratorTo(maxRows: Int)(implicit session: JdbcBackend#Session): CloseableIterator[R] =
    results(maxRows).fold(r => new CloseableIterator.Single[R](r.asInstanceOf[R]), identity)

  /** Invoke the statement and return the raw results. */
  def results(maxRows: Int,
              defaultType: ResultSetType = ResultSetType.ForwardOnly,
              defaultConcurrency: ResultSetConcurrency = ResultSetConcurrency.ReadOnly,
              defaultHoldability: ResultSetHoldability = ResultSetHoldability.Default)
             (implicit session: JdbcBackend#Session): Either[Int, PositionedResultIterator[R]] = {
    //TODO Support multiple results
    val statement = getStatement
    val st = session.prepareStatement(statement, defaultType, defaultConcurrency, defaultHoldability)
    setParam(st)
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

trait MutatingStatementInvoker[R] extends StatementInvoker[R] with MutatingInvoker[R] {

  protected def updateRowValues(rs: PositionedResult, value: R)
  protected val mutateConcurrency: ResultSetConcurrency = ResultSetConcurrency.Updatable
  protected val mutateType: ResultSetType = ResultSetType.Auto
  protected val previousAfterDelete = false

  def mutate(f: ResultSetMutator[R] => Unit, end: ResultSetMutator[R] => Unit)(implicit session: JdbcBackend#Session): Unit =
    session.withTransaction {
      /* Hsqldb forces ResultSets to be read-only in auto-commit mode, so we
       * use an explicit transaction. It shouldn't hurt other databases. */
      results(0, defaultConcurrency = mutateConcurrency, defaultType = mutateType).fold(
        _ => throw new SlickException("Cannot transform an update result"),
        pr => try {
          val rs = pr.rs
          var current: R = null.asInstanceOf[R]
          val mu = new ResultSetMutator[R] {
            def row = current
            def row_=(value: R) {
              pr.restart
              updateRowValues(pr, value)
              rs.updateRow()
            }
            def insert(value: R) {
              rs.moveToInsertRow()
              pr.restart
              updateRowValues(pr, value)
              rs.insertRow()
              rs.moveToCurrentRow()
            }
            def delete() {
              rs.deleteRow()
              if(previousAfterDelete) rs.previous()
            }
          }
          while(pr.nextRow) {
            current = extractValue(pr)
            f(mu)
          }
          if(end ne null) {
            end(new ResultSetMutator[R] {
              def row = throw new SlickException("After end of result set")
              def row_=(value: R) = throw new SlickException("After end of result set")
              def delete() = throw new SlickException("After end of result set")
              def insert(value: R) {
                rs.moveToInsertRow()
                pr.restart
                updateRowValues(pr, value)
                rs.insertRow()
              }
            })
          }
        } finally { pr.close() }
      )
    }
}
