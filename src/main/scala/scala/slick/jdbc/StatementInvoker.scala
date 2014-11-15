package scala.slick.jdbc

import java.sql.PreparedStatement
import scala.slick.util.{TableDump, SlickLogger, CloseableIterator}
import scala.slick.SlickException
import org.slf4j.LoggerFactory
import scala.collection.mutable.ArrayBuffer

private[jdbc] object StatementInvoker {
  val maxLogResults = 5
  lazy val tableDump = new TableDump(20)
  lazy val resultLogger = new SlickLogger(LoggerFactory.getLogger(classOf[StatementInvoker[_]].getName+".result"))
}

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
      val doLogResult = StatementInvoker.resultLogger.isDebugEnabled
      if(st.execute) {
        val rs = st.getResultSet
        val logHeader = if(doLogResult) {
          val meta = rs.getMetaData
          Vector(
            1.to(meta.getColumnCount).map(_.toString),
            1.to(meta.getColumnCount).map(idx => meta.getColumnLabel(idx)).to[ArrayBuffer]
          )
        } else null
        val logBuffer = if(doLogResult) new ArrayBuffer[ArrayBuffer[Any]] else null
        var rowCount = 0
        val pr = new PositionedResult(rs) {
          def close() = {
            st.close()
            if(doLogResult) {
              StatementInvoker.tableDump(logHeader, logBuffer).foreach(s => StatementInvoker.resultLogger.debug(s))
              val rest = rowCount - logBuffer.length
              if(rest > 0) StatementInvoker.resultLogger.debug(s"$rest more rows read ($rowCount total)")
            }
          }
        }
        val pri = new PositionedResultIterator[R](pr, maxRows) {
          def extractValue(pr: PositionedResult) = {
            if(doLogResult) {
              if(logBuffer.length < StatementInvoker.maxLogResults)
                logBuffer += 1.to(logHeader(0).length).map(idx => rs.getObject(idx) : Any).to[ArrayBuffer]
              rowCount += 1
            }
            self.extractValue(pr)
          }
        }
        doClose = false
        Right(pri)
      } else {
        val count = st.getUpdateCount
        if(doLogResult) StatementInvoker.resultLogger.debug(count+" rows affected")
        Left(count)
      }
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
        prit => try {
          val pr = prit.pr
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
        } finally { prit.close() }
      )
    }
}
