package slick.jdbc

import java.sql.PreparedStatement

import scala.collection.compat.*
import scala.collection.mutable.ArrayBuffer

import slick.util.{CloseableIterator, SlickLogger, TableDump}

import org.slf4j.LoggerFactory

private[jdbc] object StatementInvoker {
  val maxLogResults = 5
  lazy val tableDump = new TableDump(20)
  lazy val resultLogger = new SlickLogger(LoggerFactory.getLogger(classOf[StatementInvoker[_]].getName+".result"))
}

/** An invoker which executes an SQL statement through JDBC. */
abstract class StatementInvoker[+R] extends Invoker[R] { self =>

  protected def getStatement: String
  protected def setParam(st: PreparedStatement): Unit
  override protected def debuggingId = Some(s"statement $getStatement")

  def iteratorTo(maxRows: Int)(implicit session: JdbcBackend#Session): CloseableIterator[R] =
    results(maxRows).fold(r => new CloseableIterator.Single[R](r.asInstanceOf[R]), identity)

  /** Invoke the statement and return the raw results. */
  def results(maxRows: Int,
              defaultType: ResultSetType = ResultSetType.ForwardOnly,
              defaultConcurrency: ResultSetConcurrency = ResultSetConcurrency.ReadOnly,
              defaultHoldability: ResultSetHoldability = ResultSetHoldability.Default,
              autoClose: Boolean = true)
             (implicit session: JdbcBackend#Session): Either[Int, PositionedResultIterator[R]] = {
    //TODO Support multiple results
    val statement = getStatement
    val fetchSizeOverride = if (maxRows == 1) Some(1) else None
    val st = session.prepareStatement(statement, defaultType, defaultConcurrency, defaultHoldability, fetchSizeOverride)
    setParam(st)
    var doClose = true
    try {
      st.setMaxRows(maxRows)
      val doLogResult = StatementInvoker.resultLogger.isDebugEnabled
      if(st.execute) {
        val resultSet = st.getResultSet
        val logHeader = if(doLogResult) {
          val meta = resultSet.getMetaData
          Vector(
            1.to(meta.getColumnCount).map(_.toString),
            1.to(meta.getColumnCount).map(idx => meta.getColumnLabel(idx)).to(ArrayBuffer)
          )
        } else null
        val logBuffer = if(doLogResult) new ArrayBuffer[ArrayBuffer[Any]] else null
        var rowCount = 0
        val pr = new PositionedResult(resultSet) {
          def close() = {
            this.rs.close()
            st.close()
            if(doLogResult) {
              StatementInvoker.tableDump(logHeader.toIndexedSeq.map(_.toIndexedSeq), logBuffer.toIndexedSeq.map(_.toIndexedSeq)).foreach(s => StatementInvoker.resultLogger.debug(s))
              val rest = rowCount - logBuffer.length
              if(rest > 0) StatementInvoker.resultLogger.debug(s"$rest more rows read ($rowCount total)")
            }
          }
        }
        val pri = new PositionedResultIterator[R](pr, maxRows, autoClose) {
          def extractValue(pr: PositionedResult) = {
            if(doLogResult) {
              if(logBuffer.length < StatementInvoker.maxLogResults)
                logBuffer += 1.to(logHeader(0).length).map(idx => rs.getObject(idx) : Any).to(ArrayBuffer)
              rowCount += 1
            }
            self.extractValue(pr)
          }
        }
        doClose = false
        Right(pri)
      } else {
        val count = st.getUpdateCount
        if(doLogResult) StatementInvoker.resultLogger.debug(s"$count rows affected")
        Left(count)
      }
    } finally if(doClose) st.close()
  }

  protected def extractValue(pr: PositionedResult): R
}
