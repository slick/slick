package slick.jdbc

import java.sql.{PreparedStatement, Statement}

import scala.collection.mutable.ArrayBuffer

import slick.SlickException
import slick.compat.collection.*
import slick.util.{CloseableIterator, SlickLogger, TableDump}

import org.slf4j.LoggerFactory

private[jdbc] object StatementInvoker {
  val maxLogResults = 5
  lazy val tableDump = new TableDump(20)
  lazy val resultLogger = new SlickLogger(LoggerFactory.getLogger(classOf[StatementInvoker[?]].getName+".result"))

  /** Move past the results that follow the first one. Some drivers (e.g. SQL Server) report the
    * errors of later statements in a multi-statement string only when the caller gets to them. */
  def skipMoreResults(st: Statement): Unit =
    while (st.getMoreResults || st.getUpdateCount != -1) {}
}

/** An invoker which executes an SQL statement through JDBC. */
abstract class StatementInvoker[+R] extends Invoker[R] { self =>

  protected def getStatement: String
  protected def setParam(st: PreparedStatement): Unit
  override protected def debuggingId: Option[String] = Some(s"statement $getStatement")

  /** Whether `iteratorTo` returns the update count of a statement that does not return a ResultSet as
    * the single element. Only invokers whose `R` is `Int` and which expect an update count may allow it. */
  protected def acceptUpdateCount: Boolean = false

  def iteratorTo(maxRows: Int)(implicit session: JdbcBackend#JdbcSessionDef): CloseableIterator[R] =
    results(maxRows).fold(
      count =>
        if (acceptUpdateCount) new CloseableIterator.Single[R](count.asInstanceOf[R])
        else throw new SlickException(s"Statement did not return a ResultSet (update count $count); " +
          "use asUpdate or sqlu for non-query statements (on SQL Server, SET NOCOUNT ON before a query): " +
          getStatement),
      identity
    )

  /** Invoke the statement and return the raw results. */
  def results(maxRows: Int,
              defaultType: ResultSetType = ResultSetType.ForwardOnly,
              defaultConcurrency: ResultSetConcurrency = ResultSetConcurrency.ReadOnly,
              defaultHoldability: ResultSetHoldability = ResultSetHoldability.Default,
              autoClose: Boolean = true)
             (implicit session: JdbcBackend#JdbcSessionDef): Either[Int, PositionedResultIterator[R]] = {
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
          private[this] var exhausted = false
          override def nextRow = {
            val ret = super.nextRow
            if (!ret) exhausted = true
            ret
          }
          def close() = {
            try {
              this.rs.close()
              // Only when the result set was read to the end, so that closing it early (or after an
              // error) neither reads further results nor replaces the original error
              if (exhausted) StatementInvoker.skipMoreResults(st)
            } finally st.close()
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
                logBuffer += 1.to(logHeader(0).length).map(idx => this.pr.rs.getObject(idx) : Any).to(ArrayBuffer)
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
        StatementInvoker.skipMoreResults(st)
        Left(count)
      }
    } finally if(doClose) st.close()
  }

  protected def extractValue(pr: PositionedResult): R
}
