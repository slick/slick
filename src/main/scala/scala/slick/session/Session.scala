package scala.slick.session

import java.sql._
import scala.slick.SlickException
import scala.slick.util.Logging
import scala.Array

/**
 * A database session which opens a connection and transaction on demand.
 */
trait Session extends java.io.Closeable with Logging { self =>

  def conn: Connection
  def metaData: DatabaseMetaData
  def capabilities: DatabaseCapabilities

  def resultSetType: ResultSetType = ResultSetType.Auto
  def resultSetConcurrency: ResultSetConcurrency = ResultSetConcurrency.Auto
  def resultSetHoldability: ResultSetHoldability = ResultSetHoldability.Auto

  final def prepareStatement(sql: String,
             defaultType: ResultSetType = ResultSetType.ForwardOnly,
             defaultConcurrency: ResultSetConcurrency = ResultSetConcurrency.ReadOnly,
             defaultHoldability: ResultSetHoldability = ResultSetHoldability.Default): PreparedStatement = {
    logger.debug("Preparing statement: "+sql)
    resultSetHoldability.withDefault(defaultHoldability) match {
      case ResultSetHoldability.Default =>
        conn.prepareStatement(sql, resultSetType.withDefault(defaultType).intValue,
          resultSetConcurrency.withDefault(defaultConcurrency).intValue)
      case h =>
        conn.prepareStatement(sql, resultSetType.withDefault(defaultType).intValue,
          resultSetConcurrency.withDefault(defaultConcurrency).intValue,
          h.intValue)
    }
  }

  final def prepareInsertStatement(sql: String,
              columnNames: Array[String] = new Array[String](0)): PreparedStatement = {
    logger.debug("Preparing insert statement: "+sql+", returning: "+columnNames.mkString(","))
    conn.prepareStatement(sql, columnNames)
  }

  final def prepareInsertStatement(sql: String, columnIndexes: Array[Int]): PreparedStatement = {
    logger.debug("Preparing insert statement: "+sql+", returning indexes: "+columnIndexes.mkString(","))
    conn.prepareStatement(sql, columnIndexes)
  }

  final def createStatement(defaultType: ResultSetType = ResultSetType.ForwardOnly,
             defaultConcurrency: ResultSetConcurrency = ResultSetConcurrency.ReadOnly,
             defaultHoldability: ResultSetHoldability = ResultSetHoldability.Default): Statement = {
    loggingStatement(resultSetHoldability.withDefault(defaultHoldability) match {
      case ResultSetHoldability.Default =>
        conn.createStatement(resultSetType.withDefault(defaultType).intValue,
          resultSetConcurrency.withDefault(defaultConcurrency).intValue)
      case h =>
        conn.createStatement(resultSetType.withDefault(defaultType).intValue,
          resultSetConcurrency.withDefault(defaultConcurrency).intValue,
          h.intValue)
    })
  }

  final def withPreparedStatement[T](sql: String,
              defaultType: ResultSetType = ResultSetType.ForwardOnly,
              defaultConcurrency: ResultSetConcurrency = ResultSetConcurrency.ReadOnly,
              defaultHoldability: ResultSetHoldability = ResultSetHoldability.Default)(f: (PreparedStatement => T)): T = {
    val st = prepareStatement(sql, defaultType, defaultConcurrency, defaultHoldability)
    try f(st) finally st.close()
  }

  final def withPreparedInsertStatement[T](sql: String,
              columnNames: Array[String] = new Array[String](0))(f: (PreparedStatement => T)): T = {
    val st = prepareInsertStatement(sql, columnNames)
    try f(st) finally st.close()
  }

  final def withPreparedInsertStatement[T](sql: String,
              columnIndexes: Array[Int])(f: (PreparedStatement => T)): T = {
    val st = prepareInsertStatement(sql, columnIndexes)
    try f(st) finally st.close()
  }

  final def withStatement[T](defaultType: ResultSetType = ResultSetType.ForwardOnly,
              defaultConcurrency: ResultSetConcurrency = ResultSetConcurrency.ReadOnly,
              defaultHoldability: ResultSetHoldability = ResultSetHoldability.Default)(f: (Statement => T)): T = {
    val st = createStatement(defaultType, defaultConcurrency, defaultHoldability)
    try f(st) finally st.close()
  }

  def close(): Unit

  /**
   * Call this method within a <em>withTransaction</em> call to roll back the current
   * transaction after <em>withTransaction</em> returns.
   */
  def rollback(): Unit

  /**
   * Run the supplied function within a transaction. If the function throws an Exception
   * or the session's rollback() method is called, the transaction is rolled back,
   * otherwise it is commited when the function returns.
   */
  def withTransaction[T](f: => T): T

  def forParameters(rsType: ResultSetType = resultSetType, rsConcurrency: ResultSetConcurrency = resultSetConcurrency,
                    rsHoldability: ResultSetHoldability = resultSetHoldability): Session = new Session {
    override def resultSetType = rsType
    override def resultSetConcurrency = rsConcurrency
    override def resultSetHoldability = rsHoldability
    def conn = self.conn
    def metaData = self.metaData
    def capabilities = self.capabilities
    def close() = self.close()
    def rollback() = self.rollback()
    def withTransaction[T](f: => T) = self.withTransaction(f)
  }

  protected def loggingStatement(st: Statement): Statement = if(logger.isDebugEnabled) new Statement {
    def setMaxFieldSize(max: Int) = st.setMaxFieldSize(max)
    def clearWarnings() = st.clearWarnings()
    def getMoreResults(current: Int) = st.getMoreResults(current)
    def getMoreResults: Boolean = st.getMoreResults
    def getGeneratedKeys: ResultSet = st.getGeneratedKeys
    def cancel() = st.cancel()
    def getResultSet: ResultSet = st.getResultSet
    def setPoolable(poolable: Boolean) = st.setPoolable(poolable)
    def isPoolable: Boolean = st.isPoolable
    def setCursorName(name: String) = st.setCursorName(name)
    def getUpdateCount: Int = st.getUpdateCount
    def addBatch(sql: String) = {
      logger.debug("Adding to batch: "+sql)
      st.addBatch(sql)
    }
    def getMaxRows: Int = st.getMaxRows
    def execute(sql: String, columnNames: Array[String]): Boolean = {
      logger.debug("Executing statement: "+sql)
      st.execute(sql, columnNames)
    }
    def execute(sql: String, columnIndexes: Array[Int]): Boolean = {
      logger.debug("Executing statement: "+sql)
      st.execute(sql, columnIndexes)
    }
    def execute(sql: String, autoGeneratedKeys: Int): Boolean = {
      logger.debug("Executing statement: "+sql)
      st.execute(sql, autoGeneratedKeys)
    }
    def execute(sql: String): Boolean = {
      logger.debug("Executing statement: "+sql)
      st.execute(sql)
    }
    def executeQuery(sql: String): ResultSet = {
      logger.debug("Executing query: "+sql)
      st.executeQuery(sql)
    }
    def getResultSetType: Int = st.getResultSetType
    def unwrap[T](iface: Class[T]): T = st.unwrap(iface)
    def setMaxRows(max: Int) = st.setMaxRows(max)
    def getFetchSize: Int = st.getFetchSize
    def getResultSetHoldability: Int = st.getResultSetHoldability
    def setFetchDirection(direction: Int) = st.setFetchDirection(direction)
    def getFetchDirection: Int = st.getFetchDirection
    def getResultSetConcurrency: Int = st.getResultSetConcurrency
    def isWrapperFor(iface: Class[_]): Boolean = st.isWrapperFor(iface)
    def clearBatch() = st.clearBatch()
    def close() = st.close()
    def isClosed: Boolean = st.isClosed
    def executeUpdate(sql: String, columnNames: Array[String]): Int = {
      logger.debug("Executing update: "+sql)
      st.executeUpdate(sql, columnNames)
    }
    def executeUpdate(sql: String, columnIndexes: Array[Int]): Int = {
      logger.debug("Executing update: "+sql)
      st.executeUpdate(sql, columnIndexes)
    }
    def executeUpdate(sql: String, autoGeneratedKeys: Int): Int = {
      logger.debug("Executing update: "+sql)
      st.executeUpdate(sql, autoGeneratedKeys)
    }
    def executeUpdate(sql: String): Int = {
      logger.debug("Executing update: "+sql)
      st.executeUpdate(sql)
    }
    def getWarnings: SQLWarning = st.getWarnings
    def getQueryTimeout: Int = st.getQueryTimeout
    def setQueryTimeout(seconds: Int) = st.setQueryTimeout(seconds)
    def setFetchSize(rows: Int) = st.setFetchSize(rows)
    def setEscapeProcessing(enable: Boolean) = st.setEscapeProcessing(enable)
    def executeBatch(): Array[Int] = {
      logger.debug("Executing batch")
      st.executeBatch()
    }
    def getConnection: Connection = st.getConnection
    def getMaxFieldSize: Int = st.getMaxFieldSize
  } else st
}

class BaseSession private[session] (db: Database) extends Session {

  var open = false
  var doRollback = false
  var inTransaction = false

  lazy val conn = { open = true; db.createConnection() }
  lazy val metaData = conn.getMetaData()

  def capabilities = {
    val dc = db.capabilities
    if(dc ne null) dc
    else {
      val newDC = new DatabaseCapabilities(this)
      db.capabilities = newDC
      newDC
    }
  }

  def close() {
    if(open) conn.close()
  }

  def rollback() {
    if(conn.getAutoCommit) throw new SlickException("Cannot roll back session in auto-commit mode")
    doRollback = true
  }

  def withTransaction[T](f: => T): T = if(inTransaction) f else {
    conn.setAutoCommit(false)
    inTransaction = true
    try {
      var done = false
      try {
        doRollback = false
        val res = f
        if(doRollback) conn.rollback()
        else conn.commit()
        done = true
        res
      } finally if(!done) conn.rollback()
    } finally {
      conn.setAutoCommit(true)
      inTransaction = false
    }
  }
}
