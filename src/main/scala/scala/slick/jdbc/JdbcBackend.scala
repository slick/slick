package scala.slick.jdbc

import scala.language.reflectiveCalls
import scala.slick.backend.DatabaseComponent
import scala.slick.SlickException
import slick.util.SlickLogger
import java.util.Properties
import java.sql.{Array => _, _}
import javax.sql.DataSource
import javax.naming.InitialContext
import org.slf4j.LoggerFactory

/**
 * A JDBC-based database back-end.
 */
trait JdbcBackend extends DatabaseComponent {
  protected[this] lazy val statementLogger = new SlickLogger(LoggerFactory.getLogger(classOf[JdbcBackend].getName+".statement"))

  type Database = DatabaseDef
  type Session = SessionDef
  type DatabaseFactory = DatabaseFactoryDef

  val Database = new DatabaseFactoryDef {}
  val backend: JdbcBackend = this

  trait DatabaseDef extends super.DatabaseDef {
    /**
     * The DatabaseCapabilities, accessed through a Session and created by the
     * first Session that needs them. Access does not need to be synchronized
     * because, in the worst case, capabilities will be determined multiple
     * times by different concurrent sessions but the result should always be
     * the same.
     */
    @volatile
    protected[JdbcBackend] var capabilities: DatabaseCapabilities = null

    def createSession(): Session = new BaseSession(this)

    def createConnection(): Connection
  }

  trait DatabaseFactoryDef extends super.DatabaseFactoryDef {
    /**
     * Create a Database based on a DataSource.
     */
    def forDataSource(ds: DataSource): DatabaseDef = new DatabaseDef {
      def createConnection(): Connection = ds.getConnection
    }

    /**
     * Create a Database based on the JNDI name of a DataSource.
     */
    def forName(name: String) = new InitialContext().lookup(name) match {
      case ds: DataSource => forDataSource(ds)
      case x => throw new SlickException("Expected a DataSource for JNDI name "+name+", but got "+x)
    }

    /**
     * Create a Database that uses the DriverManager to open new connections.
     */
    def forURL(url:String, user:String = null, password:String = null, prop: Properties = null, driver:String = null): DatabaseDef = new DatabaseDef {
      if(driver ne null) Class.forName(driver)
      val cprop = if(prop.ne(null) && user.eq(null) && password.eq(null)) prop else {
        val p = new Properties(prop)
        if(user ne null) p.setProperty("user", user)
        if(password ne null) p.setProperty("password", password)
        p
      }

      def createConnection(): Connection = DriverManager.getConnection(url, cprop)
    }

    /**
     * Create a Database that directly uses a Driver to open new connections.
     * This is needed to open a JDBC URL with a driver that was not loaded by
     * the system ClassLoader.
     */
    def forDriver(driver:Driver, url:String, user:String = null, password:String = null, prop: Properties = null): DatabaseDef = new DatabaseDef {
      val cprop = if(prop.ne(null) && user.eq(null) && password.eq(null)) prop else {
        val p = new Properties(prop)
        if(user ne null) p.setProperty("user", user)
        if(password ne null) p.setProperty("password", password)
        p
      }

      def createConnection(): Connection = {
        val conn = driver.connect(url, cprop)
        if(conn eq null)
          throw new SQLException("Driver "+driver+" does not know how to handle URL "+url, "08001")
        conn
      }
    }

    /**
     * Create a Database that uses the DriverManager to open new connections.
     */
    def forURL(url:String, prop: Map[String, String]): Database = {
      val p = new Properties
      if(prop ne null)
        for((k,v) <- prop) if(k.ne(null) && v.ne(null)) p.setProperty(k, v)
      forURL(url, prop = p, driver = null)
    }
  }

  trait SessionDef extends super.SessionDef { self =>

    def database: Database
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
      statementLogger.debug("Preparing statement: "+sql)
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

    final def prepareInsertStatement(sql: String, columnNames: Array[String] = new Array[String](0)): PreparedStatement = {
      statementLogger.debug("Preparing insert statement: "+sql+", returning: "+columnNames.mkString(","))
      conn.prepareStatement(sql, columnNames)
    }

    final def prepareInsertStatement(sql: String, columnIndexes: Array[Int]): PreparedStatement = {
      statementLogger.debug("Preparing insert statement: "+sql+", returning indexes: "+columnIndexes.mkString(","))
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

    def force() { conn }

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
      def database = self.database
      def conn = self.conn
      def metaData = self.metaData
      def capabilities = self.capabilities
      def close() = self.close()
      def rollback() = self.rollback()
      def withTransaction[T](f: => T) = self.withTransaction(f)
    }

    protected def loggingStatement(st: Statement): Statement = if(statementLogger.isDebugEnabled) new Statement {
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
        statementLogger.debug("Adding to batch: "+sql)
        st.addBatch(sql)
      }
      def getMaxRows: Int = st.getMaxRows
      def execute(sql: String, columnNames: Array[String]): Boolean = {
        statementLogger.debug("Executing statement: "+sql)
        st.execute(sql, columnNames)
      }
      def execute(sql: String, columnIndexes: Array[Int]): Boolean = {
        statementLogger.debug("Executing statement: "+sql)
        st.execute(sql, columnIndexes)
      }
      def execute(sql: String, autoGeneratedKeys: Int): Boolean = {
        statementLogger.debug("Executing statement: "+sql)
        st.execute(sql, autoGeneratedKeys)
      }
      def execute(sql: String): Boolean = {
        statementLogger.debug("Executing statement: "+sql)
        st.execute(sql)
      }
      def executeQuery(sql: String): ResultSet = {
        statementLogger.debug("Executing query: "+sql)
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
        statementLogger.debug("Executing update: "+sql)
        st.executeUpdate(sql, columnNames)
      }
      def executeUpdate(sql: String, columnIndexes: Array[Int]): Int = {
        statementLogger.debug("Executing update: "+sql)
        st.executeUpdate(sql, columnIndexes)
      }
      def executeUpdate(sql: String, autoGeneratedKeys: Int): Int = {
        statementLogger.debug("Executing update: "+sql)
        st.executeUpdate(sql, autoGeneratedKeys)
      }
      def executeUpdate(sql: String): Int = {
        statementLogger.debug("Executing update: "+sql)
        st.executeUpdate(sql)
      }
      def getWarnings: SQLWarning = st.getWarnings
      def getQueryTimeout: Int = st.getQueryTimeout
      def setQueryTimeout(seconds: Int) = st.setQueryTimeout(seconds)
      def setFetchSize(rows: Int) = st.setFetchSize(rows)
      def setEscapeProcessing(enable: Boolean) = st.setEscapeProcessing(enable)
      def executeBatch(): Array[Int] = {
        statementLogger.debug("Executing batch")
        st.executeBatch()
      }
      def getConnection: Connection = st.getConnection
      def getMaxFieldSize: Int = st.getMaxFieldSize
      def closeOnCompletion(): Unit =
        st.asInstanceOf[{ def closeOnCompletion(): Unit }].closeOnCompletion()
      def isCloseOnCompletion(): Boolean =
        st.asInstanceOf[{ def isCloseOnCompletion(): Boolean }].isCloseOnCompletion()
    } else st
  }

  class BaseSession(val database: Database) extends SessionDef {
    protected var open = false
    protected var doRollback = false
    protected var inTransaction = false

    lazy val conn = { open = true; database.createConnection() }
    lazy val metaData = conn.getMetaData()

    def capabilities = {
      val dc = database.capabilities
      if(dc ne null) dc
      else {
        val newDC = new DatabaseCapabilities(this)
        database.capabilities = newDC
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

  /**
   * Describes capabilities of the database which can be determined from a
   * DatabaseMetaData object and then cached and reused for all sessions.
   */
  class DatabaseCapabilities(session: Session) {
    val supportsBatchUpdates = session.metaData.supportsBatchUpdates
  }
}

object JdbcBackend extends JdbcBackend {}
