package scala.slick.jdbc

import scala.concurrent.Future
import scala.language.reflectiveCalls

import java.util.Properties
import java.sql.{Array => _, _}
import javax.sql.DataSource
import javax.naming.InitialContext

import scala.slick.backend.DatabaseComponent
import scala.slick.SlickException
import scala.slick.util.{SlickLogger, AsyncExecutor}
import scala.slick.util.ConfigExtensionMethods._

import org.slf4j.LoggerFactory
import com.typesafe.config.{ConfigFactory, Config}

/** A JDBC-based database back-end which can be used for <em>Plain SQL</em> queries
  * and with all [[scala.slick.driver.JdbcProfile]]-based drivers. */
trait JdbcBackend extends DatabaseComponent {
  protected[this] lazy val statementLogger = new SlickLogger(LoggerFactory.getLogger(classOf[JdbcBackend].getName+".statement"))
  protected[this] lazy val benchmarkLogger = new SlickLogger(LoggerFactory.getLogger(classOf[JdbcBackend].getName+".benchmark"))

  type Database = DatabaseDef
  type Session = SessionDef
  type DatabaseFactory = DatabaseFactoryDef

  val Database = new DatabaseFactoryDef {}
  val backend: JdbcBackend = this

  class DatabaseDef(val source: JdbcDataSource, val executor: AsyncExecutor) extends super.DatabaseDef {
    /** The DatabaseCapabilities, accessed through a Session and created by the
      * first Session that needs them. Access does not need to be synchronized
      * because, in the worst case, capabilities will be determined multiple
      * times by different concurrent sessions but the result should always be
      * the same. */
    @volatile
    protected[JdbcBackend] var capabilities: DatabaseCapabilities = null

    def createSession(): Session = new BaseSession(this)

    protected[this] def asyncExecutionContext = executor.executionContext

    /** Free all resources allocated by Slick for this Database object. In particular, the
      * [[scala.slick.util.AsyncExecutor]] with the thread pool for asynchronous execution is shut
      * down. If this object represents a connection pool managed directly by Slick, it is also
      * closed. */
    def close(): Unit = try executor.close() finally source.close()
   }

  trait DatabaseFactoryDef extends super.DatabaseFactoryDef {
    /** Create a Database based on a [[JdbcDataSource]]. */
    def forSource(source: JdbcDataSource, executor: AsyncExecutor = AsyncExecutor.default()) =
      new DatabaseDef(source, executor)

    /** Create a Database based on a DataSource. */
    def forDataSource(ds: DataSource, executor: AsyncExecutor = AsyncExecutor.default()): DatabaseDef =
      forSource(new DataSourceJdbcDataSource(ds))

    /** Create a Database based on the JNDI name of a DataSource. */
    def forName(name: String, executor: AsyncExecutor = null) = new InitialContext().lookup(name) match {
      case ds: DataSource => forDataSource(ds, executor match {
        case null => AsyncExecutor.default(name)
        case e => e
      })
      case x => throw new SlickException("Expected a DataSource for JNDI name "+name+", but got "+x)
    }

    /** Create a Database that uses the DriverManager to open new connections. */
    def forURL(url:String, user:String = null, password:String = null, prop: Properties = null, driver:String = null, executor: AsyncExecutor = AsyncExecutor.default()): DatabaseDef =
      forSource(new DriverJdbcDataSource(url, user, password, prop, driverName = driver), executor)

    /** Create a Database that uses the DriverManager to open new connections. */
    def forURL(url:String, prop: Map[String, String]): Database = {
      val p = new Properties
      if(prop ne null)
        for((k,v) <- prop) if(k.ne(null) && v.ne(null)) p.setProperty(k, v)
      forURL(url, prop = p, driver = null)
    }

    /** Create a Database that directly uses a Driver to open new connections.
      * This is needed to open a JDBC URL with a driver that was not loaded by the system ClassLoader. */
    def forDriver(driver:Driver, url:String, user:String = null, password:String = null, prop: Properties = null, executor: AsyncExecutor = AsyncExecutor.default()): DatabaseDef =
      forSource(new DriverJdbcDataSource(url, user, password, prop, driver = driver), executor)

    /** Load a database configuration through [[https://github.com/typesafehub/config Typesafe Config]].
      *
      * The main config key to set is `pool`. It determines the connection pool implementation to
      * use (if any). The default is undefined/null (no pool, use the DriverManager directly).
      * Slick comes with support for [[https://github.com/brettwooldridge/HikariCP HikariCP]] which
      * can be selected by setting `pool=HikariCP` (or the full object name, e.g.
      * `scala.slick.jdbc.HikariCPJdbcDataSource`).
      * 3rd-party connection pool implementations have to be specified with the fully qualified
      * name of an object implementing [[JdbcDataSourceFactory]].
      *
      * The following config keys are supported for pool settings `null` and `HikariCP`:
      * <ul>
      *   <li>`url` (String, required): JDBC URL</li>
      *   <li>`driver` or `driverClassName` (String, optional): JDBC driver class to load</li>
      *   <li>`user` (String, optional): User name</li>
      *   <li>`password` (String, optional): Password</li>
      *   <li>`isolation` (String, optional): Transaction isolation level for new connections.
      *     Allowed values are: `NONE`, `READ_COMMITTED`, `READ_UNCOMMITTED`, `REPEATABLE_READ`,
      *     `SERIALIZABLE`.</li>
      *   <li>`catalog` (String, optional): Default catalog for new connections.</li>
      *   <li>`readOnly` (Boolean, optional): Read Only flag for new connections.</li>
      *   <li>`properties` (Map, optional): Properties to pass to the driver (or
      *     to the [[javax.sql.DataSource]] when using HikariCP with a `dataSourceClass`
      *     instead of a driver).</li>
      *   <li>`numThreads` (Int, optional, default: 20): The number of concurrent threads in the
      *     thread pool for asynchronous execution of database actions.</li>
      *   <li>`queueSize` (Int, optional, default: 1000): The size of the queue for database
      *     actions which cannot be executed immediately when all threads are busy. Beyond this
      *     limit new actions fail immediately. Set to 0 for no queue (direct hand-off) or to -1
      *     for an unlimited queue size (not recommended).</li>
      * </ul>
      *
      * The following additional keys are supported for pool setting `HikariCP`:
      * <ul>
      *   <li>`dataSourceClass` (String, optional): The name of the DataSource class provided by
      *     the JDBC driver. This is preferred over using `driver`. Note that `url` is ignored when
      *     this key is set (You have to use `properties` to configure the database
      *     connection instead).</li>
      *   <li>`maxConnections` (Int, optional, default: `numThreads` * 5): The maximum number of
      *     connections in the pool.</li>
      *   <li>`minConnections` (Int, optional, default: same as `numThreads`): The minimum number
      *     of connections to keep in the pool.</li>
      *   <li>`connectionTimeout` (Duration, optional, default: 1s): The maximum time to wait
      *     before a call to getConnection is timed out. If this time is exceeded without a
      *     connection becoming available, a SQLException will be thrown. 100ms is the minimum
      *     value.</li>
      *   <li>`idleTimeout` (Duration, optional, default: 10min): The maximum amount
      *     of time that a connection is allowed to sit idle in the pool. A value of 0 means that
      *     idle connections are never removed from the pool.</li>
      *   <li>`maxLifetime` (Duration, optional, default: 30min): The maximum lifetime of a
      *     connection in the pool. When an idle connection reaches this timeout, even if recently
      *     used, it will be retired from the pool. A value of 0 indicates no maximum
      *     lifetime.</li>
      *   <li>`connectionInitSql` (String, optional): A SQL statement that will be
      *     executed after every new connection creation before adding it to the pool. If this SQL
      *     is not valid or throws an exception, it will be treated as a connection failure and the
      *     standard retry logic will be followed.</li>
      *   <li>`initializationFailFast` (Boolean, optional, default: false): Controls whether the
      *     pool will "fail fast" if the pool cannot be seeded with initial connections
      *     successfully. If connections cannot be created at pool startup time, a RuntimeException
      *     will be thrown. This property has no effect if `minConnections` is 0.</li>
      *   <li>`leakDetectionThreshold` (Duration, optional, default: 0): The amount of time that a
      *     connection can be out of the pool before a message is logged indicating a possible
      *     connection leak. A value of 0 means leak detection is disabled. Lowest acceptable value
      *     for enabling leak detection is 10s.</li>
      *   <li>`connectionTestQuery` (String, optional): A statement that will be executed just
      *     before a connection is obtained from the pool to validate that the connection to the
      *     database is still alive. It is database dependent and should be a query that takes very
      *     little processing by the database (e.g. "VALUES 1"). When not set, the JDBC4
      *     `Connection.isValid()` method is used instead (which is usually preferable).</li>
      *   <li>`registerMbeans` (Boolean, optional, default: false): Whether or not JMX Management
      *     Beans ("MBeans") are registered.</li>
      * </ul>
      *
      * By default the pool is tuned for asynchronous execution. Apart from the connection
      * parameters you should only have to set `numThreads` and `queueSize` in most cases. In this
      * scenario there is contention over the thread pool (via its queue), not over the
      * connections, so you can have a rather large limit on the maximum number of connections
      * (what the database server can still handle, not what is most efficient). Slick will use
      * more connections than there are threads in the pool when sequencing non-database actions
      * inside a transaction.
      *
      * Unknown keys are ignored. Invalid values or missing mandatory keys will trigger a
      * [[SlickException]].
      *
      * @param path The path in the configuration file for the database configuration (e.g. `foo.bar`
      *             would find a database URL at config key `foo.bar.url`)
      * @param config The `Config` object to read from. This defaults to the global app config
      *               (e.g. in `application.conf` at the root of the class path) if not specified.
      * @param driver An optional JDBC driver to call directly. If this is set to a non-null value,
      *               the `driver` key from the configuration is ignored. The default is to use the
      *               standard lookup mechanism. The explicit driver may not be supported by all
      *               connection pools (in particular, the default [[HikariJdbcDataSource]]).
      */
    def forConfig(path: String, config: Config = ConfigFactory.load(), driver: Driver = null): Database = {
      val source = JdbcDataSource.forConfig(if(path.isEmpty) config else config.getConfig(path), driver, path)
      val executor = AsyncExecutor(path, config.getIntOr("numThreads", 20), config.getIntOr("queueSize", 1000))
      forSource(source, executor)
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
      loggingPreparedStatement(resultSetHoldability.withDefault(defaultHoldability) match {
        case ResultSetHoldability.Default =>
          conn.prepareStatement(sql, resultSetType.withDefault(defaultType).intValue,
            resultSetConcurrency.withDefault(defaultConcurrency).intValue)
        case h =>
          conn.prepareStatement(sql, resultSetType.withDefault(defaultType).intValue,
            resultSetConcurrency.withDefault(defaultConcurrency).intValue,
            h.intValue)
      })
    }

    final def prepareInsertStatement(sql: String, columnNames: Array[String] = new Array[String](0)): PreparedStatement = {
      statementLogger.debug("Preparing insert statement: "+sql+", returning: "+columnNames.mkString(","))
      loggingPreparedStatement(conn.prepareStatement(sql, columnNames))
    }

    final def prepareInsertStatement(sql: String, columnIndexes: Array[Int]): PreparedStatement = {
      statementLogger.debug("Preparing insert statement: "+sql+", returning indexes: "+columnIndexes.mkString(","))
      loggingPreparedStatement(conn.prepareStatement(sql, columnIndexes))
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

    /** A wrapper around the JDBC Connection's prepareStatement method, that automatically closes the statement. */
    final def withPreparedStatement[T](sql: String,
                                       defaultType: ResultSetType = ResultSetType.ForwardOnly,
                                       defaultConcurrency: ResultSetConcurrency = ResultSetConcurrency.ReadOnly,
                                       defaultHoldability: ResultSetHoldability = ResultSetHoldability.Default)(f: (PreparedStatement => T)): T = {
      val st = prepareStatement(sql, defaultType, defaultConcurrency, defaultHoldability)
      try f(st) finally st.close()
    }

    /** A wrapper around the JDBC Connection's prepareInsertStatement method, that automatically closes the statement. */
    final def withPreparedInsertStatement[T](sql: String,
                                             columnNames: Array[String] = new Array[String](0))(f: (PreparedStatement => T)): T = {
      val st = prepareInsertStatement(sql, columnNames)
      try f(st) finally st.close()
    }

    /** A wrapper around the JDBC Connection's prepareInsertStatement method, that automatically closes the statement. */
    final def withPreparedInsertStatement[T](sql: String,
                                             columnIndexes: Array[Int])(f: (PreparedStatement => T)): T = {
      val st = prepareInsertStatement(sql, columnIndexes)
      try f(st) finally st.close()
    }

    /** A wrapper around the JDBC Connection's createStatement method, that automatically closes the statement. */
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

    /**
     * Create a new Slick Session wrapping the same JDBC connection, but using the given values as defaults for
     * resultSetType, resultSetConcurrency and resultSetHoldability.
     */
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

    protected def loggingStatement(st: Statement): Statement =
      if(statementLogger.isDebugEnabled || benchmarkLogger.isDebugEnabled) new LoggingStatement(st) else st

    protected def loggingPreparedStatement(st: PreparedStatement): PreparedStatement =
      if(statementLogger.isDebugEnabled || benchmarkLogger.isDebugEnabled) new LoggingPreparedStatement(st) else st

    class LoggingStatement(st: Statement) extends Statement {
      private[this] val doStatement = statementLogger.isDebugEnabled
      private[this] val doBenchmark = benchmarkLogger.isDebugEnabled

      @inline protected[this] def logged[T](sql: String, what: String = "statement")(f: =>T) = {
        if(doStatement && (sql ne null)) statementLogger.debug("Executing "+what+": "+sql)
        val t0 = if(doBenchmark) System.nanoTime() else 0L
        val res = f
        if(doBenchmark) benchmarkLogger.debug("Execution of "+what+" took "+formatNS(System.nanoTime()-t0))
        res
      }

      private[this] def formatNS(ns: Long): String = {
        if(ns < 1000L) ns + "ns"
        else if(ns < 1000000L) (ns / 1000L) + "µs"
        else if(ns < 1000000000L) (ns / 1000000L) + "ms"
        else (ns / 1000000000L) + "s"
      }

      def addBatch(sql: String) = {
        if(doStatement) statementLogger.debug("Adding to batch: "+sql)
        st.addBatch(sql)
      }
      def execute(sql: String, columnNames: Array[String]): Boolean = logged(sql) { st.execute(sql, columnNames) }
      def execute(sql: String, columnIndexes: Array[Int]): Boolean = logged(sql) { st.execute(sql, columnIndexes) }
      def execute(sql: String, autoGeneratedKeys: Int): Boolean = logged(sql) { st.execute(sql, autoGeneratedKeys) }
      def execute(sql: String): Boolean = logged(sql) { st.execute(sql) }
      def executeQuery(sql: String): ResultSet = logged(sql, "query") { st.executeQuery(sql) }
      def executeUpdate(sql: String, columnNames: Array[String]): Int = logged(sql, "update") { st.executeUpdate(sql, columnNames) }
      def executeUpdate(sql: String, columnIndexes: Array[Int]): Int = logged(sql, "update") { st.executeUpdate(sql, columnIndexes) }
      def executeUpdate(sql: String, autoGeneratedKeys: Int): Int = logged(sql, "update") { st.executeUpdate(sql, autoGeneratedKeys) }
      def executeUpdate(sql: String): Int = logged(sql, "update") { st.executeUpdate(sql) }
      def executeBatch(): Array[Int] = logged(null, "batch") { st.executeBatch() }

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
      def getMaxRows: Int = st.getMaxRows
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
      def getWarnings: SQLWarning = st.getWarnings
      def getQueryTimeout: Int = st.getQueryTimeout
      def setQueryTimeout(seconds: Int) = st.setQueryTimeout(seconds)
      def setFetchSize(rows: Int) = st.setFetchSize(rows)
      def setEscapeProcessing(enable: Boolean) = st.setEscapeProcessing(enable)
      def getConnection: Connection = st.getConnection
      def getMaxFieldSize: Int = st.getMaxFieldSize
      def closeOnCompletion(): Unit =
        st.asInstanceOf[{ def closeOnCompletion(): Unit }].closeOnCompletion()
      def isCloseOnCompletion(): Boolean =
        st.asInstanceOf[{ def isCloseOnCompletion(): Boolean }].isCloseOnCompletion()
    }

    class LoggingPreparedStatement(st: PreparedStatement) extends LoggingStatement(st) with PreparedStatement {
      def execute(): Boolean = logged(null, "prepared statement") { st.execute() }
      def executeQuery(): java.sql.ResultSet = logged(null, "prepared query") { st.executeQuery() }
      def executeUpdate(): Int = logged(null, "prepared update") { st.executeUpdate() }

      def addBatch(): Unit = st.addBatch()
      def clearParameters(): Unit = st.clearParameters()
      def getMetaData(): java.sql.ResultSetMetaData = st.getMetaData
      def getParameterMetaData(): java.sql.ParameterMetaData = st.getParameterMetaData()
      def setArray(idx: Int, value: java.sql.Array): Unit = st.setArray(idx, value)
      def setAsciiStream(idx: Int, value: java.io.InputStream): Unit = st.setAsciiStream(idx, value)
      def setAsciiStream(idx: Int, value: java.io.InputStream, len: Long): Unit = st.setAsciiStream(idx, value, len)
      def setAsciiStream(idx: Int, value: java.io.InputStream, len: Int): Unit = st.setAsciiStream(idx, value, len)
      def setBigDecimal(idx: Int, value: java.math.BigDecimal): Unit = st.setBigDecimal(idx, value)
      def setBinaryStream(idx: Int, value: java.io.InputStream): Unit = st.setBinaryStream(idx, value)
      def setBinaryStream(idx: Int, value: java.io.InputStream, len: Long): Unit = st.setBinaryStream(idx, value, len)
      def setBinaryStream(idx: Int, value: java.io.InputStream, len: Int): Unit = st.setBinaryStream(idx, value, len)
      def setBlob(idx: Int, value: java.io.InputStream): Unit = st.setBlob(idx, value)
      def setBlob(idx: Int, value: java.io.InputStream, len: Long): Unit = st.setBlob(idx, value, len)
      def setBlob(idx: Int, value: java.sql.Blob): Unit = st.setBlob(idx, value)
      def setBoolean(idx: Int, value: Boolean): Unit = st.setBoolean(idx, value)
      def setByte(idx: Int, value: Byte): Unit = st.setByte(idx, value)
      def setBytes(idx: Int, value: Array[Byte]): Unit = st.setBytes(idx, value)
      def setCharacterStream(idx: Int, value: java.io.Reader): Unit = st.setCharacterStream(idx, value)
      def setCharacterStream(idx: Int, value: java.io.Reader, len: Long): Unit = st.setCharacterStream(idx, value, len)
      def setCharacterStream(idx: Int, value: java.io.Reader, len: Int): Unit = st.setCharacterStream(idx, value, len)
      def setClob(idx: Int, value: java.io.Reader): Unit = st.setClob(idx, value)
      def setClob(idx: Int, value: java.io.Reader, len: Long): Unit = st.setClob(idx, value, len)
      def setClob(idx: Int, value: java.sql.Clob): Unit = st.setClob(idx, value)
      def setDate(idx: Int, value: java.sql.Date, cal: java.util.Calendar): Unit = st.setDate(idx, value, cal)
      def setDate(idx: Int, value: java.sql.Date): Unit = st.setDate(idx, value)
      def setDouble(idx: Int, value: Double): Unit = st.setDouble(idx, value)
      def setFloat(idx: Int, value: Float): Unit = st.setFloat(idx, value)
      def setInt(idx: Int, value: Int): Unit = st.setInt(idx, value)
      def setLong(idx: Int, value: Long): Unit = st.setLong(idx, value)
      def setNCharacterStream(idx: Int, value: java.io.Reader): Unit = st.setNCharacterStream(idx, value)
      def setNCharacterStream(idx: Int, value: java.io.Reader, len: Long): Unit = st.setNCharacterStream(idx, value, len)
      def setNClob(idx: Int, value: java.io.Reader): Unit = st.setNClob(idx, value)
      def setNClob(idx: Int, value: java.io.Reader, len: Long): Unit = st.setNClob(idx, value, len)
      def setNClob(idx: Int, value: java.sql.NClob): Unit = st.setNClob(idx, value)
      def setNString(idx: Int, value: String): Unit = st.setNString(idx, value)
      def setNull(idx: Int, tpe: Int, tpeString: String): Unit = st.setNull(idx, tpe, tpeString)
      def setNull(idx: Int, tpe: Int): Unit = st.setNull(idx, tpe)
      def setObject(idx: Int, value: Any, tpe: Int, scale: Int): Unit = st.setObject(idx, value, tpe, scale)
      def setObject(idx: Int, value: Any): Unit = st.setObject(idx, value)
      def setObject(idx: Int, value: Any, tpe: Int): Unit = st.setObject(idx, value, tpe)
      def setRef(idx: Int, value: java.sql.Ref): Unit = st.setRef(idx, value)
      def setRowId(idx: Int, value: java.sql.RowId): Unit = st.setRowId(idx, value)
      def setSQLXML(idx: Int, value: java.sql.SQLXML): Unit = st.setSQLXML(idx, value)
      def setShort(idx: Int, value: Short): Unit = st.setShort(idx, value)
      def setString(idx: Int, value: String): Unit = st.setString(idx, value)
      def setTime(idx: Int, value: java.sql.Time, cal: java.util.Calendar): Unit = st.setTime(idx, value, cal)
      def setTime(idx: Int, value: java.sql.Time): Unit = st.setTime(idx, value)
      def setTimestamp(idx: Int, value: java.sql.Timestamp, cal: java.util.Calendar): Unit = st.setTimestamp(idx, value, cal)
      def setTimestamp(idx: Int, value: java.sql.Timestamp): Unit = st.setTimestamp(idx, value)
      def setURL(idx: Int, value: java.net.URL): Unit = st.setURL(idx, value)
      @deprecated("setUnicodeStream is deprecated", "")
      def setUnicodeStream(idx: Int, value: java.io.InputStream, len: Int): Unit = st.setUnicodeStream(idx, value, len)
    }
  }

  class BaseSession(val database: Database) extends SessionDef {
    protected var open = false
    protected var doRollback = false
    protected var inTransaction = false

    def isOpen = open
    def isInTransaction = inTransaction

    lazy val conn = { open = true; database.source.createConnection }
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
