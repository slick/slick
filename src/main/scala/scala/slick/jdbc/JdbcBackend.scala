package scala.slick.jdbc

import org.reactivestreams.Subscriber

import scala.concurrent.{ExecutionContext, Future}

import java.util.Properties
import java.sql.{Array => _, _}
import javax.sql.DataSource
import javax.naming.InitialContext

import scala.slick.action._
import scala.slick.backend.{DatabasePublisher, DatabaseComponent, RelationalBackend}
import scala.slick.SlickException
import scala.slick.util.{SlickLogger, AsyncExecutor}
import scala.slick.util.ConfigExtensionMethods._

import org.slf4j.LoggerFactory
import com.typesafe.config.{ConfigFactory, Config}

/** A JDBC-based database back-end which can be used for <em>Plain SQL</em> queries
  * and with all [[scala.slick.driver.JdbcProfile]]-based drivers. */
trait JdbcBackend extends RelationalBackend {
  protected[jdbc] lazy val statementLogger = new SlickLogger(LoggerFactory.getLogger(classOf[JdbcBackend].getName+".statement"))
  protected[jdbc] lazy val benchmarkLogger = new SlickLogger(LoggerFactory.getLogger(classOf[JdbcBackend].getName+".benchmark"))

  type This = JdbcBackend
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

    /** Like `stream(StreamingAction)` but you can disable pre-buffering of the next row by setting
      * `bufferNext = false`. The ResultSet will not advance to the next row until you
      * `request()` more data. This allows you to process LOBs asynchronously by requesting only
      * one single element at a time after processing the current one, so that the proper
      * sequencing is preserved even though processing may happen on a different thread. */
    final def stream[T](a: StreamingAction[_, T], bufferNext: Boolean): DatabasePublisher[T] =
      createPublisher(a, s => new JdbcStreamingDatabaseActionContext(s, false, DatabaseDef.this, bufferNext))

    override protected[this] def createStreamingDatabaseActionContext[T](s: Subscriber[_ >: T], useSameThread: Boolean): StreamingDatabaseActionContext =
      new JdbcStreamingDatabaseActionContext(s, useSameThread, DatabaseDef.this, true)

    protected[this] def synchronousExecutionContext = executor.executionContext

    /** Run some code on the [[ioExecutionContext]]. */
    final def io[T](thunk: => T): Future[T] = Future(thunk)(ioExecutionContext)

    /** The `ExecutionContext` which is used for performing blocking database I/O, similar to how
      * `run` or `stream` would run it. This can be used for calling back into blocking JDBC APIs
      * (e.g. for materializing a LOB or mutating a result set row) from asynchronous processors of
      * unbuffered streams. */
    final def ioExecutionContext: ExecutionContext = executor.executionContext

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
    def forURL(url:String, user:String = null, password:String = null, prop: Properties = null, driver:String = null, executor: AsyncExecutor = AsyncExecutor.default(), keepAliveConnection: Boolean = false): DatabaseDef =
      forSource(new DriverJdbcDataSource(url, user, password, prop, driverName = driver, keepAliveConnection = keepAliveConnection), executor)

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
      * The main config key to set is `connectionPool`. It determines the connection pool
      * implementation to use. The default is `HikariCP` (for
      * [[https://github.com/brettwooldridge/HikariCP HikariCP]]). Use `disabled` to disable
      * connection pooling (using the DriverManager directly). A third-party connection pool
      * implementation can be selected by specifying the fully qualified name of an object
      * implementing [[JdbcDataSourceFactory]].
      *
      * The pool is tuned for asynchronous execution by default. Apart from the connection
      * parameters you should only have to set `numThreads` and `queueSize` in most cases. In this
      * scenario there is contention over the thread pool (via its queue), not over the
      * connections, so you can have a rather large limit on the maximum number of connections
      * (based on what the database server can still handle, not what is most efficient). Slick
      * will use more connections than there are threads in the pool when sequencing non-database
      * actions inside a transaction.
      *
      * The following config keys are supported for HikariCP and direct connections:
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
      *     to the DataSource when using HikariCP with a `dataSourceClass`
      *     instead of a driver).</li>
      *   <li>`numThreads` (Int, optional, default: 20): The number of concurrent threads in the
      *     thread pool for asynchronous execution of database actions. See the
      *     [[https://github.com/brettwooldridge/HikariCP/wiki/About-Pool-Sizing HikariCP wiki]]
      *     for more imformation about sizing the thread pool correctly. Note that for asynchronous
      *     execution in Slick you should tune the thread pool size (this parameter) accordingly
      *     instead of the maximum connection pool size.</li>
      *   <li>`queueSize` (Int, optional, default: 1000): The size of the queue for database
      *     actions which cannot be executed immediately when all threads are busy. Beyond this
      *     limit new actions fail immediately. Set to 0 for no queue (direct hand-off) or to -1
      *     for an unlimited queue size (not recommended).</li>
      * </ul>
      *
      * The following additional keys are supported for HikariCP only:
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
      * The following additional keys are supported for direct connections only:
      * <ul>
      *   <li>`keepAliveConnection` (Boolean, optional, default: false): If this is set to true,
      *     one extra connection will be opened as soon as the database is accessed for the first
      *     time, and kept open until `close()` is called. This is useful for named in-memory
      *     databases in test environments.</li>
      * </ul>
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
      *               connection pools (in particular, the default [[HikariCPJdbcDataSource]]).
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
      private[slick] def startInTransaction: Unit = self.startInTransaction
      private[slick] def endInTransaction(f: => Unit): Unit = self.endInTransaction(f)
    }

    protected def loggingStatement(st: Statement): Statement =
      if(statementLogger.isDebugEnabled || benchmarkLogger.isDebugEnabled) new LoggingStatement(st) else st

    protected def loggingPreparedStatement(st: PreparedStatement): PreparedStatement =
      if(statementLogger.isDebugEnabled || benchmarkLogger.isDebugEnabled) new LoggingPreparedStatement(st) else st

    /** Start a `transactionally` block */
    private[slick] def startInTransaction: Unit
    /** End a `transactionally` block, running the specified function first if it is the outermost one. */
    private[slick] def endInTransaction(f: => Unit): Unit
  }

  class BaseSession(val database: Database) extends SessionDef {
    protected var open = false
    protected var doRollback = false
    protected var inTransactionally = 0

    def isOpen = open
    def isInTransaction = inTransactionally > 0

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

    def withTransaction[T](f: => T): T = if(isInTransaction) f else {
      startInTransaction
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
      } finally endInTransaction()
    }

    private[slick] def startInTransaction: Unit = {
      if(!isInTransaction) conn.setAutoCommit(false)
      inTransactionally += 1
    }

    private[slick] def endInTransaction(f: => Unit): Unit = {
      inTransactionally -= 1
      if(!isInTransaction) try f finally conn.setAutoCommit(true)
    }

    def getTransactionality: (Int, Boolean) = (inTransactionally, conn.getAutoCommit)
  }

  /**
   * Describes capabilities of the database which can be determined from a
   * DatabaseMetaData object and then cached and reused for all sessions.
   */
  class DatabaseCapabilities(session: Session) {
    val supportsBatchUpdates = session.metaData.supportsBatchUpdates
  }

  class JdbcStreamingDatabaseActionContext(subscriber: Subscriber[_], useSameThread: Boolean, database: Database, val bufferNext: Boolean) extends StreamingDatabaseActionContext(subscriber, useSameThread, database)
}

object JdbcBackend extends JdbcBackend
