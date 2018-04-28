package slick.jdbc


import org.reactivestreams.Subscriber

import scala.concurrent.{ExecutionContext, Future}

import java.util.Properties
import java.sql.{Array => _, _}
import javax.sql.DataSource
import javax.naming.InitialContext

import slick.dbio._
import slick.basic.DatabasePublisher
import slick.SlickException
import slick.relational.RelationalBackend
import slick.util._
import slick.util.ConfigExtensionMethods._

import org.slf4j.LoggerFactory
import com.typesafe.config.{ConfigFactory, Config}

/** A JDBC-based database back-end that is used by [[slick.jdbc.JdbcProfile]]. */
trait JdbcBackend extends RelationalBackend {
  type This = JdbcBackend
  type Database = DatabaseDef
  type Session = SessionDef
  type DatabaseFactory = DatabaseFactoryDef
  type Context = JdbcActionContext
  type StreamingContext = JdbcStreamingActionContext

  val Database = new DatabaseFactoryDef {}
  val backend: JdbcBackend = this

  def createDatabase(config: Config, path: String): Database = Database.forConfig(path, config)

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
    final def stream[T](a: StreamingDBIO[_, T], bufferNext: Boolean): DatabasePublisher[T] =
      createPublisher(a, s => new JdbcStreamingActionContext(s, false, DatabaseDef.this, bufferNext))

    override protected[this] def createDatabaseActionContext[T](_useSameThread: Boolean): Context =
      new JdbcActionContext { val useSameThread = _useSameThread }

    override protected[this] def createStreamingDatabaseActionContext[T](s: Subscriber[_ >: T], useSameThread: Boolean): StreamingContext =
      new JdbcStreamingActionContext(s, useSameThread, DatabaseDef.this, true)

    protected[this] def synchronousExecutionContext = executor.executionContext

    /** Run some code on the [[ioExecutionContext]]. */
    final def io[T](thunk: => T): Future[T] = Future(thunk)(ioExecutionContext)

    /** The `ExecutionContext` which is used for performing blocking database I/O, similar to how
      * `run` or `stream` would run it. This can be used for calling back into blocking JDBC APIs
      * (e.g. for materializing a LOB or mutating a result set row) from asynchronous processors of
      * unbuffered streams. */
    final def ioExecutionContext: ExecutionContext = executor.executionContext

    /** Free all resources allocated by Slick for this Database object. In particular, the
      * [[slick.util.AsyncExecutor]] with the thread pool for asynchronous execution is shut
      * down. If this object represents a connection pool managed directly by Slick, it is also
      * closed. */
    def close: Unit = try executor.close() finally source.close()
   }

  trait DatabaseFactoryDef {
    /** Create a Database based on a [[JdbcDataSource]]. */
    def forSource(source: JdbcDataSource, executor: AsyncExecutor = AsyncExecutor.default()) =
      new DatabaseDef(source, executor)

    /** Create a Database based on a DataSource.
      *
      * @param ds The DataSource to use.
      * @param maxConnections The maximum number of connections that the DataSource can provide. This is necessary to
      *                      prevent deadlocks when scheduling database actions. Use `None` if there is no hard limit.
      * @param executor The AsyncExecutor for scheduling database actions.
      * @param keepAliveConnection If this is set to true, one extra connection will be opened as soon as the database
      *                            is accessed for the first time, and kept open until `close()` is called. This is
      *                            useful for named in-memory databases in test environments.
      */
    def forDataSource(ds: DataSource, maxConnections: Option[Int], executor: AsyncExecutor = AsyncExecutor.default(), keepAliveConnection: Boolean = false): DatabaseDef =
      forSource(new DataSourceJdbcDataSource(ds, keepAliveConnection, maxConnections), executor)

    /** Create a Database based on the JNDI name of a DataSource.
      *
      * NOTE: this method will return a Slick [[DatabaseDef]] configured to use the
      * named [[DataSource]] and the passed [[AsyncExecutor]].
      * Calling this method more then once for the same [[DataSource]] name, will
      * result in different [[DatabaseDef]]s configured with different [[AsyncExecutor]]s
      * but backed by the same [[DataSource]]. This is probably not what you want.
      * Therefore, it's recommended to call it only once and re-use the returned [[DatabaseDef]] whenever needed.
      * Each [[DataSource]] should be associated with only one [[AsyncExecutor]].
      *
      * @param name The name of the DataSource to use.
      * @param maxConnections The maximum number of connections that the DataSource can provide. This is necessary to
      *                      prevent deadlocks when scheduling database actions. Use `None` if there is no hard limit.
      * @param executor The AsyncExecutor for scheduling database actions.
      */
    def forName(name: String, maxConnections: Option[Int], executor: AsyncExecutor = null): DatabaseDef =
      new InitialContext().lookup(name) match {

        case ds: DataSource =>
          val configuredExecutor =
            (executor, maxConnections) match {
              case (null, Some(maxConnec)) => AsyncExecutor.default(name, maxConnec)
              case (null, None) => AsyncExecutor.default(name)
              case (e, _) => e
            }
          forDataSource(ds, maxConnections, configuredExecutor)

        case x => throw new SlickException("Expected a DataSource for JNDI name "+name+", but got "+x)
      }

    /** Create a Database that uses the DriverManager to open new connections. */
    def forURL(url: String, user: String = null, password: String = null, prop: Properties = null, driver: String = null,
               executor: AsyncExecutor = AsyncExecutor.default(), keepAliveConnection: Boolean = false,
               classLoader: ClassLoader = ClassLoaderUtil.defaultClassLoader): DatabaseDef =
      forDataSource(new DriverDataSource(url, user, password, prop, driver, classLoader = classLoader), None, executor, keepAliveConnection)

    /** Create a Database that uses the DriverManager to open new connections. */
    def forURL(url: String, prop: Map[String, String]): Database = {
      val p = new Properties
      if(prop ne null)
        for((k,v) <- prop) if(k.ne(null) && v.ne(null)) p.setProperty(k, v)
      forURL(url, prop = p, driver = null)
    }

    /** Create a Database that directly uses a Driver to open new connections.
      * This is needed to open a JDBC URL with a driver that was not loaded by the system ClassLoader. */
    def forDriver(driver: Driver, url: String, user: String = null, password: String = null, prop: Properties = null,
                  executor: AsyncExecutor = AsyncExecutor.default()): DatabaseDef =
      forDataSource(new DriverDataSource(url, user, password, prop, driverObject = driver), None, executor)

    /** Load a database configuration through [[https://github.com/typesafehub/config Typesafe Config]].
      *
      * The main config key to set is `connectionPool`. It determines the connection pool
      * implementation to use. The default is `HikariCP` (a.k.a. `slick.jdbc.hikaricp.HikariCPJdbcDataSource$`)
      * for [[https://github.com/brettwooldridge/HikariCP HikariCP]]). This requires the "slick-hikaricp"
      * dependency to be added to your project, in addition to "slick" itself. Use `disabled` (a.k.a.
      * `slick.jdbc.DataSourceJdbcDataSource$`) to disable connection pooling and use a DataSource or
      * the DriverManager directly. A third-party connection pool implementation can be selected by
      * specifying the fully qualified name of an object implementing [[JdbcDataSourceFactory]].
      *
      * The following config keys are supported for all connection pools, both built-in and
      * third-party:
      * <ul>
      *   <li>`numThreads` (Int, optional, default: 20): The number of concurrent threads in the
      *     thread pool for asynchronous execution of database actions. See the
      *     [[https://github.com/brettwooldridge/HikariCP/wiki/About-Pool-Sizing HikariCP wiki]]
      *     for more information about sizing the thread pool correctly. Note that for asynchronous
      *     execution in Slick you should tune the thread pool size (this parameter) accordingly
      *     instead of the maximum connection pool size.</li>
      *   <li>`queueSize` (Int, optional, default: 1000): The size of the queue for database
      *     actions which cannot be executed immediately when all threads are busy. Beyond this
      *     limit new actions fail immediately. Set to 0 for no queue (direct hand-off) or to -1
      *     for an unlimited queue size (not recommended).</li>
      *   <li>`registerMbeans` (Boolean, optional, default: false): Whether or not JMX Management
      *     Beans ("MBeans") are registered. Slick supports an MBean of its own for monitoring the
      *     `AsyncExecutor` with the thread pool and queue, but connection pool implementations
      *     may register additional MBeans. In particular, HikariCP does this.</li>
      *   <li>`poolName` (String, optional): A user-defined name for the connection pool in logging
      *     and JMX management consoles to identify pools and pool configurations. This defaults to
      *     the config path.</li>
      * </ul>
      *
      * The pool is tuned for asynchronous execution by default. Apart from the connection
      * parameters you should only have to set `numThreads` and `queueSize` in most cases. In this
      * scenario there is contention over the thread pool (via its queue), not over the
      * connections, so you can have a rather large limit on the maximum number of connections
      * (based on what the database server can still handle, not what is most efficient). Slick
      * will use more connections than there are threads in the pool when sequencing non-database
      * actions inside a transaction.
      *
      * The following config keys are supported for HikariCP:
      * <ul>
      *   <li>Essentials:
      *     <ul>
      *       <li>`dataSourceClass` (String, optional): The name of the DataSource class provided by
      *         the JDBC driver. This is preferred over using `driver`. Note that `url` is ignored when
      *         this key is set (You have to use `properties` to configure the database
      *         connection instead).</li>
      *       <li>`jdbcUrl` or `url` (String, required): JDBC URL</li>
      *       <li>`username` or `user` (String, optional): User name</li>
      *       <li>`password` (String, optional): Password</li>
      *     </ul>
      *   </li>
      *   <li>Frequently used:
      *     <ul>
      *       <li>`autoCommit` (Boolean, optional, default: true): controls the default auto-commit
      *         behavior of connections returned from the pool.</li>
      *       <li>`connectionTimeout` (Duration, optional, default: 1s): The maximum time to wait
      *         before a call to getConnection is timed out. If this time is exceeded without a
      *         connection becoming available, a SQLException will be thrown. 1000ms is the minimum
      *         value.</li>
      *       <li>`idleTimeout` (Duration, optional, default: 10min): The maximum amount
      *         of time that a connection is allowed to sit idle in the pool. A value of 0 means that
      *         idle connections are never removed from the pool.</li>
      *       <li>`maxLifetime` (Duration, optional, default: 30min): The maximum lifetime of a
      *         connection in the pool. When an idle connection reaches this timeout, even if recently
      *         used, it will be retired from the pool. A value of 0 indicates no maximum
      *         lifetime.</li>
      *       <li>`connectionTestQuery` (String, optional): A statement that will be executed just
      *         before a connection is obtained from the pool to validate that the connection to the
      *         database is still alive. It is database dependent and should be a query that takes very
      *         little processing by the database (e.g. "VALUES 1"). When not set, the JDBC4
      *         `Connection.isValid()` method is used instead (which is usually preferable).</li>
      *       <li>`minimumIdle` or `minConnections` (Int, optional, default: same as `numThreads`): The minimum number
      *         of connections to keep in the pool.</li>
      *       <li>`maximumPoolSize` or `maxConnections` (Int, optional, default: `numThreads` * 5): The maximum number of
      *         connections in the pool.</li>
      *     </ul>
      *   </li>
      *   <li>Infrequently used:
      *     <ul>
      *       <li>`initializationFailTimeout` (Long, optional, default: 1): controls whether the pool will
      *         "fail fast" if the pool cannot be seeded with an initial connection successfully. Any positive
      *         number is taken to be the number of milliseconds to attempt to acquire an initial connection;
      *         the application thread will be blocked during this period. If a connection cannot be acquired
      *         before this timeout occurs, an exception will be thrown. This timeout is applied after the
      *         connectionTimeout period. If the value is zero (0), HikariCP will attempt to obtain and validate
      *         a connection. If a connection is obtained, but fails validation, an exception will be thrown and
      *         the pool not started. However, if a connection cannot be obtained, the pool will start, but later
      *         efforts to obtain a connection may fail. A value less than zero will bypass any initial connection
      *         attempt, and the pool will start immediately while trying to obtain connections in the background.
      *         Consequently, later efforts to obtain a connection may fail.
      *       <li>DEPRECATED:`initializationFailFast` (Boolean, optional, default: false): Controls whether the
      *         pool will "fail fast" if the pool cannot be seeded with initial connections
      *         successfully. If connections cannot be created at pool startup time, a RuntimeException
      *         will be thrown. This property has no effect if `minConnections` is 0.</li>
      *       <li>`isolateInternalQueries` (Boolean, optional, default: false): determines whether HikariCP
      *         isolates internal pool queries, such as the connection alive test, in their own transaction.
      *         Since these are typically read-only queries, it is rarely necessary to encapsulate them in their
      *         own transaction. This property only applies if `autoCommit` is disabled.</li>
      *       <li>`allowPoolSuspension` (Boolean, optional, default: false): controls whether the pool can be
      *         suspended and resumed through JMX. This is useful for certain failover automation scenarios.
      *         When the pool is suspended, calls to getConnection() will not timeout and will be held until
      *         the pool is resumed.</li>
      *       <li>`readOnly` (Boolean, optional): Read Only flag for new connections.</li>
      *       <li>`catalog` (String, optional): Default catalog for new connections.</li>
      *       <li>`connectionInitSql` (String, optional): A SQL statement that will be
      *         executed after every new connection creation before adding it to the pool. If this SQL
      *         is not valid or throws an exception, it will be treated as a connection failure and the
      *         standard retry logic will be followed.</li>
      *       <li>`driver` or `driverClassName` (String, optional): JDBC driver class to load</li>
      *       <li>`transactionIsolation` or `isolation` (String, optional): Transaction isolation level for new connections.
      *         Allowed values are: `NONE`, `READ_COMMITTED`, `READ_UNCOMMITTED`, `REPEATABLE_READ`,
      *         `SERIALIZABLE`.</li>
      *       <li>`validationTimeout` (Duration, optional, default: 1s): The maximum amount of time
      *         that a connection will be tested for aliveness. 1000ms is the minimum value.</li>
      *       <li>`leakDetectionThreshold` (Duration, optional, default: 0): The amount of time that a
      *         connection can be out of the pool before a message is logged indicating a possible
      *         connection leak. A value of 0 means leak detection is disabled. Lowest acceptable value
      *         for enabling leak detection is 10s.</li>
      *       <li>`schema` (String, optional): Default catalog for new connections.</li>
      *     </ul>
      *   </li>
      *   <li>Driver or DataSource configuration:
      *     <ul>
      *       <li>`properties` (Map, optional): Properties to pass to the driver or DataSource.</li>
      *     </ul>
      *   </li>
      * </ul>
      *
      * Direct connections are based on a `java.sql.DataSource` or a `java.sql.Driver`. This is
      * determined by the presence or absence of the `dataSourceClass` config key. The following
      * keys are supported for direct connections in DataSource mode:
      * <ul>
      *   <li>`dataSourceClass` (String): The name of the DataSource class provided by
      *     the JDBC driver.</li>
      *   <li>`properties` (Map, optional): Java Bean properties to set on the DataSource.</li>
      * </ul>
      *
      * The following keys are supported for direct connections in Driver mode:
      * <ul>
      *   <li>`url` (String, required): JDBC URL</li>
      *   <li>`driver` or `driverClassName` (String, optional): JDBC driver class to load. If not
      *     specified, the DriverManager is expected to already know how to handle the URL. Slick
      *     will check if the driver class is already registered before loading a new copy.</li>
      *   <li>`user` (String, optional): User name</li>
      *   <li>`password` (String, optional): Password</li>
      *   <li>`properties` (Map, optional): Connection properties to pass to the driver.</li>
      *   <li>`deregisterDriver` (Boolean, optional, default: false): If this is set to true and
      *     Slick loaded a JDBC driver when creating the Database object, it attempts to deregister
      *     that driver from the DriverManager when `close()` is called. Note that this may
      *     prevent the same driver from being registered again within the same ClassLoader through
      *     the usual automatic registration process.</li>
      * </ul>
      *
      * The following keys are supported for all direct connections:
      * <ul>
      *   <li>`isolation` (String, optional): Transaction isolation level for new connections.
      *     Allowed values are: `NONE`, `READ_COMMITTED`, `READ_UNCOMMITTED`, `REPEATABLE_READ`,
      *     `SERIALIZABLE`.</li>
      *   <li>`catalog` (String, optional): Default catalog for new connections.</li>
      *   <li>`readOnly` (Boolean, optional): Read Only flag for new connections.</li>
      *   <li>`keepAliveConnection` (Boolean, optional, default: false): If this is set to true,
      *     one extra connection will be opened as soon as the database is accessed for the first
      *     time, and kept open until `close()` is called. This is useful for named in-memory
      *     databases in test environments.</li>
      * </ul>
      *
      * Note that Driver mode is equivalent to using DataSource mode with a [[DriverDataSource]]
      * and moving the config keys `url`, `user`, `password`, `properties` and `driver` /
      * `driverClassName` down into the DataSource `properties`, except that DataSource mode
      * always uses the default ClassLoader whereas Driver mode can be used with alternate
      * ClassLoaders.
      *
      * Unknown keys are ignored. Invalid values or missing mandatory keys will trigger a
      * [[SlickException]].
      *
      * @param path The path in the configuration file for the database configuration (e.g. `foo.bar`
      *             would find a database URL at config key `foo.bar.url`) or an empty string for
      *             the top level of the `Config` object.
      * @param config The `Config` object to read from. This defaults to the global app config
      *               (e.g. in `application.conf` at the root of the class path) if not specified.
      * @param driver An optional JDBC driver to call directly. If this is set to a non-null value,
      *               the `driver` key from the configuration is ignored. The default is to use the
      *               standard lookup mechanism. The explicit driver may not be supported by all
      *               connection pools (in particular, the default HikariCPJdbcDataSource).
      * @param classLoader The ClassLoader to use to load any custom classes from. The default is to
      *                    try the context ClassLoader first and fall back to Slick's ClassLoader.
      */
    def forConfig(path: String, config: Config = null, driver: Driver = null,
                  classLoader: ClassLoader = ClassLoaderUtil.defaultClassLoader): Database = {
      val initializedConfig = if(config eq null) ConfigFactory.load(classLoader) else config
      val usedConfig = if(path.isEmpty) initializedConfig else initializedConfig.getConfig(path)
      val source = JdbcDataSource.forConfig(usedConfig, driver, path, classLoader)
      val poolName = usedConfig.getStringOr("poolName", path)
      val numThreads = usedConfig.getIntOr("numThreads", 20)
      val maxConnections = source.maxConnections.getOrElse(numThreads)
      val registerMbeans = usedConfig.getBooleanOr("registerMbeans", false)
      val executor = AsyncExecutor(poolName, numThreads, numThreads, usedConfig.getIntOr("queueSize", 1000),
        maxConnections, registerMbeans = registerMbeans)
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
    def decorateStatement[S <: Statement](statement: S): S = statement
    def fetchSize: Int = 0

    final def prepareStatement(sql: String,
                               defaultType: ResultSetType = ResultSetType.ForwardOnly,
                               defaultConcurrency: ResultSetConcurrency = ResultSetConcurrency.ReadOnly,
                               defaultHoldability: ResultSetHoldability = ResultSetHoldability.Default): PreparedStatement = {
      JdbcBackend.logStatement("Preparing statement", sql)
      val s = loggingPreparedStatement(decorateStatement(resultSetHoldability.withDefault(defaultHoldability) match {
        case ResultSetHoldability.Default =>
          val rsType = resultSetType.withDefault(defaultType).intValue
          val rsConc = resultSetConcurrency.withDefault(defaultConcurrency).intValue
          if(rsType == ResultSet.TYPE_FORWARD_ONLY && rsConc == ResultSet.CONCUR_READ_ONLY)
            conn.prepareStatement(sql)
          else
            conn.prepareStatement(sql, rsType, rsConc)
        case h =>
          conn.prepareStatement(sql, resultSetType.withDefault(defaultType).intValue,
            resultSetConcurrency.withDefault(defaultConcurrency).intValue,
            h.intValue)
      }))
      if(fetchSize != 0) s.setFetchSize(fetchSize)
      s
    }

    final def prepareInsertStatement(sql: String, columnNames: Array[String] = new Array[String](0)): PreparedStatement = {
      if(JdbcBackend.statementLogger.isDebugEnabled)
        JdbcBackend.logStatement("Preparing insert statement (returning: "+columnNames.mkString(",")+")", sql)
      val s = loggingPreparedStatement(decorateStatement(conn.prepareStatement(sql, columnNames)))
      if(fetchSize != 0) s.setFetchSize(fetchSize)
      s
    }

    final def prepareInsertStatement(sql: String, columnIndexes: Array[Int]): PreparedStatement = {
      if(JdbcBackend.statementLogger.isDebugEnabled)
        JdbcBackend.logStatement("Preparing insert statement (returning indexes: "+columnIndexes.mkString(",")+")", sql)
      val s = loggingPreparedStatement(decorateStatement(conn.prepareStatement(sql, columnIndexes)))
      if(fetchSize != 0) s.setFetchSize(fetchSize)
      s
    }

    final def createStatement(defaultType: ResultSetType = ResultSetType.ForwardOnly,
                              defaultConcurrency: ResultSetConcurrency = ResultSetConcurrency.ReadOnly,
                              defaultHoldability: ResultSetHoldability = ResultSetHoldability.Default): Statement = {
      val s = loggingStatement(decorateStatement(resultSetHoldability.withDefault(defaultHoldability) match {
        case ResultSetHoldability.Default =>
          conn.createStatement(resultSetType.withDefault(defaultType).intValue,
            resultSetConcurrency.withDefault(defaultConcurrency).intValue)
        case h =>
          conn.createStatement(resultSetType.withDefault(defaultType).intValue,
            resultSetConcurrency.withDefault(defaultConcurrency).intValue,
            h.intValue)
      }))
      if(fetchSize != 0) s.setFetchSize(fetchSize)
      s
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

    def force(): Unit = { conn }

    private[slick] final def internalForParameters(rsType: ResultSetType, rsConcurrency: ResultSetConcurrency,
                      rsHoldability: ResultSetHoldability, statementInit: Statement => Unit, _fetchSize: Int): Session = new Session {
      override def resultSetType = rsType
      override def resultSetConcurrency = rsConcurrency
      override def resultSetHoldability = rsHoldability
      override def fetchSize = _fetchSize
      override def decorateStatement[S <: Statement](statement: S): S = {
        if(statementInit ne null) statementInit(statement)
        statement
      }
      def database = self.database
      def conn = self.conn
      def metaData = self.metaData
      def capabilities = self.capabilities
      def close() = self.close()
      private[slick] def startInTransaction: Unit = self.startInTransaction
      private[slick] def endInTransaction(f: => Unit): Unit = self.endInTransaction(f)
    }

    protected def loggingStatement(st: Statement): Statement =
      if(JdbcBackend.statementLogger.isDebugEnabled || JdbcBackend.benchmarkLogger.isDebugEnabled)
        new LoggingStatement(st) else st

    protected def loggingPreparedStatement(st: PreparedStatement): PreparedStatement =
      if(JdbcBackend.statementLogger.isDebugEnabled || JdbcBackend.benchmarkLogger.isDebugEnabled || JdbcBackend.parameterLogger.isDebugEnabled)
        new LoggingPreparedStatement(st) else st

    /** Start a `transactionally` block */
    private[slick] def startInTransaction: Unit
    /** End a `transactionally` block, running the specified function first if it is the outermost one. */
    private[slick] def endInTransaction(f: => Unit): Unit
  }

  class BaseSession(val database: Database) extends SessionDef {
    protected var inTransactionally = 0

    def isInTransaction = inTransactionally > 0

    val conn = database.source.createConnection

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

    def close(): Unit = { conn.close() }

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

  trait JdbcActionContext extends BasicActionContext {
    private[JdbcBackend] var statementParameters: List[JdbcBackend.StatementParameters] = null

    def pushStatementParameters(p: JdbcBackend.StatementParameters): Unit = {
      val p2 = if((p.rsType eq null) || (p.rsConcurrency eq null) || (p.rsHoldability eq null) || (p.statementInit eq null)) {
        val curr = if(statementParameters eq null) JdbcBackend.defaultStatementParameters else statementParameters.head
        JdbcBackend.StatementParameters(
          if(p.rsType eq null) curr.rsType else p.rsType,
          if(p.rsConcurrency eq null) curr.rsConcurrency else p.rsConcurrency,
          if(p.rsHoldability eq null) curr.rsHoldability else p.rsHoldability,
          if(p.statementInit eq null) curr.statementInit
          else if(curr.statementInit eq null) p.statementInit
          else { s => curr.statementInit(s); p.statementInit(s) },
          p.fetchSize
        )
      } else p
      statementParameters = p2 :: (if(statementParameters eq null) Nil else statementParameters)
    }

    def popStatementParameters: Unit = {
      val p = statementParameters.tail
      if(p.isEmpty) statementParameters = null else statementParameters = p
    }

    /* TODO: Creating a new Session here for parameter overrides is not the most efficient solution
       but it provides compatibility with the old Session-based API. This should be changed once
       the old API has been removed. */
    override def session: Session =
      if(statementParameters eq null) super.session
      else {
        val p = statementParameters.head
        super.session.internalForParameters(p.rsType, p.rsConcurrency, p.rsHoldability, p.statementInit, p.fetchSize)
      }

    /** The current JDBC Connection */
    def connection: Connection = session.conn
  }

  class JdbcStreamingActionContext(subscriber: Subscriber[_], useSameThread: Boolean, database: Database, val bufferNext: Boolean) extends BasicStreamingActionContext(subscriber, useSameThread, database) with JdbcActionContext
}

object JdbcBackend extends JdbcBackend {
  case class StatementParameters(rsType: ResultSetType, rsConcurrency: ResultSetConcurrency,
                                 rsHoldability: ResultSetHoldability, statementInit: Statement => Unit, fetchSize: Int)
  val defaultStatementParameters = StatementParameters(ResultSetType.Auto, ResultSetConcurrency.Auto, ResultSetHoldability.Auto, null, 0)

  protected[jdbc] lazy val statementLogger = new SlickLogger(LoggerFactory.getLogger(classOf[JdbcBackend].getName+".statement"))
  protected[jdbc] lazy val benchmarkLogger = new SlickLogger(LoggerFactory.getLogger(classOf[JdbcBackend].getName+".benchmark"))
  protected[jdbc] lazy val parameterLogger = new SlickLogger(LoggerFactory.getLogger(classOf[JdbcBackend].getName+".parameter"))

  protected[jdbc] def logStatement(msg: String, stmt: String) = if(statementLogger.isDebugEnabled) {
    val s = if(GlobalConfig.sqlIndent) msg + ":\n" + LogUtil.multilineBorder(stmt) else msg + ": " + stmt
    statementLogger.debug(s)
  }
}
