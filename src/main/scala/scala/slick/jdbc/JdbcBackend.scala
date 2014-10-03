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
      * Slick comes with support for [[https://github.com/brettwooldridge/HikariCP HikariCP]] and
      * [[http://jolbox.com/ BoneCP]] which can be selected by setting `pool=HikariCP` or `pool=BoneCP`
      * respectively (or the full object name, e.g. `scala.slick.jdbc.HikariCPJdbcDataSource`).
      * 3rd-party connection pool implementations have to be specified with the fully qualified
      * name of an object implementing [[JdbcDataSourceFactory]].
      *
      * The following config keys are supported for pool settings `null`, `HikariCP` and `BoneCP`:
      * <ul>
      *   <li>`url` (String, required): JDBC URL</li>
      *   <li>`driver` (String, optional): JDBC driver class to load</li>
      *   <li>`user` (String, optional): User name</li>
      *   <li>`password` (String, optional): Password</li>
      *   <li>`autocommit` (Boolean, optional): Autocommit mode for new connections.</li>
      *   <li>`isolation` (String, optional): Isolation level for new connections. Allowed values
      *     are: `NONE`, `READ_COMMITTED`, `READ_UNCOMMITTED`, `REPEATABLE_READ`, `SERIALIZABLE`.</li>
      *   <li>`defaultCatalog` (String, optional): Default catalog for new connections.</li>
      *   <li>`readOnly` (Boolean, optional): Read Only flag for new connections.</li>
      *   <li>`properties` (Map, optional): Properties to pass to the driver.</li>
      * </ul>
      *
      * The following extra config keys are supported for pool setting `BoneCP`:
      * <ul>
      *   <li>`partitionCount` (Int, optional, default: 1): In order to reduce lock contention and
      *     thus improve performance, each incoming connection request picks off a connection from
      *     a pool that has thread-affinity. The higher this number, the better your performance
      *     will be for the case when you have plenty of short-lived threads. Beyond a certain
      *     threshold, maintenance of these pools will start to have a negative effect on
      *     performance (and only for the case when connections on a partition start running out).</li>
      *   <li>`maxConnectionsPerPartition` (Int, optional, default: 30): The number of connections
      *     to create per partition. Setting this to 5 with 3 partitions means you will have 15
      *     unique connections to the database. Note that BoneCP will not create all these
      *     connections in one go but rather start off with minConnectionsPerPartition and
      *     gradually increase connections as required.</li>
      *   <li>`minConnectionsPerPartition` (Int, optional, default: 5): The number of initial
      *     connections, per partition.</li>
      *   <li>`acquireIncrement` (Int, optional, default: 1): When the available connections are
      *     about to run out, BoneCP will dynamically create new ones in batches. This property
      *     controls how many new connections to create in one go (up to a maximum of
      *     `maxConnectionsPerPartition`). Note: This is a per-partition setting.</li>
      *   <li>`acquireRetryAttempts` (Int, optional, default: 10): After attempting to acquire a
      *     connection and failing, try to connect this number of times before giving up.</li>
      *   <li>`acquireRetryDelay` (Duration, optional, default: 1s): How long to wait before
      *     attempting to obtain a connection again after a failure.</li>
      *   <li>`connectionTimeout` (Duration, optional, default: 1s): The maximum time to wait
      *     before a call to getConnection is timed out.</li>
      *   <li>`idleMaxAge` (Duration, optional, default: 10m): Idle max age.</li>
      *   <li>`idleConnectionTestPeriod` (Duration, optional, default: 1m): This sets the time for
      *     a connection to remain idle before sending a test query to the DB. This is useful to
      *     prevent a DB from timing out connections on its end.</li>
      *   <li>`maxConnectionAge` (Duration, optional, default: 1h): The maximum connection age.</li>
      *   <li>`queryExecuteTimeLimit` (Duration, optional, default: 0 (no limit)): The maximum
      *     query execution time. Queries slower than this will be logged as a warning.</li>
      *   <li>`initSQL` (String, optional): An initial SQL statement that is run only when a
      *     connection is first created.</li>
      *   <li>`logStatements` (Boolean, optional, default: false): If enabled, log SQL statements
      *     being executed.</li>
      *   <li>`disableJMX` (Boolean, optional, default: true): Set to true to disable JMX.</li>
      *   <li>`statisticsEnabled` (Boolean, optional, default: false): If set to true, keep track
      *     of some more statistics for exposure via JMX. Will slow down the pool operation.</li>
      *   <li>`disableConnectionTracking` (Boolean, optional, default: true): If set to true, the
      *     pool will not monitor connections for proper closure. Enable this option if you only
      *     ever obtain your connections via a mechanism that is guaranteed to release the
      *     connection back to the pool (e.g. using `withSession` or `withTransaction`).</li>
      *   <li>`connectionTestStatement` (String, optional): Sets the connection test statement. The
      *     query to send to the DB to maintain keep-alives and test for dead connections. This is
      *     database specific and should be set to a query that consumes the minimal amount of load
      *     on the server. If not set (or set to null), BoneCP will issue a metadata request
      *     instead that should work on all databases but is probably slower.</li>
      * </ul>
      *
      * For pool setting `HikariCP`, all standard HikariCP configuration options are supported.
      * In cases where they have different names than the traditional Play / BoneCP names, you
      * can use both versions for compatibility (like in
      * [[http://edulify.github.io/play-hikaricp.edulify.com/ play-hikaricp]]). In particular, the
      * following config keys are supported for HikariCP:
      *
      * <ul>
      *   <li>`dataSourceClassName` (String, optional): The name of the DataSource class provided
      *     by the JDBC driver. When using HikariCP this is preferred over `driver`.
      *   <li>`driverClassName` or `driver` (String, optional): JDBC driver class to load.</li>
      *   <li>`jdbcUrl` or `url` (String, optional): The JDBC URL to be used with the driver.
      *     Required when using a driver directly, ignored when using `dataSourceClassName`. In
      *     this case you have to set the connection properties through `dataSource` or
      *     `properties`.</li>
      *   <li>`username` or `user` (String, optional): User name</li>
      *   <li>`password` (String, optional): Password</li>
      *   <li>`dataSource` or `properties` (Map, optional): Properties to pass to the Driver or
      *     DataSource.</li>
      *   <li>`connectionTimeout` (Duration, optional, default: 30s): The maximum time to wait
      *     before a call to getConnection is timed out. If this time is exceeded without a
      *     connection becoming available, a SQLException will be thrown. 100ms is the minimum
      *     value.</li>
      *   <li>`idleTimeout` or `idleMaxAge` (Duration, optional, default: 10m): The maximum amount
      *     of time that a connection is allowed to sit idle in the pool. Whether a connection is
      *     retired as idle or not is subject to a maximum variation of +30 seconds, and average
      *     variation of +15 seconds. A connection will never be retired as idle before this
      *     timeout. A value of 0 means that idle connections are never removed from the pool.</li>
      *   <li>`maxLifetime` or `maxConnectionAge` (Duration, optional, default: 30min): The maximum
      *     lifetime of a connection in the pool. When a connection reaches this timeout, even if
      *     recently used, it will be retired from the pool. An in-use connection will never be
      *     retired, only when it is idle will it be removed. A value of 0 indicates no maximum
      *     lifetime.</li>
      *   <li>`leakDetectionThreshold` (Duration, optional, default: 0): The amount of time that a
      *     connection can be out of the pool before a message is logged indicating a possible
      *     connection leak. A value of 0 means leak detection is disabled. Lowest acceptable value
      *     for enabling leak detection is 10s.</li>
      *   <li>`initializationFailFast` (Boolean, optional, default: false): Controls whether the
      *     pool will "fail fast" if the pool cannot be seeded with initial connections
      *     successfully. If connections cannot be created at pool startup time, a RuntimeException
      *     will be thrown. This property has no effect if `minimumIdle` is 0.</li>
      *   <li>`jdbc4ConnectionTest` (Boolean, optional, default: true): Determines whether the
      *     JDBC4 Connection.isValid() method is used to check that a connection is still alive.
      *     This value is mutually exclusive with the `connectionTestQuery` property, and this
      *     method of testing connection validity should be preferred if supported by the JDBC
      *     driver.</li>
      *   <li>`connectionTestQuery` or `connectionTestStatement` (String, optional): This is for
      *     "legacy" databases that do not support the JDBC4 Connection.isValid() API. This is the
      *     query that will be executed just before a connection is given to you from the pool to
      *     validate that the connection to the database is still alive. It is database dependent
      *     and should be a query that takes very little processing by the database (eg.
      *     "VALUES 1"). See the `jdbc4ConnectionTest` property for a more efficent alive test.
      *     This must be set if `jdbc4ConnectionTest` is `false`.</li>
      *   <li>`connectionInitSql` or `initSQL` (String, optional): A SQL statement that will be
      *     executed after every new connection creation before adding it to the pool. If this SQL
      *     is not valid or throws an exception, it will be treated as a connection failure and the
      *     standard retry logic will be followed.</li>
      *   <li>`maximumPoolSize` (Int, optional, default: 10): The maximum number of connections in
      *     the pool. For compatibility with the BoneCP configuration, you can also specify this
      *     as `maxConnectionsPerPartition` * `partitionCount` (where `partitionCount` defaults to
      *     1).</li>
      *   <li>`minimumIdle` (Int, optional, default: same as `maximumPoolSize`): The minimum number
      *     of connections to keep in the pool. For compatibility with the BoneCP configuration,
      *     you can also specify this as `minConnectionsPerPartition` * `partitionCount` (where
      *     `partitionCount` defaults to 1).</li>
      *   <li>`poolName` (String, optional): A user-defined name for the connection pool in logging
      *     and JMX management consoles to identify pools and pool configurations. This defaults to
      *     the config path, or an auto-generated name when creating the `Database` from the root
      *     of a `Config` object.</li>
      *   <li>`registerMbeans` (Boolean, optional, default: false): Whether or not JMX Management
      *     Beans ("MBeans") are registered. For compatibility with the BoneCP configuration, you
      *     can also speficy this via `disableJMX` (with the negated value).</li>
      *   <li>`isolateInternalQueries` (Boolean, optional, default: false): Determines whether
      *     HikariCP isolates internal pool queries, such as the connection alive test, in their
      *     own transaction. Since these are typically read-only queries, it is rarely necessary to
      *     encapsulate them in their own transaction. This property only applies if autoCommit is
      *     disabled.</li>
      *   <li>`autoCommit` or `autocommit` (Boolean, optional, default: true): Autocommit mode for
      *     new connections.</li>
      *   <li>`readOnly` (Boolean, optional, default: false): Read Only flag for new
      *     connections.</li>
      *   <li>`transactionIsolation` or `isolation` (String, optional): Isolation level for new
      *     connections. Allowed values are: `NONE`, `TRANSACTION_READ_COMMITTED` or
      *     `READ_COMMITTED`, `TRANSACTION_READ_UNCOMMITTED` or `READ_UNCOMMITTED`,
      *     `TRANSACTION_REPEATABLE_READ` or `REPEATABLE_READ`, `TRANSACTION_SERIALIZABLE` or
      *     `SERIALIZABLE`.</li>
      *   <li>`catalog` or `defaultCatalog` (String, optional): Default catalog for new
      *     connections.</li>
      * </ul>
      *
      * For asynchronous execution of database actions on top of JDBC, Slick needs to create a
      * thread pool for the database. It can be configured with the following keys:
      * <ul>
      *   <li>`threads.max` (Int, optional): The maximum number of concurrent threads. When using
      *     HikariCP or BoneCP, this defaults to the maximum number of connections, otherwise to
      *     30. It should be manually set to the correct size when using an external connection
      *     pool. Note that the automatic sizing assumes that the connection pool is used
      *     exclusively for asynchronous execution. This limit should be set lower than the actual
      *     pool size if you expect to use the pool for other calls, too.</li>
      *   <li>threads.core` (Int, optional): The maximum number of concurrent threads after which
      *     queueing is preferred to spawning new threads. The pool will extend further up to
      *     `threads.max` size when the queue is full. If not set (or set to null), the same size
      *     as `threads.max` is used.</li>
      *   <li>`threads.keepAlive` (Duration, optional, default: 1min): The time after which unused
      *     threads are shut down and removed from the pool.
      *   <li>`threads.queueSize` (Int, optional, default: 1000): The size of the queue for
      *     database actions which cannot be executed immediately when all threads are busy. When
      *     the queue is full, new threads are spawned up to `threads.max`. Beyond this limit new
      *     actions fail immediately. Set to 0 for no queue (direct hand-off) or to -1 for an
      *     unlimited queue size (not recommended).</li>
      * </ul>
      *
      * Unknown keys are ignored. Invalid values or missing mandatory keys will trigger a
      * [[SlickException]].
      *
      * The configuration settings for BoneCP are very similar to the ones supported by
      * [[http://www.playframework.com/documentation/2.4.x/SettingsJDBC Play 2.4]], with a few
      * notable differences:
      * <ul>
      *   <li>Play uses BoneCP by default. Slick requires `pool=BoneCP` to be set for that.</li>
      *   <li>Play always sets `autocommit`, `isolation` and `readOnly` when checking out a
      *     connection, with suitable default values. Slick requires explicit settings for that
      *     when using BoneCP, otherwise new connections are not modified.</li>
      *   <li>Slick has no special support for MySQL, PostgreSQL and H2 URLs. All URLs are passed
      *     directly to the pool or DriverManager.</li>
      *   <li>Slick does not support the `jndiName` setting.</li>
      *   <li>Play does not support the `properties` setting.</li>
      * </ul>
      *
      * @param path The path in the configuration file for the database configuration (e.g. `foo.bar`
      *             would find a database URL at config key `foo.bar.url`)
      * @param config The `Config` object to read from. This defaults to the global app config
      *               (e.g. in `application.conf` at the root of the class path) if not specified.
      * @param driver An optional JDBC driver to call directly. If this is set to a non-null value,
      *               the `driver` key from the configuration is ignored. The default is to use the
      *               standard lookup mechanism. The explicit driver may not be supported by all
      *               connection pools (in particular, the default [[BoneCPJdbcDataSource]]).
      */
    def forConfig(path: String, config: Config = ConfigFactory.load(), driver: Driver = null): Database = {
      val source = JdbcDataSource.forConfig(if(path.isEmpty) config else config.getConfig(path), driver, path)
      val maxThreads = if(source.maxConnections == -1) 30 else source.maxConnections
      val executor = AsyncExecutor(path, config.getConfigOr("threads"), maxThreads)
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
