package slick.jdbc

import scala.util.control.NonFatal
import scala.concurrent.duration.FiniteDuration

import java.util.Properties
import java.sql.{Array => _, *}
import javax.sql.DataSource
import javax.naming.InitialContext

import cats.effect.{Async, Resource}

import fs2.Stream

import slick.dbio.*
import slick.SlickException
import slick.relational.RelationalBackend
import slick.basic.ConcurrencyControl.*
import slick.util.*
import slick.util.ConfigExtensionMethods.*

import org.slf4j.LoggerFactory
import com.typesafe.config.{ConfigFactory, Config}

/** A JDBC-based database back-end that is used by [[slick.jdbc.JdbcProfile]]. */
trait JdbcBackend extends RelationalBackend {
  type Database[F[_]] = JdbcDatabaseDef[F]
  type Session = JdbcSessionDef
  type DatabaseFactory = DatabaseFactoryDef
  type Context = JdbcActionContext
  type StreamingContext = JdbcStreamingActionContext

  val Database: DatabaseFactory = new DatabaseFactoryDef {}
  val backend: JdbcBackend = this

  def createDatabase[F[_]: Async](config: Config, path: String, classLoader: ClassLoader = ClassLoaderUtil.defaultClassLoader): Resource[F, Database[F]] =
    Database.forConfig[F](path, config, classLoader = classLoader)

  /** A JDBC database connected to a given DataSource.
    *
    * `F[_]` is the effect type (e.g. `cats.effect.IO`).
    * Construct via `Database.forConfig[IO](...)` etc. — this returns a `Resource[IO, Database[IO]]`
    * that manages connection pool lifecycle.
    */
  abstract class JdbcDatabaseDef[F[_]](
    val source: JdbcDataSource,
    override val controls: Controls[F]
  )(
    override implicit val asyncF: Async[F]
  ) extends BasicDatabaseDef[F] {

    /** DatabaseCapabilities, lazily determined and cached from the first Session. */
    @volatile
    protected[JdbcBackend] var capabilities: DatabaseCapabilities = null

    def createSession(): Session = new BaseSession[F](this)

    /** Free all resources allocated by Slick for this Database object.
      * If this object represents a connection pool managed directly by Slick, it is also closed. */
    def close(): Unit = source.close()

    /** Run a blocking thunk on the CE3 blocking thread pool.
      *
      * Useful for blocking JDBC work (e.g. materializing a LOB) that needs to happen outside
      * a DBIO action. Equivalent to `cats.effect.Async[F].blocking(thunk)`. */
    final def io[T](thunk: => T): F[T] = asyncF.blocking(thunk)

    // ------------------------------------------------------------------
    // Transaction helpers — called by the interpreter
    // ------------------------------------------------------------------

    override protected def setupTransaction(session: Session, isolationLevel: Option[Int]): Option[Int] = {
      val prev = isolationLevel.map(_ => session.conn.getTransactionIsolation)
      isolationLevel.foreach(session.conn.setTransactionIsolation)
      session.conn.setAutoCommit(false)
      prev
    }

    override protected def commitTransaction(session: Session, previousIsolationLevel: Option[Int]): Unit =
      try session.conn.commit()
      finally {
        previousIsolationLevel.foreach(session.conn.setTransactionIsolation)
        session.conn.setAutoCommit(true)
      }

    override protected def rollbackTransaction(session: Session, previousIsolationLevel: Option[Int]): Unit =
      try session.conn.rollback()
      catch { case NonFatal(_) => () }
      finally
        try {
          previousIsolationLevel.foreach(session.conn.setTransactionIsolation)
          session.conn.setAutoCommit(true)
        }
        catch { case NonFatal(_) => () }

    // ------------------------------------------------------------------
    // Session → Context bridge
    // ------------------------------------------------------------------

    override protected def sessionAsContext(session: Session, state: ExecState): Context = {
      val outerSession = session
      val depth = state.transactionDepth
      val pinned = state.pinned
      new JdbcActionContext {
        override def rawSession: JdbcSessionDef = outerSession
        override def transactionDepth: Int = depth
        override def isPinned: Boolean = pinned
      }
    }

    // ------------------------------------------------------------------
    // Streaming
    // ------------------------------------------------------------------

    override protected def streamFromSDA[T](
      a: SynchronousDatabaseAction[?, Streaming[T], Context, StreamingContext, Nothing],
      session: Session,
      state: ExecState
    ): Stream[F, T] = {
      val F = asyncF
      val outerSession = session
      val outerTransactionDepth = state.transactionDepth
      val outerPinned = state.pinned
      // emitStream drives the SDA one element at a time.
      // We use AnyRef as the state type since SynchronousDatabaseAction.StreamState is an
      // instance-level type member and cannot be named from outside the instance.
      // We capture `a` in a stable val to allow path-dependent type access.
      val sda = a.asInstanceOf[SynchronousDatabaseAction[?, Streaming[T], JdbcActionContext, JdbcStreamingActionContext, Nothing]]

      // Factory for a JdbcStreamingActionContext that closes over the outer session/tx state
      // and delegates emit() to the supplied callback.  Used both for normal streaming and
      // for the cancelStream() call so the action always sees a consistent context.
      def mkStreamingCtx(onEmit: Any => Unit): JdbcStreamingActionContext =
        new JdbcStreamingActionContext {
          override def rawSession: JdbcSessionDef = outerSession
          override def transactionDepth: Int = outerTransactionDepth
          override def isPinned: Boolean = outerPinned
          override def emit(v: Any): Unit = onEmit(v)
        }

      // SDA streaming uses null as the initial/sentinel StreamState by API contract:
      //   null input  = start a new stream
      //   null return = stream exhausted
      // We name it once here and reuse below to avoid repeating the cast.
      val initialState = null.asInstanceOf[sda.StreamState]

      // State is Option[sda.StreamState]:
      //   Some(st) = active (st may be initialState on first call)
      //   None     = exhausted (stop unfolding)
      //
      // We use unfoldChunkEval rather than unfoldEval so that a step can produce zero
      // elements while still advancing state.  This is necessary for MutatingResultAction
      // when the row-loop exhausts the result set and transitions to end-marker-pending
      // state (state==1) without emitting anything: the next step then emits the end marker.
      // With unfoldEval a no-emission step would have to either stop the stream or duplicate
      // state-advancement logic unsafely.
      //
      // liveState tracks the most recent StreamState so that cancelStream() can be called
      // with it if the stream is cancelled mid-way.
      Stream.eval(F.ref[Option[sda.StreamState]](Some(initialState))).flatMap { liveState =>
        Stream.unfoldChunkEval[F, Option[sda.StreamState], T](Some(initialState)) {
          case None =>
            // Stream already exhausted — should not be called, but guard anyway.
            F.flatMap(liveState.set(None))(_ => F.pure(None))

          case Some(st) =>
            F.flatMap(F.blocking {
              var emitted: Option[T] = None
              val ctx2 = mkStreamingCtx { v =>
                // The value is provided through onEmit (the SDA push callback), so we
                // capture it into a local cell to pair it with the returned next state.
                emitted = Some(v.asInstanceOf[T])
              }
              val nextState: Option[sda.StreamState] = Option(sda.emitStream(ctx2, 1L, st))

              (emitted, nextState)
            }) { case (emitted, nextState) =>
              // Update liveState inside an uncancelable region so that cancellation
              // cannot arrive between F.blocking returning the new state and liveState
              // being set.  Without this, the onFinalizeCase handler could read a stale
              // StreamState and call cancelStream with it, potentially skipping ResultSet
              // cleanup.
              F.uncancelable { _ =>
                F.flatMap(liveState.set(nextState)) { _ =>
                  F.pure(
                    // unfoldChunkEval: None = stop stream; Some((chunk, s)) = emit chunk and continue.
                    // - If nextState is Some: more steps to come; emit element if any (may be empty chunk).
                    // - If nextState is None and emitted is Some: last element, then stop.
                    // - If nextState is None and emitted is None: stop immediately (exhausted).
                    (nextState, emitted) match {
                      case (Some(_), _)   => Some((fs2.Chunk.fromOption(emitted), nextState))
                      case (None, Some(v)) => Some((fs2.Chunk.singleton(v), None))
                      case (None, None)    => None
                    }
                  )
                }
              }
            }
        }.onFinalizeCase {
          case Resource.ExitCase.Canceled =>
            // Call the action's own cancel hook so it can close open ResultSets /
            // iterators that would otherwise be abandoned until the connection closes.
            // emitStream error paths self-dispose by contract, so we only need this
            // for the Canceled case.  Errors are swallowed so as not to mask cancellation.
            F.flatMap(liveState.get) {
              case Some(st) if st ne null =>
                F.flatMap(F.attempt(F.blocking(sda.cancelStream(mkStreamingCtx(_ => ()), st))))(_ => F.unit)
              case _ =>
                F.unit
            }
          case _ =>
            F.unit
        }
      }
    }
  }

  // --------------------------------------------------------------------------
  // DatabaseFactory
  // --------------------------------------------------------------------------

  trait DatabaseFactoryDef {

    /** Create a Database backed by a [[JdbcDataSource]].
      * Returns a `Resource[G, Database[G]]` that manages source lifecycle.
      *
      * @param source          The underlying data source.
      * @param maxConnections  The maximum number of concurrent connections available for work.
      *                        When the source is a [[DataSourceJdbcDataSource]] with
      *                        `keepAliveConnection` enabled, one additional connection is opened
      *                        to keep named in-memory databases (e.g. H2, HSQLDB) alive; that
      *                        connection is not counted against this limit.  If the underlying
      *                        `DataSource` in that case is bounded (e.g. a connection pool), it
      *                        must be sized to at least `maxConnections + 1` to accommodate the
      *                        keep-alive connection.
      */
    def forSource[G[_]](
      source: JdbcDataSource,
      maxConnections: Option[Int] = None,
      queueSize: Int = 1000,
      maxInflightActions: Option[Int] = None,
      inflightAdmissionTimeout: Option[FiniteDuration] = None,
      connectionAcquireTimeout: Option[FiniteDuration] = None
    )(implicit AG: Async[G]): Resource[G, Database[G]] = {
      val n = maxConnections.getOrElse(20)
      val maxInflight = math.max(n, maxInflightActions.getOrElse(n * 2))
      require(n > 0, s"maxConnections must be > 0, got $n")
      Resource.make(
        AG.flatMap(Controls.create[G](n.toLong, queueSize.toLong, maxInflight.toLong, inflightAdmissionTimeout, connectionAcquireTimeout)) { controls =>
          AG.pure(new JdbcDatabaseDef[G](source, controls) {
            override implicit val asyncF: Async[G] = AG
          })
        }
      )(db => AG.blocking(db.close())).evalTap { _ =>
        // The keep-alive connection prevents named in-memory databases (e.g. H2, HSQLDB)
        // from being destroyed when no other connections are open.  It is opened eagerly
        // here so that the database survives the period between pool construction and the
        // first DBIO action.  This connection is held for the lifetime of the database and
        // is separate from the maxConnections working connections.
        source match {
          case ds: DataSourceJdbcDataSource if ds.keepAliveConnection =>
            AG.blocking(ds.openKeepAlive())
          case _ => AG.unit
        }
      }
    }

    /** Create a Database from a `javax.sql.DataSource`. */
    def forDataSource[G[_]: Async](
      ds: DataSource,
      maxConnections: Option[Int],
      keepAliveConnection: Boolean = false
    ): Resource[G, Database[G]] =
      forSource[G](new DataSourceJdbcDataSource(ds, keepAliveConnection, maxConnections), maxConnections = maxConnections)

    /** Create a Database based on the JNDI name of a DataSource. */
    def forName[G[_]: Async](name: String, maxConnections: Option[Int]): Resource[G, Database[G]] =
      new InitialContext().lookup(name) match {
        case ds: DataSource =>
          forDataSource[G](ds, maxConnections)
        case x =>
          throw new SlickException("Expected a DataSource for JNDI name " + name + ", but got " + x)
      }

    /** Create a Database using the DriverManager to open new connections. */
    def forURL[G[_]: Async](
      url: String,
      user: String = null,
      password: String = null,
      prop: Properties = null,
      driver: String = null,
      keepAliveConnection: Boolean = false,
      classLoader: ClassLoader = ClassLoaderUtil.defaultClassLoader
    ): Resource[G, Database[G]] =
      forDataSource[G](
        new DriverDataSource(url, user, password, prop, driver, classLoader = classLoader),
        maxConnections = None,
        keepAliveConnection = keepAliveConnection
      )

    /** Create a Database using the DriverManager with a property map. */
    def forURL[G[_]: Async](url: String, prop: Map[String, String]): Resource[G, Database[G]] = {
      val p = new Properties
      if (prop ne null)
        for ((k, v) <- prop) if (k.ne(null) && v.ne(null)) p.setProperty(k, v)
      forURL[G](url, prop = p, driver = null)
    }

    /** Create a Database that directly uses a Driver to open new connections. */
    def forDriver[G[_]: Async](
      driver: Driver,
      url: String,
      user: String = null,
      password: String = null,
      prop: Properties = null
    ): Resource[G, Database[G]] =
      forDataSource[G](new DriverDataSource(url, user, password, prop, driverObject = driver), None)

    /** Load a database configuration through [[https://github.com/typesafehub/config Typesafe Config]].
      *
      * Returns a `Resource[G, Database[G]]` — the Resource finalizer closes the connection pool.
      *
      * Supported config keys:
      * <ul>
      *   <li>`maxConnections` (Int, default 20) — max concurrent JDBC connections</li>
      *   <li>`numThreads` (Int, legacy fallback) — treated as `maxConnections` when `maxConnections` is not set</li>
      *   <li>`poolName` (String, optional) — name for logging</li>
      * </ul>
      *
      * @param path   The path in the config file (e.g. `"mydb"`), or empty string for root.
      * @param config The `Config` object to read from (defaults to `ConfigFactory.load()`).
      * @param driver An optional JDBC driver to call directly.
      * @param classLoader ClassLoader for custom classes.
      */
    def forConfig[G[_]: Async](
      path: String,
      config: Config = null,
      driver: Driver = null,
      classLoader: ClassLoader = ClassLoaderUtil.defaultClassLoader
    ): Resource[G, Database[G]] = {
      val initializedConfig = if (config eq null) ConfigFactory.load(classLoader) else config
      val usedConfig = if (path.isEmpty) initializedConfig else initializedConfig.getConfig(path)
      val source = JdbcDataSource.forConfig(usedConfig, driver, path, classLoader)
      val configuredMaxConnections = usedConfig.getIntOpt("maxConnections").orElse(usedConfig.getIntOpt("numThreads")).getOrElse(20)
      val maxConnections = source.maxConnections.getOrElse(configuredMaxConnections)
      val queueSize = usedConfig.getIntOr("queueSize", 1000)
      val maxInflightActions = usedConfig.getIntOpt("maxInflightActions")
      val inflightAdmissionTimeout = usedConfig.getFiniteDurationOpt("inflightAdmissionTimeout")
      val connectionAcquireTimeout = usedConfig.getFiniteDurationOpt("connectionAcquireTimeout")

      forSource[G](
        source,
        maxConnections = Some(maxConnections),
        queueSize = queueSize,
        maxInflightActions = maxInflightActions,
        inflightAdmissionTimeout = inflightAdmissionTimeout,
        connectionAcquireTimeout = connectionAcquireTimeout
      )
    }
  }

  // --------------------------------------------------------------------------
  // JdbcSessionDef
  // --------------------------------------------------------------------------

  trait JdbcSessionDef extends BasicSessionDef { self =>

    def database: AnyDatabaseDef
    def conn: Connection
    def metaData: DatabaseMetaData
    def capabilities: DatabaseCapabilities

    def resultSetType: ResultSetType = ResultSetType.Auto
    def resultSetConcurrency: ResultSetConcurrency = ResultSetConcurrency.Auto
    def resultSetHoldability: ResultSetHoldability = ResultSetHoldability.Auto
    def decorateStatement[S <: Statement](statement: S): S = statement
    def fetchSize: Int = 0

    final def prepareStatement(
      sql: String,
      defaultType: ResultSetType = ResultSetType.ForwardOnly,
      defaultConcurrency: ResultSetConcurrency = ResultSetConcurrency.ReadOnly,
      defaultHoldability: ResultSetHoldability = ResultSetHoldability.Default,
      fetchSizeOverride: Option[Int] = None
    ): PreparedStatement = {
      JdbcBackend.logStatement("Preparing statement", sql)
      val s = loggingPreparedStatement(decorateStatement(resultSetHoldability.withDefault(defaultHoldability) match {
        case ResultSetHoldability.Default =>
          val rsType = resultSetType.withDefault(defaultType).intValue
          val rsConc = resultSetConcurrency.withDefault(defaultConcurrency).intValue
          if (rsType == ResultSet.TYPE_FORWARD_ONLY && rsConc == ResultSet.CONCUR_READ_ONLY)
            conn.prepareStatement(sql)
          else
            conn.prepareStatement(sql, rsType, rsConc)
        case h =>
          conn.prepareStatement(
            sql,
            resultSetType.withDefault(defaultType).intValue,
            resultSetConcurrency.withDefault(defaultConcurrency).intValue,
            h.intValue
          )
      }))
      val computedFetchSize = fetchSizeOverride.getOrElse(fetchSize)
      if (computedFetchSize != 0) s.setFetchSize(computedFetchSize)
      s
    }

    final def prepareInsertStatement(sql: String, columnNames: Array[String] = new Array[String](0)): PreparedStatement = {
      if (JdbcBackend.statementLogger.isDebugEnabled)
        JdbcBackend.logStatement("Preparing insert statement (returning: " + columnNames.mkString(",") + ")", sql)
      val s = loggingPreparedStatement(decorateStatement(conn.prepareStatement(sql, columnNames)))
      if (fetchSize != 0) s.setFetchSize(fetchSize)
      s
    }

    final def prepareInsertStatement(sql: String, columnIndexes: Array[Int]): PreparedStatement = {
      if (JdbcBackend.statementLogger.isDebugEnabled)
        JdbcBackend.logStatement("Preparing insert statement (returning indexes: " + columnIndexes.mkString(",") + ")", sql)
      val s = loggingPreparedStatement(decorateStatement(conn.prepareStatement(sql, columnIndexes)))
      if (fetchSize != 0) s.setFetchSize(fetchSize)
      s
    }

    final def createStatement(
      defaultType: ResultSetType = ResultSetType.ForwardOnly,
      defaultConcurrency: ResultSetConcurrency = ResultSetConcurrency.ReadOnly,
      defaultHoldability: ResultSetHoldability = ResultSetHoldability.Default
    ): Statement = {
      val s = loggingStatement(decorateStatement(resultSetHoldability.withDefault(defaultHoldability) match {
        case ResultSetHoldability.Default =>
          conn.createStatement(
            resultSetType.withDefault(defaultType).intValue,
            resultSetConcurrency.withDefault(defaultConcurrency).intValue
          )
        case h =>
          conn.createStatement(
            resultSetType.withDefault(defaultType).intValue,
            resultSetConcurrency.withDefault(defaultConcurrency).intValue,
            h.intValue
          )
      }))
      if (fetchSize != 0) s.setFetchSize(fetchSize)
      s
    }

    final def withPreparedStatement[T](
      sql: String,
      defaultType: ResultSetType = ResultSetType.ForwardOnly,
      defaultConcurrency: ResultSetConcurrency = ResultSetConcurrency.ReadOnly,
      defaultHoldability: ResultSetHoldability = ResultSetHoldability.Default
    )(f: PreparedStatement => T): T = {
      val st = prepareStatement(sql, defaultType, defaultConcurrency, defaultHoldability)
      try f(st) finally st.close()
    }

    final def withPreparedInsertStatement[T](
      sql: String,
      columnNames: Array[String] = new Array[String](0)
    )(f: PreparedStatement => T): T = {
      val st = prepareInsertStatement(sql, columnNames)
      try f(st) finally st.close()
    }

    final def withPreparedInsertStatement[T](
      sql: String,
      columnIndexes: Array[Int]
    )(f: PreparedStatement => T): T = {
      val st = prepareInsertStatement(sql, columnIndexes)
      try f(st) finally st.close()
    }

    final def withStatement[T](
      defaultType: ResultSetType = ResultSetType.ForwardOnly,
      defaultConcurrency: ResultSetConcurrency = ResultSetConcurrency.ReadOnly,
      defaultHoldability: ResultSetHoldability = ResultSetHoldability.Default
    )(f: Statement => T): T = {
      val st = createStatement(defaultType, defaultConcurrency, defaultHoldability)
      try f(st) finally st.close()
    }

    def close(): Unit

    def force(): Unit = { conn }

    private[slick] final def internalForParameters(
      rsType: ResultSetType,
      rsConcurrency: ResultSetConcurrency,
      rsHoldability: ResultSetHoldability,
      statementInit: Statement => Unit,
      _fetchSize: Int
    ): Session = new Session {
      override def resultSetType        = rsType
      override def resultSetConcurrency = rsConcurrency
      override def resultSetHoldability = rsHoldability
      override def fetchSize            = _fetchSize
      override def decorateStatement[S <: Statement](statement: S): S = {
        if (statementInit ne null) statementInit(statement)
        statement
      }
      def database   = self.database
      def conn       = self.conn
      def metaData   = self.metaData
      def capabilities = self.capabilities
      def close()    = self.close()
    }

    protected def loggingStatement(st: Statement): Statement =
      if (JdbcBackend.statementLogger.isDebugEnabled || JdbcBackend.benchmarkLogger.isDebugEnabled ||
          JdbcBackend.statementAndParameterLogger.isDebugEnabled)
        new LoggingStatement(st) else st

    protected def loggingPreparedStatement(st: PreparedStatement): PreparedStatement =
      if (JdbcBackend.statementLogger.isDebugEnabled || JdbcBackend.benchmarkLogger.isDebugEnabled ||
          JdbcBackend.parameterLogger.isDebugEnabled || JdbcBackend.statementAndParameterLogger.isDebugEnabled)
        new LoggingPreparedStatement(st) else st
  }

  class BaseSession[F[_]](val database: Database[F]) extends JdbcSessionDef {
    // Connection is opened eagerly when the session is created.
    // The BasicBackend.withSession helper controls when createSession() is called.
    val conn = database.source.createConnection()

    lazy val metaData = conn.getMetaData()

    def capabilities = {
      val dc = database.capabilities
      if (dc ne null) dc
      else {
        val newDC = new DatabaseCapabilities(this)
        database.capabilities = newDC
        newDC
      }
    }

    /** Mutable stack of statement parameters, shared across all JdbcActionContext instances
      * that use this session. This allows PushStatementParameters / PopStatementParameters
      * to survive across multiple SDA invocations within the same session. */
    var statementParameters: List[JdbcBackend.StatementParameters] = null

    def close(): Unit = { conn.close() }
  }

  /**
   * Describes capabilities of the database which can be determined from a
   * DatabaseMetaData object and then cached and reused for all sessions.
   */
  class DatabaseCapabilities(session: Session) {
    val supportsBatchUpdates = session.metaData.supportsBatchUpdates
  }

  // --------------------------------------------------------------------------
  // Action contexts
  // --------------------------------------------------------------------------

  trait JdbcActionContext extends BasicActionContext {
    /** The underlying session, set once at context creation. */
    def rawSession: Session

    /** Statement parameters stack — stored on the BaseSession so it persists
      * across multiple JdbcActionContext instances within the same session. */
    private def sessionParams: List[JdbcBackend.StatementParameters] =
      rawSession match {
        case bs: BaseSession[_] => bs.statementParameters
        case _               => null
      }
    private def sessionParams_=(v: List[JdbcBackend.StatementParameters]): Unit =
      rawSession match {
        case bs: BaseSession[_] => bs.statementParameters = v
        case _               => () // fallback: ignore (keepAlive delegate sessions)
      }

    def pushStatementParameters(p: JdbcBackend.StatementParameters): Unit = {
      val sp = sessionParams
      val p2 = if ((p.rsType eq null) || (p.rsConcurrency eq null) || (p.rsHoldability eq null) || (p.statementInit eq null)) {
        val curr = if (sp eq null) JdbcBackend.defaultStatementParameters else sp.head
        JdbcBackend.StatementParameters(
          if (p.rsType eq null) curr.rsType else p.rsType,
          if (p.rsConcurrency eq null) curr.rsConcurrency else p.rsConcurrency,
          if (p.rsHoldability eq null) curr.rsHoldability else p.rsHoldability,
          if (p.statementInit eq null) curr.statementInit
          else if (curr.statementInit eq null) p.statementInit
          else { s => curr.statementInit(s); p.statementInit(s) },
          p.fetchSize
        )
      } else p
      sessionParams = p2 :: (if (sp eq null) Nil else sp)
    }

    def popStatementParameters: Unit = {
      val p = sessionParams.tail
      sessionParams = if (p.isEmpty) null else p
    }

    override def session: Session = {
      val sp = sessionParams
      if (sp eq null) rawSession
      else {
        val p = sp.head
        rawSession.internalForParameters(p.rsType, p.rsConcurrency, p.rsHoldability, p.statementInit, p.fetchSize)
      }
    }

    /** The current JDBC Connection */
    def connection: Connection = session.conn
  }

  trait JdbcStreamingActionContext extends BasicStreamingActionContext with JdbcActionContext
}

object JdbcBackend extends JdbcBackend {
  case class StatementParameters(
    rsType: ResultSetType,
    rsConcurrency: ResultSetConcurrency,
    rsHoldability: ResultSetHoldability,
    statementInit: Statement => Unit,
    fetchSize: Int
  )
  val defaultStatementParameters =
    StatementParameters(ResultSetType.Auto, ResultSetConcurrency.Auto, ResultSetHoldability.Auto, null, 0)

  protected[jdbc] lazy val statementLogger =
    new SlickLogger(LoggerFactory.getLogger(classOf[JdbcBackend].getName + ".statement"))
  protected[jdbc] lazy val benchmarkLogger =
    new SlickLogger(LoggerFactory.getLogger(classOf[JdbcBackend].getName + ".benchmark"))
  protected[jdbc] lazy val parameterLogger =
    new SlickLogger(LoggerFactory.getLogger(classOf[JdbcBackend].getName + ".parameter"))
  protected[jdbc] lazy val statementAndParameterLogger =
    new SlickLogger(LoggerFactory.getLogger(classOf[JdbcBackend].getName + ".statementAndParameter"))

  protected[jdbc] def logStatement(msg: String, stmt: String) =
    if (statementLogger.isDebugEnabled) {
      val s = if (GlobalConfig.sqlIndent) msg + ":\n" + LogUtil.multilineBorder(stmt) else msg + ": " + stmt
      statementLogger.debug(s)
    }
}
