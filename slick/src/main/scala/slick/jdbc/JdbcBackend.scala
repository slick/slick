package slick.jdbc

import scala.util.control.NonFatal

import java.sql.{Array => _, *}

import cats.effect.Async

import slick.relational.RelationalBackend
import slick.basic.ConcurrencyControl.*
import slick.util.*
import slick.util.ConfigExtensionMethods.*

import org.slf4j.LoggerFactory

/** A JDBC-based database back-end that is used by [[slick.jdbc.JdbcProfile]]. */
trait JdbcBackend extends RelationalBackend {
  type Database[F[_]] = JdbcDatabaseDef[F]
  type Session = JdbcSessionDef
  type Context = JdbcActionContext

  val backend: JdbcBackend = this

  override def makeDatabase[F[_]: Async](config: slick.basic.BasicDatabaseConfig[?]): F[Database[F]] = {
    // If the config has a "db" sub-section use it (the {profile=..., db={...}} format),
    // otherwise use the config directly (flat format where datasource keys are at the top level).
    val dbConfig = config.config.getConfigOr("db", config.config)
    val source = JdbcDataSource.forConfig(dbConfig, null, config.path, config.classLoader)
    val cc = config.controls
    val n  = source.maxConnections.getOrElse(cc.maxConnections)
    makeDatabaseWithSource[F](source, cc.copy(maxConnections = n))
  }

  def makeDatabase[F[_]: Async](config: JdbcDatabaseConfig[?]): F[Database[F]] = {
    val cc = config.controls
    val n  = config.source.maxConnections.getOrElse(cc.maxConnections)
    makeDatabaseWithSource[F](config.source, cc.copy(maxConnections = n))
  }

  /** Construct a [[Database]] from a [[JdbcDataSource]] and open the keepalive connection if
    * configured.  This is the primitive used by both [[makeDatabase]] overloads.  The caller is
    * responsible for calling [[JdbcDatabaseDef.close]] when done. */
  private def makeDatabaseWithSource[F[_]: Async](
    source: JdbcDataSource,
    controls: slick.ControlsConfig
  ): F[Database[F]] = {
    Async[F].flatMap(Controls.create[F](controls)) { c =>
      val ag = Async[F]
      val db = new JdbcDatabaseDef[F](source, c)(ag) {}
      val keepAlive: F[Unit] = source match {
        case ds: DataSourceJdbcDataSource if ds.keepAliveConnection =>
          Async[F].blocking(ds.openKeepAlive())
        case _ =>
          Async[F].unit
      }
      Async[F].as(keepAlive, db)
    }
  }

  /** A JDBC database connected to a given DataSource.
    *
    * `F[_]` is the effect type (e.g. `cats.effect.IO`).
    * Construct via `DatabaseConfig.forConfig[IO](...)` etc. — this returns a `Resource[IO, Database[IO]]`
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
