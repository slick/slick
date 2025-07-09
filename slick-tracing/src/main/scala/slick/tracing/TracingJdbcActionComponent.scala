package slick.tracing

import slick.jdbc.{JdbcActionComponent, JdbcBackend}
import slick.dbio.{DBIOAction, Effect, NoStream, SynchronousDatabaseAction}
import slick.util.{DumpInfo, SQLBuilder}
import io.opentelemetry.api.trace.{Span, StatusCode}
import io.opentelemetry.semconv.incubating.DbIncubatingAttributes
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
 * Tracing-enabled version of JdbcActionComponent that instruments database operations
 * with OpenTelemetry spans and SQL comment injection.
 */
trait TracingJdbcActionComponent extends JdbcActionComponent { self =>
  
  /**
   * The tracing configuration for this component.
   */
  def tracingConfig: TracingConfig
  
  /**
   * The tracing context for this component.
   */
  def tracingContext: TracingContext
  
  /**
   * The SQL comment injector for this component.
   */
  lazy val sqlCommentInjector: SqlCommentInjector = SqlCommentInjector(tracingConfig)
  
  /**
   * Enhanced SimpleJdbcProfileAction with tracing capabilities.
   */
  abstract class TracingSimpleJdbcProfileAction[+R](
    _name: String,
    _statements: Vector[String],
    applicationTags: Map[String, String] = Map.empty
  ) extends SimpleJdbcProfileAction[R](_name, _statements) {
    
    /**
     * Executes the action with tracing instrumentation.
     */
    override def run(ctx: JdbcBackend#JdbcActionContext): R = {
      if (!tracingConfig.enabled || !tracingConfig.queryExecution.enabled) {
        return super.run(ctx)
      }
      
      val dbSystem = detectDatabaseSystem(ctx)
      val connection = ctx.session.conn
      val dbName = getConnectionProperty(connection, "database", "catalog")
      val dbUser = getConnectionProperty(connection, "user")
      val serverAddress = getConnectionProperty(connection, "serverName", "host")
      val serverPort = getConnectionProperty(connection, "port").map(_.toInt)
      
      val spanBuilder = tracingContext.createDatabaseSpan(
        operationName = _name,
        dbSystem = dbSystem,
        dbName = dbName,
        dbStatement = statements.headOption,
        dbUser = dbUser,
        serverAddress = serverAddress,
        serverPort = serverPort
      )
      
      // Add application tags
      applicationTags.foreach { case (key, value) =>
        spanBuilder.setAttribute(s"app.$key", value)
      }
      
      // Add performance attributes
      spanBuilder.setAttribute("db.statement.count", statements.size.toLong)
      
      tracingContext.withSpanSync(spanBuilder) { span =>
        // Inject SQL comments if configured
        val instrumentedStatements = if (tracingConfig.sqlComments.enabled) {
          statements.map(sql => sqlCommentInjector.injectComments(sql, tracingContext, applicationTags))
        } else {
          statements
        }
        
        val startTime = System.nanoTime()
        
        try {
          // Execute with instrumented statements
          val result = runWithInstrumentedStatements(ctx, instrumentedStatements)
          
          // Record success metrics
          val endTime = System.nanoTime()
          val durationMs = (endTime - startTime) / 1_000_000
          span.setAttribute("db.operation.duration_ms", durationMs)
          
          result
        } catch {
          case ex: Exception =>
            // Record error details
            span.recordException(ex)
            span.setAttribute("db.operation.error", ex.getClass.getSimpleName)
            span.setAttribute("db.operation.error_message", ex.getMessage)
            throw ex
        }
      }
    }
    
    /**
     * Executes the action with instrumented SQL statements.
     * Subclasses can override this to provide custom execution logic.
     */
    protected def runWithInstrumentedStatements(
      ctx: JdbcBackend#JdbcActionContext,
      instrumentedStatements: Vector[String]
    ): R = {
      // Create a new action instance with instrumented statements
      val instrumentedAction = overrideStatements(instrumentedStatements)
      instrumentedAction.run(ctx)
    }
    
    /**
     * Adds tracing-specific extension methods to this action.
     */
    def withTracing(tags: Map[String, String]): TracingSimpleJdbcProfileAction[R] = {
      new TracingSimpleJdbcProfileAction[R](_name, statements, applicationTags ++ tags) {
        override def run(ctx: JdbcBackend#JdbcActionContext, sql: Vector[String]): R = 
          self.run(ctx, sql)
      }
    }
    
    /**
     * Adds a specific application tag to this action.
     */
    def withTag(key: String, value: String): TracingSimpleJdbcProfileAction[R] = {
      withTracing(Map(key -> value))
    }
    
    /**
     * Adds an operation name tag to this action.
     */
    def withOperationName(operationName: String): TracingSimpleJdbcProfileAction[R] = {
      withTag("operation", operationName)
    }
  }
  
  /**
   * Detects the database system from the connection context.
   */
  private def detectDatabaseSystem(ctx: JdbcBackend#JdbcActionContext): Option[String] = {
    try {
      val connection = ctx.session.conn
      val url = connection.getMetaData.getURL
      
      url.toLowerCase match {
        case u if u.contains("postgresql") => Some("postgresql")
        case u if u.contains("mysql") => Some("mysql")
        case u if u.contains("oracle") => Some("oracle")
        case u if u.contains("sqlserver") => Some("mssql")
        case u if u.contains("db2") => Some("db2")
        case u if u.contains("h2") => Some("h2")
        case u if u.contains("hsqldb") => Some("hsqldb")
        case u if u.contains("derby") => Some("derby")
        case u if u.contains("sqlite") => Some("sqlite")
        case _ => None
      }
    } catch {
      case _: Exception => None
    }
  }
  
  /**
   * Safely gets a connection property.
   */
  private def getConnectionProperty(connection: java.sql.Connection, property: String*): Option[String] = {
    try {
      val metadata = connection.getMetaData
      property.map { prop =>
        prop.toLowerCase match {
          case "database" => Option(metadata.getDatabaseName)
          case "catalog" => Option(connection.getCatalog)
          case "user" => Option(metadata.getUserName)
          case "servername" | "host" => Option(metadata.getURL).flatMap(extractFromUrl(_, "//([^:/]+)"))
          case "port" => Option(metadata.getURL).flatMap(extractFromUrl(_, ":(\\d+)"))
          case _ => None
        }
      }.find(_.isDefined).flatten
    } catch {
      case _: Exception => None
    }
  }
  
  /**
   * Extracts information from JDBC URL using regex.
   */
  private def extractFromUrl(url: String, pattern: String): Option[String] = {
    try {
      val regex = pattern.r
      regex.findFirstMatchIn(url).map(_.group(1))
    } catch {
      case _: Exception => None
    }
  }
  
  /**
   * Enhanced transaction actions with tracing.
   */
  object TracingTransactionActions {
    
    object StartTransaction extends SynchronousDatabaseAction[Unit, NoStream, JdbcBackend#JdbcActionContext, JdbcBackend#JdbcStreamingActionContext, Effect] {
      def run(ctx: JdbcBackend#JdbcActionContext): Unit = {
        if (tracingConfig.enabled && tracingConfig.queryExecution.enabled) {
          val spanBuilder = tracingContext.createDatabaseSpan("transaction.start")
          tracingContext.withSpanSync(spanBuilder) { span =>
            ctx.pin
            ctx.session.startInTransaction
            span.setAttribute("db.transaction.started", true)
          }
        } else {
          ctx.pin
          ctx.session.startInTransaction
        }
      }
      def getDumpInfo = DumpInfo(name = "StartTransaction")
    }
    
    object Commit extends SynchronousDatabaseAction[Unit, NoStream, JdbcBackend#JdbcActionContext, JdbcBackend#JdbcStreamingActionContext, Effect] {
      def run(ctx: JdbcBackend#JdbcActionContext): Unit = {
        if (tracingConfig.enabled && tracingConfig.queryExecution.enabled) {
          val spanBuilder = tracingContext.createDatabaseSpan("transaction.commit")
          tracingContext.withSpanSync(spanBuilder) { span =>
            try {
              ctx.session.endInTransaction(ctx.session.conn.commit())
              span.setAttribute("db.transaction.committed", true)
            } finally {
              ctx.unpin
            }
          }
        } else {
          try ctx.session.endInTransaction(ctx.session.conn.commit()) finally ctx.unpin
        }
      }
      def getDumpInfo = DumpInfo(name = "Commit")
    }
    
    object Rollback extends SynchronousDatabaseAction[Unit, NoStream, JdbcBackend#JdbcActionContext, JdbcBackend#JdbcStreamingActionContext, Effect] {
      def run(ctx: JdbcBackend#JdbcActionContext): Unit = {
        if (tracingConfig.enabled && tracingConfig.queryExecution.enabled) {
          val spanBuilder = tracingContext.createDatabaseSpan("transaction.rollback")
          tracingContext.withSpanSync(spanBuilder) { span =>
            try {
              ctx.session.endInTransaction(ctx.session.conn.rollback())
              span.setAttribute("db.transaction.rolledback", true)
            } finally {
              ctx.unpin
            }
          }
        } else {
          try ctx.session.endInTransaction(ctx.session.conn.rollback()) finally ctx.unpin
        }
      }
      def getDumpInfo = DumpInfo(name = "Rollback")
    }
  }
  
  /**
   * Override transaction actions to use tracing versions.
   */
  override protected object StartTransaction extends SynchronousDatabaseAction[Unit, NoStream, JdbcBackend#JdbcActionContext, JdbcBackend#JdbcStreamingActionContext, Effect] {
    def run(ctx: JdbcBackend#JdbcActionContext): Unit = TracingTransactionActions.StartTransaction.run(ctx)
    def getDumpInfo = TracingTransactionActions.StartTransaction.getDumpInfo
  }
  
  override protected object Commit extends SynchronousDatabaseAction[Unit, NoStream, JdbcBackend#JdbcActionContext, JdbcBackend#JdbcStreamingActionContext, Effect] {
    def run(ctx: JdbcBackend#JdbcActionContext): Unit = TracingTransactionActions.Commit.run(ctx)
    def getDumpInfo = TracingTransactionActions.Commit.getDumpInfo
  }
  
  override protected object Rollback extends SynchronousDatabaseAction[Unit, NoStream, JdbcBackend#JdbcActionContext, JdbcBackend#JdbcStreamingActionContext, Effect] {
    def run(ctx: JdbcBackend#JdbcActionContext): Unit = TracingTransactionActions.Rollback.run(ctx)
    def getDumpInfo = TracingTransactionActions.Rollback.getDumpInfo
  }
}