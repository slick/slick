package slick.tracing

import io.opentelemetry.api.OpenTelemetry
import io.opentelemetry.api.trace.{Span, SpanBuilder, SpanKind, StatusCode, Tracer}
import io.opentelemetry.context.Context
import io.opentelemetry.semconv.SchemaUrls
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
 * Context for distributed tracing in Slick operations.
 * 
 * Provides OpenTelemetry-based tracing capabilities with proper context propagation
 * and semantic conventions for database operations.
 */
trait TracingContext {
  def openTelemetry: OpenTelemetry
  def isTracingEnabled: Boolean
  def currentContext: Context
  def tracer: Tracer
  
  /**
   * Creates a new database operation span with proper semantic conventions.
   * 
   * @param operationName The name of the database operation
   * @param dbSystem The database system (e.g., "postgresql", "mysql")
   * @param dbName The database name
   * @param dbStatement The SQL statement (will be sanitized)
   * @param dbUser The database user
   * @param serverAddress The server address
   * @param serverPort The server port
   * @return A SpanBuilder configured with database semantic conventions
   */
  def createDatabaseSpan(
    operationName: String,
    dbSystem: Option[String] = None,
    dbName: Option[String] = None,
    dbStatement: Option[String] = None,
    dbUser: Option[String] = None,
    serverAddress: Option[String] = None,
    serverPort: Option[Int] = None
  ): SpanBuilder = {
    val spanBuilder = tracer.spanBuilder(operationName)
      .setSpanKind(SpanKind.CLIENT)
      .setAttribute("db.operation.name", operationName)
      
    dbSystem.foreach(sys => spanBuilder.setAttribute("db.system", sys))
    dbName.foreach(name => spanBuilder.setAttribute("db.name", name))
    dbStatement.foreach(stmt => spanBuilder.setAttribute("db.statement", sanitizeStatement(stmt)))
    dbUser.foreach(user => spanBuilder.setAttribute("db.user", user))
    serverAddress.foreach(addr => spanBuilder.setAttribute("server.address", addr))
    serverPort.foreach(port => spanBuilder.setAttribute("server.port", port.toLong))
    
    spanBuilder
  }
  
  /**
   * Creates a new query compilation span.
   * 
   * @param phaseName The name of the compilation phase
   * @param queryType The type of query being compiled
   * @return A SpanBuilder configured for query compilation
   */
  def createCompilationSpan(phaseName: String, queryType: Option[String] = None): SpanBuilder = {
    val spanBuilder = tracer.spanBuilder(s"slick.compile.$phaseName")
      .setSpanKind(SpanKind.INTERNAL)
      .setAttribute("slick.compilation.phase", phaseName)
      
    queryType.foreach(qt => spanBuilder.setAttribute("slick.query.type", qt))
    
    spanBuilder
  }
  
  /**
   * Executes a block of code within a span, handling exceptions and span lifecycle.
   * 
   * @param spanBuilder The span builder to use
   * @param block The code to execute within the span
   * @param ec The execution context
   * @tparam T The return type
   * @return A Future containing the result
   */
  def withSpan[T](spanBuilder: SpanBuilder)(block: Span => Future[T])(implicit ec: ExecutionContext): Future[T] = {
    if (!isTracingEnabled) return block(Span.getInvalid)
    
    val span = spanBuilder.startSpan()
    val scope = span.makeCurrent()
    
    try {
      val result = block(span)
      result.transform(
        { value =>
          span.setStatus(StatusCode.OK)
          span.end()
          scope.close()
          value
        },
        { throwable =>
          span.setStatus(StatusCode.ERROR, throwable.getMessage)
          span.recordException(throwable)
          span.end()
          scope.close()
          throwable
        }
      )
    } catch {
      case throwable: Throwable =>
        span.setStatus(StatusCode.ERROR, throwable.getMessage)
        span.recordException(throwable)
        span.end()
        scope.close()
        Future.failed(throwable)
    }
  }
  
  /**
   * Executes a synchronous block of code within a span.
   * 
   * @param spanBuilder The span builder to use
   * @param block The code to execute within the span
   * @tparam T The return type
   * @return The result of the block
   */
  def withSpanSync[T](spanBuilder: SpanBuilder)(block: Span => T): T = {
    if (!isTracingEnabled) return block(Span.getInvalid)
    
    val span = spanBuilder.startSpan()
    val scope = span.makeCurrent()
    
    try {
      val result = block(span)
      span.setStatus(StatusCode.OK)
      result
    } catch {
      case throwable: Throwable =>
        span.setStatus(StatusCode.ERROR, throwable.getMessage)
        span.recordException(throwable)
        throw throwable
    } finally {
      span.end()
      scope.close()
    }
  }
  
  /**
   * Sanitizes SQL statements for tracing by replacing literals with placeholders.
   * This follows OpenTelemetry semantic conventions for security.
   * 
   * @param statement The SQL statement to sanitize
   * @return The sanitized statement
   */
  private def sanitizeStatement(statement: String): String = {
    // Basic sanitization - replace string literals and numeric literals with placeholders
    // This is a simplified implementation; production code should use a proper SQL parser
    statement
      .replaceAll("'[^']*'", "?") // Replace string literals
      .replaceAll("\\b\\d+\\b", "?") // Replace numeric literals
      .replaceAll("\\s+", " ") // Normalize whitespace
      .trim
  }
  
  /**
   * Adds application-specific tags to a span for cloud database integration.
   * 
   * @param span The span to add tags to
   * @param tags The tags to add
   */
  def addApplicationTags(span: Span, tags: Map[String, String]): Unit = {
    tags.foreach { case (key, value) =>
      span.setAttribute(s"app.$key", value)
    }
  }
  
  /**
   * Creates a child context with the current span.
   * 
   * @return A new context with the current span
   */
  def childContext(): Context = {
    Context.current()
  }
}

/**
 * Default implementation of TracingContext.
 */
class DefaultTracingContext(
  val openTelemetry: OpenTelemetry,
  val isTracingEnabled: Boolean = true
) extends TracingContext {
  
  override val currentContext: Context = Context.current()
  
  override val tracer: Tracer = openTelemetry.getTracer(
    "slick",
    BuildInfo.version,
    SchemaUrls.V1_25_0
  )
}

/**
 * No-op implementation of TracingContext for when tracing is disabled.
 */
object NoOpTracingContext extends TracingContext {
  override val openTelemetry: OpenTelemetry = OpenTelemetry.noop()
  override val isTracingEnabled: Boolean = false
  override val currentContext: Context = Context.current()
  override val tracer: Tracer = openTelemetry.getTracer("slick-noop")
}

/**
 * Companion object with utility methods and implicits.
 */
object TracingContext {
  
  /**
   * Implicit conversions and context propagation utilities.
   */
  object Implicits {
    
    /**
     * Implicit TracingContext that can be used throughout the application.
     * By default, uses a no-op implementation. Applications should override this
     * with their own TracingContext implementation.
     */
    implicit val defaultTracingContext: TracingContext = NoOpTracingContext
    
    /**
     * Provides syntax for adding tracing to any operation.
     */
    implicit class TracingOps[T](private val value: T) extends AnyVal {
      def withTracing(
        operationName: String,
        tags: Map[String, String] = Map.empty
      )(implicit ctx: TracingContext, ec: ExecutionContext): Future[T] = {
        val spanBuilder = ctx.tracer.spanBuilder(operationName)
        tags.foreach { case (key, value) =>
          spanBuilder.setAttribute(key, value)
        }
        
        ctx.withSpan(spanBuilder) { span =>
          ctx.addApplicationTags(span, tags)
          Future.successful(value)
        }
      }
    }
  }
}