package slick.tracing

import io.opentelemetry.api.OpenTelemetry
import io.opentelemetry.api.trace.{Span, StatusCode, Tracer}
import slick.dbio.{DBIOAction, Effect, NoStream}
import scala.concurrent.{ExecutionContext, Future}

/**
 * Simple tracing support that can be mixed into any JDBC profile.
 * This provides basic tracing capabilities without complex API changes.
 */
trait SimpleTracingSupport {
  
  /**
   * The OpenTelemetry instance to use for tracing.
   */
  def openTelemetry: OpenTelemetry
  
  /**
   * Whether tracing is enabled.
   */
  def tracingEnabled: Boolean = true
  
  /**
   * The tracer instance.
   */
  lazy val tracer: Tracer = openTelemetry.getTracer("slick")
  
  /**
   * Creates a span for a database operation.
   */
  def createSpan(operationName: String): Span = {
    if (!tracingEnabled) {
      return Span.getInvalid
    }
    
    tracer.spanBuilder(operationName)
      .setAttribute("db.system", "unknown")
      .setAttribute("component", "slick")
      .startSpan()
  }
  
  /**
   * Wraps a DBIOAction with tracing.
   */
  def withTracing[R](
    operationName: String,
    action: DBIOAction[R, NoStream, Effect.All]
  )(implicit ec: ExecutionContext): DBIOAction[R, NoStream, Effect.All] = {
    if (!tracingEnabled) {
      return action
    }
    
    val span = createSpan(operationName)
    val scope = span.makeCurrent()
    
    action.asTry.map { tryResult =>
      try {
        tryResult match {
          case scala.util.Success(result) =>
            span.setStatus(StatusCode.OK)
            result
          case scala.util.Failure(ex) =>
            span.setStatus(StatusCode.ERROR, ex.getMessage)
            span.recordException(ex)
            throw ex
        }
      } finally {
        span.end()
        scope.close()
      }
    }
  }
  
  /**
   * Wraps a Future with tracing.
   */
  def withTracingFuture[T](
    operationName: String,
    future: Future[T]
  )(implicit ec: ExecutionContext): Future[T] = {
    if (!tracingEnabled) {
      return future
    }
    
    val span = createSpan(operationName)
    val scope = span.makeCurrent()
    
    future.transform(
      { result =>
        span.setStatus(StatusCode.OK)
        result
      },
      { throwable =>
        span.setStatus(StatusCode.ERROR, throwable.getMessage)
        span.recordException(throwable)
        throwable
      }
    ).andThen { case _ =>
      span.end()
      scope.close()
    }
  }
  
  /**
   * Synchronous span execution.
   */
  def withSpanSync[T](operationName: String)(block: Span => T): T = {
    if (!tracingEnabled) {
      return block(Span.getInvalid)
    }
    
    val span = createSpan(operationName)
    val scope = span.makeCurrent()
    
    try {
      val result = block(span)
      span.setStatus(StatusCode.OK)
      result
    } catch {
      case ex: Exception =>
        span.setStatus(StatusCode.ERROR, ex.getMessage)
        span.recordException(ex)
        throw ex
    } finally {
      span.end()
      scope.close()
    }
  }
}

/**
 * A simple traced profile that can be used as a drop-in replacement.
 */
class TracedH2Profile(val openTelemetry: OpenTelemetry) extends slick.jdbc.H2Profile with SimpleTracingSupport

/**
 * A simple traced profile for PostgreSQL.
 */
class TracedPostgresProfile(val openTelemetry: OpenTelemetry) extends slick.jdbc.PostgresProfile with SimpleTracingSupport

/**
 * A simple traced profile for MySQL.
 */
class TracedMySQLProfile(val openTelemetry: OpenTelemetry) extends slick.jdbc.MySQLProfile with SimpleTracingSupport

/**
 * Factory for creating traced profiles.
 */
object TracedProfiles {
  def h2(openTelemetry: OpenTelemetry): TracedH2Profile = new TracedH2Profile(openTelemetry)
  def postgres(openTelemetry: OpenTelemetry): TracedPostgresProfile = new TracedPostgresProfile(openTelemetry)
  def mysql(openTelemetry: OpenTelemetry): TracedMySQLProfile = new TracedMySQLProfile(openTelemetry)
}