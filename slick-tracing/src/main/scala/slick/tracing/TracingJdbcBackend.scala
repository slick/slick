package slick.tracing

import slick.jdbc.{JdbcBackend, JdbcDataSource}
import slick.util.AsyncExecutor
import slick.basic.DatabasePublisher
import slick.dbio.{DBIOAction, Effect, NoStream, StreamingDBIO}
import io.opentelemetry.api.trace.Span
import io.opentelemetry.api.common.Attributes
import io.opentelemetry.semconv.incubating.DbIncubatingAttributes
import org.reactivestreams.Subscriber
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}
import java.sql.Connection
import javax.sql.DataSource

/**
 * Tracing-enhanced JDBC backend that instruments connection pool operations
 * and database streaming operations with OpenTelemetry spans.
 */
trait TracingJdbcBackend extends JdbcBackend {
  
  def tracingConfig: TracingConfig
  def tracingContext: TracingContext
  
  /**
   * Tracing-enhanced database definition that monitors connection pool operations.
   */
  class TracingJdbcDatabaseDef(
    source: JdbcDataSource,
    executor: AsyncExecutor,
    val tracingConfig: TracingConfig,
    val tracingContext: TracingContext
  ) extends JdbcDatabaseDef(source, executor) {
    
    /**
     * Creates a session with connection pool tracing.
     */
    override def createSession(): Session = {
      if (tracingConfig.enabled && tracingConfig.queryExecution.connectionPoolMetrics) {
        val spanBuilder = tracingContext.createDatabaseSpan("connection.acquire")
          .setAttribute("db.connection_pool.name", source.getClass.getSimpleName)
        
        tracingContext.withSpanSync(spanBuilder) { span =>
          val startTime = System.nanoTime()
          
          try {
            val session = new TracingBaseSession(this, tracingConfig, tracingContext)
            
            val endTime = System.nanoTime()
            val duration = (endTime - startTime) / 1_000_000
            span.setAttribute("db.connection.acquire_duration_ms", duration)
            span.setAttribute("db.connection.status", "acquired")
            
            session
          } catch {
            case ex: Exception =>
              span.recordException(ex)
              span.setAttribute("db.connection.status", "failed")
              span.setAttribute("db.connection.error", ex.getClass.getSimpleName)
              throw ex
          }
        }
      } else {
        new TracingBaseSession(this, tracingConfig, tracingContext)
      }
    }
    
    /**
     * Streams data with tracing instrumentation.
     */
    override def stream[T](a: StreamingDBIO[?, T], bufferNext: Boolean): DatabasePublisher[T] = {
      if (tracingConfig.enabled && tracingConfig.queryExecution.streamingMetrics) {
        val spanBuilder = tracingContext.createDatabaseSpan("stream.create")
          .setAttribute("db.stream.buffer_next", bufferNext)
        
        tracingContext.withSpanSync(spanBuilder) { span =>
          val publisher = createPublisher(a, s => new TracingJdbcStreamingActionContext(s, false, this, bufferNext, tracingConfig, tracingContext))
          
          span.setAttribute("db.stream.created", true)
          span.setAttribute("db.stream.action_type", a.getClass.getSimpleName)
          
          publisher
        }
      } else {
        createPublisher(a, s => new TracingJdbcStreamingActionContext(s, false, this, bufferNext, tracingConfig, tracingContext))
      }
    }
    
    /**
     * Creates a database action context with tracing.
     */
    override protected[this] def createDatabaseActionContext[T](_useSameThread: Boolean): Context =
      new TracingJdbcActionContext(_useSameThread, tracingConfig, tracingContext)
    
    /**
     * Creates a streaming database action context with tracing.
     */
    override protected[this] def createStreamingDatabaseActionContext[T](s: Subscriber[? >: T], useSameThread: Boolean): StreamingContext =
      new TracingJdbcStreamingActionContext(s, useSameThread, this, true, tracingConfig, tracingContext)
  }
  
  /**
   * Tracing-enhanced session that monitors connection lifecycle.
   */
  class TracingBaseSession(
    database: JdbcDatabaseDef,
    val tracingConfig: TracingConfig,
    val tracingContext: TracingContext
  ) extends BaseSession(database) {
    
    private var connectionAcquiredTime: Long = 0
    
    /**
     * Lazy connection acquisition with tracing.
     */
    override lazy val conn: Connection = {
      if (tracingConfig.enabled && tracingConfig.queryExecution.connectionPoolMetrics) {
        val spanBuilder = tracingContext.createDatabaseSpan("connection.get")
        
        tracingContext.withSpanSync(spanBuilder) { span =>
          connectionAcquiredTime = System.nanoTime()
          
          try {
            val connection = database.source.createConnection()
            
            // Add connection metadata
            val metadata = connection.getMetaData
            span.setAttribute("db.connection.id", connection.hashCode().toString)
            span.setAttribute("db.connection.url", metadata.getURL)
            span.setAttribute("db.connection.driver", metadata.getDriverName)
            span.setAttribute("db.connection.driver_version", metadata.getDriverVersion)
            span.setAttribute("db.connection.auto_commit", connection.getAutoCommit)
            span.setAttribute("db.connection.isolation_level", connection.getTransactionIsolation.toLong)
            
            connection
          } catch {
            case ex: Exception =>
              span.recordException(ex)
              span.setAttribute("db.connection.error", ex.getClass.getSimpleName)
              throw ex
          }
        }
      } else {
        database.source.createConnection()
      }
    }
    
    /**
     * Connection close with tracing.
     */
    override def close(): Unit = {
      if (tracingConfig.enabled && tracingConfig.queryExecution.connectionPoolMetrics && connectionAcquiredTime > 0) {
        val spanBuilder = tracingContext.createDatabaseSpan("connection.close")
        
        tracingContext.withSpanSync(spanBuilder) { span =>
          val closeTime = System.nanoTime()
          val connectionDuration = (closeTime - connectionAcquiredTime) / 1_000_000
          
          span.setAttribute("db.connection.duration_ms", connectionDuration)
          span.setAttribute("db.connection.closed", true)
          
          try {
            super.close()
          } catch {
            case ex: Exception =>
              span.recordException(ex)
              span.setAttribute("db.connection.close_error", ex.getClass.getSimpleName)
              throw ex
          }
        }
      } else {
        super.close()
      }
    }
  }
  
  /**
   * Tracing-enhanced action context.
   */
  class TracingJdbcActionContext(
    val useSameThread: Boolean,
    val tracingConfig: TracingConfig,
    val tracingContext: TracingContext
  ) extends JdbcActionContext {
    override val useSameThread: Boolean = useSameThread
  }
  
  /**
   * Tracing-enhanced streaming action context.
   */
  class TracingJdbcStreamingActionContext[T](
    subscriber: Subscriber[? >: T],
    useSameThread: Boolean,
    database: JdbcDatabaseDef,
    bufferNext: Boolean,
    val tracingConfig: TracingConfig,
    val tracingContext: TracingContext
  ) extends JdbcStreamingActionContext(subscriber, useSameThread, database, bufferNext) {
    
    private var streamStartTime: Long = 0
    private var elementCount: Long = 0
    
    /**
     * Enhanced streaming with metrics collection.
     */
    override def emit(v: T): Unit = {
      if (tracingConfig.enabled && tracingConfig.queryExecution.streamingMetrics) {
        if (streamStartTime == 0) {
          streamStartTime = System.nanoTime()
        }
        
        elementCount += 1
        
        // Add streaming metrics to current span if available
        val currentSpan = Span.current()
        if (currentSpan.isRecording) {
          currentSpan.setAttribute("db.stream.element_count", elementCount)
          
          if (elementCount % 1000 == 0) { // Log every 1000 elements to avoid spam
            val currentTime = System.nanoTime()
            val streamDuration = (currentTime - streamStartTime) / 1_000_000
            currentSpan.setAttribute("db.stream.duration_ms", streamDuration)
            currentSpan.setAttribute("db.stream.throughput_per_second", (elementCount * 1000) / streamDuration)
          }
        }
      }
      
      super.emit(v)
    }
    
    /**
     * Stream completion with final metrics.
     */
    override def finished(success: Boolean): Unit = {
      if (tracingConfig.enabled && tracingConfig.queryExecution.streamingMetrics && streamStartTime > 0) {
        val currentSpan = Span.current()
        if (currentSpan.isRecording) {
          val endTime = System.nanoTime()
          val totalDuration = (endTime - streamStartTime) / 1_000_000
          
          currentSpan.setAttribute("db.stream.completed", success)
          currentSpan.setAttribute("db.stream.total_elements", elementCount)
          currentSpan.setAttribute("db.stream.total_duration_ms", totalDuration)
          
          if (totalDuration > 0) {
            currentSpan.setAttribute("db.stream.avg_throughput_per_second", (elementCount * 1000) / totalDuration)
          }
        }
      }
      
      super.finished(success)
    }
  }
  
  /**
   * Factory for creating tracing database instances.
   */
  trait TracingDatabaseFactory extends DatabaseFactoryDef {
    
    def tracingConfig: TracingConfig
    def tracingContext: TracingContext
    
    /**
     * Creates a database with tracing enabled.
     */
    override def forSource(source: JdbcDataSource, executor: AsyncExecutor = AsyncExecutor.default()) =
      new TracingJdbcDatabaseDef(source, executor, tracingConfig, tracingContext)
  }
}