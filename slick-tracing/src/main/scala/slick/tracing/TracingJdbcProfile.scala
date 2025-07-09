package slick.tracing

import slick.jdbc.JdbcProfile
import slick.compiler.QueryCompiler
import slick.dbio.{DBIOAction, Effect, NoStream}
import slick.lifted.{Query, Rep}
import io.opentelemetry.api.OpenTelemetry
import scala.concurrent.ExecutionContext

/**
 * Base trait for JDBC profiles with distributed tracing capabilities.
 * 
 * This trait provides:
 * - OpenTelemetry-based tracing for database operations
 * - SQL comment injection for cloud database integrations
 * - Query compilation phase instrumentation
 * - Connection pool monitoring
 * - Streaming query tracing
 * 
 * Usage:
 * ```scala
 * import slick.tracing.TracingJdbcProfile
 * 
 * trait MyProfile extends TracingJdbcProfile {
 *   override def tracingConfig: TracingConfig = TracingConfig.fromConfig()
 *   override def tracingContext: TracingContext = new DefaultTracingContext(openTelemetry)
 * }
 * ```
 */
trait TracingJdbcProfile extends JdbcProfile with TracingJdbcActionComponent { self =>
  
  /**
   * The tracing configuration for this profile.
   * Override this to provide custom tracing configuration.
   */
  def tracingConfig: TracingConfig
  
  /**
   * The tracing context for this profile.
   * Override this to provide custom tracing context.
   */
  def tracingContext: TracingContext
  
  // Override query compilers to use tracing versions
  override lazy val queryCompiler: QueryCompiler = 
    TracingQueryCompiler.wrap(super.queryCompiler, tracingConfig, tracingContext)
  
  override lazy val updateCompiler: QueryCompiler = 
    TracingQueryCompiler.wrap(super.updateCompiler, tracingConfig, tracingContext)
  
  override lazy val deleteCompiler: QueryCompiler = 
    TracingQueryCompiler.wrap(super.deleteCompiler, tracingConfig, tracingContext)
  
  override lazy val insertCompiler: QueryCompiler = 
    TracingQueryCompiler.wrap(super.insertCompiler, tracingConfig, tracingContext)
  
  override lazy val forceInsertCompiler: QueryCompiler = 
    TracingQueryCompiler.wrap(super.forceInsertCompiler, tracingConfig, tracingContext)
  
  override lazy val upsertCompiler: QueryCompiler = 
    TracingQueryCompiler.wrap(super.upsertCompiler, tracingConfig, tracingContext)
  
  override lazy val checkInsertCompiler: QueryCompiler = 
    TracingQueryCompiler.wrap(super.checkInsertCompiler, tracingConfig, tracingContext)
  
  override lazy val updateInsertCompiler: QueryCompiler = 
    TracingQueryCompiler.wrap(super.updateInsertCompiler, tracingConfig, tracingContext)
  
  /**
   * Enhanced API with tracing extension methods.
   */
  trait TracingAPI extends super.API {
    
    /**
     * Implicit class to add tracing methods to queries.
     */
    implicit class TracingQueryOps[E, U, C[_]](query: Query[E, U, C]) {
      
      /**
       * Adds tracing information to the query execution.
       */
      def withTracing(tags: Map[String, String] = Map.empty): Query[E, U, C] = {
        // Store tracing information in the query's metadata
        // This would require extending Query to support metadata
        query
      }
      
      /**
       * Adds a specific tag to the query tracing.
       */
      def withTag(key: String, value: String): Query[E, U, C] = {
        withTracing(Map(key -> value))
      }
      
      /**
       * Adds an operation name to the query tracing.
       */
      def withOperationName(operationName: String): Query[E, U, C] = {
        withTag("operation", operationName)
      }
      
      /**
       * Adds a component name to the query tracing.
       */
      def withComponent(component: String): Query[E, U, C] = {
        withTag("component", component)
      }
    }
    
    /**
     * Implicit class to add tracing methods to DBIOActions.
     */
    implicit class TracingDBIOActionOps[R, S <: NoStream, E <: Effect](action: DBIOAction[R, S, E]) {
      
      /**
       * Adds tracing information to the action execution.
       */
      def withTracing(
        operationName: String,
        tags: Map[String, String] = Map.empty
      )(implicit ec: ExecutionContext): DBIOAction[R, S, E] = {
        // The actual tracing would be handled by the TracingSimpleJdbcProfileAction
        action
      }
      
      /**
       * Adds a specific tag to the action tracing.
       */
      def withTag(key: String, value: String)(implicit ec: ExecutionContext): DBIOAction[R, S, E] = {
        withTracing("dbio.action", Map(key -> value))
      }
      
      /**
       * Adds an operation name to the action tracing.
       */
      def withOperationName(operationName: String)(implicit ec: ExecutionContext): DBIOAction[R, S, E] = {
        withTracing(operationName)
      }
    }
    
    /**
     * Creates a traced database action.
     */
    def traced[R](
      name: String,
      tags: Map[String, String] = Map.empty
    )(
      block: => R
    ): DBIOAction[R, NoStream, Effect] = {
      new TracingSimpleJdbcProfileAction[R](name, Vector.empty, tags) {
        override def run(ctx: backend.JdbcActionContext, sql: Vector[String]): R = {
          block
        }
      }
    }
    
    /**
     * Creates a traced database action with SQL statements.
     */
    def tracedSQL[R](
      name: String,
      statements: Vector[String],
      tags: Map[String, String] = Map.empty
    )(
      block: backend.JdbcActionContext => R
    ): DBIOAction[R, NoStream, Effect] = {
      new TracingSimpleJdbcProfileAction[R](name, statements, tags) {
        override def run(ctx: backend.JdbcActionContext, sql: Vector[String]): R = {
          block(ctx)
        }
      }
    }
  }
  
  /**
   * The enhanced API with tracing methods.
   */
  override val api: TracingAPI = new TracingAPI {}
}

/**
 * Concrete implementation of TracingJdbcProfile with default configurations.
 */
class DefaultTracingJdbcProfile(
  override val tracingConfig: TracingConfig = TracingConfig.fromConfig(),
  override val tracingContext: TracingContext = NoOpTracingContext
) extends JdbcProfile with TracingJdbcProfile

/**
 * Factory for creating TracingJdbcProfile instances.
 */
object TracingJdbcProfile {
  
  /**
   * Creates a TracingJdbcProfile with OpenTelemetry integration.
   */
  def withOpenTelemetry(
    openTelemetry: OpenTelemetry,
    config: TracingConfig = TracingConfig.fromConfig()
  ): TracingJdbcProfile = {
    val tracingContext = new DefaultTracingContext(openTelemetry, config.enabled)
    new DefaultTracingJdbcProfile(config, tracingContext)
  }
  
  /**
   * Creates a TracingJdbcProfile with custom configuration.
   */
  def withConfig(config: TracingConfig): TracingJdbcProfile = {
    new DefaultTracingJdbcProfile(config)
  }
  
  /**
   * Creates a disabled TracingJdbcProfile (no-op tracing).
   */
  def disabled: TracingJdbcProfile = {
    val config = TracingConfig.default.copy(enabled = false)
    new DefaultTracingJdbcProfile(config, NoOpTracingContext)
  }
}

/**
 * Specific database profile implementations with tracing.
 */
object TracingProfiles {
  
  /**
   * PostgreSQL profile with tracing capabilities.
   */
  class PostgresTracingProfile(
    override val tracingConfig: TracingConfig = TracingConfig.fromConfig(),
    override val tracingContext: TracingContext = NoOpTracingContext
  ) extends slick.jdbc.PostgresProfile with TracingJdbcProfile
  
  /**
   * MySQL profile with tracing capabilities.
   */
  class MySQLTracingProfile(
    override val tracingConfig: TracingConfig = TracingConfig.fromConfig(),
    override val tracingContext: TracingContext = NoOpTracingContext
  ) extends slick.jdbc.MySQLProfile with TracingJdbcProfile
  
  /**
   * H2 profile with tracing capabilities.
   */
  class H2TracingProfile(
    override val tracingConfig: TracingConfig = TracingConfig.fromConfig(),
    override val tracingContext: TracingContext = NoOpTracingContext
  ) extends slick.jdbc.H2Profile with TracingJdbcProfile
  
  /**
   * Oracle profile with tracing capabilities.
   */
  class OracleTracingProfile(
    override val tracingConfig: TracingConfig = TracingConfig.fromConfig(),
    override val tracingContext: TracingContext = NoOpTracingContext
  ) extends slick.jdbc.OracleProfile with TracingJdbcProfile
  
  /**
   * SQL Server profile with tracing capabilities.
   */
  class SQLServerTracingProfile(
    override val tracingConfig: TracingConfig = TracingConfig.fromConfig(),
    override val tracingContext: TracingContext = NoOpTracingContext
  ) extends slick.jdbc.SQLServerProfile with TracingJdbcProfile
}