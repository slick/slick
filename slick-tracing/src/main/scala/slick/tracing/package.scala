package slick

/**
 * Slick Distributed Tracing Integration
 * 
 * This package provides OpenTelemetry-based distributed tracing capabilities for Slick,
 * including:
 * 
 * - Database operation spans with OpenTelemetry semantic conventions
 * - SQL comment injection for cloud database integrations (Google Cloud SQL, AWS Aurora, Azure SQL)
 * - Query compilation phase instrumentation
 * - Connection pool monitoring
 * - Streaming query tracing
 * 
 * == Quick Start ==
 * 
 * {{{
 * import slick.tracing._
 * import io.opentelemetry.api.OpenTelemetry
 * 
 * // Create tracing profile
 * val openTelemetry: OpenTelemetry = // ... initialize OpenTelemetry
 * val profile = TracingJdbcProfile.withOpenTelemetry(openTelemetry)
 * 
 * // Use the profile
 * import profile.api._
 * 
 * class Users(tag: Tag) extends Table[(Long, String)](tag, "users") {
 *   def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
 *   def name = column[String]("name")
 *   def * = (id, name)
 * }
 * 
 * val users = TableQuery[Users]
 * 
 * // Queries are automatically traced
 * val query = users.filter(_.name === "Alice").result
 * 
 * // Add custom tracing information
 * val tracedQuery = users.filter(_.name === "Alice")
 *   .withTracing(Map("operation" -> "user-lookup", "component" -> "user-service"))
 *   .result
 * }}}
 * 
 * == Configuration ==
 * 
 * Configure tracing via application.conf:
 * 
 * {{{
 * slick.tracing {
 *   enabled = true
 *   
 *   query-execution {
 *     enabled = true
 *     include-parameters = false
 *     include-resultset-metadata = true
 *   }
 *   
 *   query-compilation {
 *     enabled = true
 *     include-phase-details = false
 *     performance-metrics = true
 *   }
 *   
 *   sql-comments {
 *     enabled = true
 *     include-trace-context = true
 *     include-application-tags = true
 *     format = "standard"
 *   }
 *   
 *   cloud-integration {
 *     google-cloud-sql {
 *       enabled = false
 *       project-id = "my-project"
 *       query-insights = true
 *     }
 *   }
 * }
 * }}}
 * 
 * == Cloud Database Integration ==
 * 
 * The tracing module automatically injects SQL comments for cloud database query insights:
 * 
 * - '''Google Cloud SQL''': Supports Query Insights with application tags and trace context
 * - '''AWS Aurora''': Supports Performance Insights with query tagging
 * - '''Azure SQL''': Supports Query Store with application metadata
 * 
 * == OpenTelemetry Semantic Conventions ==
 * 
 * The tracing module follows OpenTelemetry semantic conventions for database operations:
 * 
 * - `db.system`: Database system (postgresql, mysql, etc.)
 * - `db.name`: Database name
 * - `db.statement`: SQL statement (sanitized)
 * - `db.user`: Database user
 * - `server.address`: Database server address
 * - `server.port`: Database server port
 * 
 * == Performance Considerations ==
 * 
 * The tracing module is designed to have minimal performance impact:
 * 
 * - Configurable sampling rates
 * - Asynchronous span processing
 * - Efficient SQL comment injection
 * - Optional detailed instrumentation
 * 
 * @since 3.7.0
 */
package object tracing {
  
  // Common type aliases for convenience
  // Note: These reference the actual classes defined in this package
  
  // Convenience imports for common configurations
  object Implicits {
    /**
     * Default tracing configuration loaded from application.conf.
     */
    implicit lazy val defaultTracingConfig: TracingConfig = TracingConfig.fromConfig()
    
    /**
     * Default no-op tracing context.
     * Applications should override this with their own TracingContext.
     */
    implicit lazy val defaultTracingContext: TracingContext = NoOpTracingContext
  }
  
  /**
   * Convenience methods for creating tracing profiles.
   */
  object profiles {
    /**
     * Creates a PostgreSQL profile with tracing.
     */
    def postgres(
      config: TracingConfig = TracingConfig.fromConfig(),
      context: TracingContext = NoOpTracingContext
    ): TracingProfiles.PostgresTracingProfile = {
      new TracingProfiles.PostgresTracingProfile(config, context)
    }
    
    /**
     * Creates a MySQL profile with tracing.
     */
    def mysql(
      config: TracingConfig = TracingConfig.fromConfig(),
      context: TracingContext = NoOpTracingContext
    ): TracingProfiles.MySQLTracingProfile = {
      new TracingProfiles.MySQLTracingProfile(config, context)
    }
    
    /**
     * Creates an H2 profile with tracing.
     */
    def h2(
      config: TracingConfig = TracingConfig.fromConfig(),
      context: TracingContext = NoOpTracingContext
    ): TracingProfiles.H2TracingProfile = {
      new TracingProfiles.H2TracingProfile(config, context)
    }
    
    /**
     * Creates an Oracle profile with tracing.
     */
    def oracle(
      config: TracingConfig = TracingConfig.fromConfig(),
      context: TracingContext = NoOpTracingContext
    ): TracingProfiles.OracleTracingProfile = {
      new TracingProfiles.OracleTracingProfile(config, context)
    }
    
    /**
     * Creates a SQL Server profile with tracing.
     */
    def sqlserver(
      config: TracingConfig = TracingConfig.fromConfig(),
      context: TracingContext = NoOpTracingContext
    ): TracingProfiles.SQLServerTracingProfile = {
      new TracingProfiles.SQLServerTracingProfile(config, context)
    }
  }
  
  /**
   * Utility functions for tracing.
   */
  object util {
    /**
     * Checks if tracing is enabled in the current configuration.
     */
    def isTracingEnabled(implicit config: TracingConfig): Boolean = config.enabled
    
    /**
     * Creates a tracing context from OpenTelemetry.
     */
    def createTracingContext(
      openTelemetry: io.opentelemetry.api.OpenTelemetry,
      enabled: Boolean = true
    ): TracingContext = {
      new DefaultTracingContext(openTelemetry, enabled)
    }
    
    /**
     * Creates a disabled tracing context.
     */
    def noOpTracingContext: TracingContext = NoOpTracingContext
  }
}