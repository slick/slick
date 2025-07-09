package slick.tracing

import com.typesafe.config.{Config, ConfigFactory}
import scala.jdk.CollectionConverters.*
import scala.util.Try

/**
 * Configuration for Slick distributed tracing features.
 * 
 * This configuration supports:
 * - Feature toggles for different tracing aspects
 * - SQL comment injection for cloud database integrations
 * - Query parameter sanitization options
 * - Performance impact controls
 */
case class TracingConfig(
  enabled: Boolean = false,
  queryExecution: QueryExecutionConfig = QueryExecutionConfig(),
  queryCompilation: QueryCompilationConfig = QueryCompilationConfig(),
  sqlComments: SqlCommentsConfig = SqlCommentsConfig(),
  sanitization: SanitizationConfig = SanitizationConfig(),
  performance: PerformanceConfig = PerformanceConfig(),
  cloudIntegration: CloudIntegrationConfig = CloudIntegrationConfig()
)

/**
 * Configuration for query execution tracing.
 */
case class QueryExecutionConfig(
  enabled: Boolean = true,
  includeParameters: Boolean = false,
  includeResultSetMetadata: Boolean = true,
  connectionPoolMetrics: Boolean = true,
  streamingMetrics: Boolean = true
)

/**
 * Configuration for query compilation tracing.
 */
case class QueryCompilationConfig(
  enabled: Boolean = true,
  includePhaseDetails: Boolean = false,
  includeAstDetails: Boolean = false,
  performanceMetrics: Boolean = true
)

/**
 * Configuration for SQL comment injection for cloud database integrations.
 */
case class SqlCommentsConfig(
  enabled: Boolean = true,
  includeTraceContext: Boolean = true,
  includeApplicationTags: Boolean = true,
  customTags: Map[String, String] = Map.empty,
  format: SqlCommentFormat = SqlCommentFormat.Standard
)

/**
 * Supported SQL comment formats for different cloud providers.
 */
sealed trait SqlCommentFormat
object SqlCommentFormat {
  case object Standard extends SqlCommentFormat
  case object GoogleCloudSQL extends SqlCommentFormat
  case object AWSAurora extends SqlCommentFormat
  case object AzureSQL extends SqlCommentFormat
}

/**
 * Configuration for SQL statement and parameter sanitization.
 */
case class SanitizationConfig(
  enabled: Boolean = true,
  sanitizeParameters: Boolean = true,
  sanitizeStatements: Boolean = true,
  includeQuerySummary: Boolean = true,
  maxStatementLength: Int = 1000
)

/**
 * Configuration for performance impact controls.
 */
case class PerformanceConfig(
  samplingRate: Double = 1.0,
  maxSpansPerTrace: Int = 100,
  asyncProcessing: Boolean = true,
  bufferSize: Int = 1000
)

/**
 * Configuration for cloud database integrations.
 */
case class CloudIntegrationConfig(
  googleCloudSQL: GoogleCloudSQLConfig = GoogleCloudSQLConfig(),
  awsAurora: AWSAuroraConfig = AWSAuroraConfig(),
  azureSQL: AzureSQLConfig = AzureSQLConfig()
)

case class GoogleCloudSQLConfig(
  enabled: Boolean = false,
  projectId: Option[String] = None,
  instanceId: Option[String] = None,
  queryInsights: Boolean = true
)

case class AWSAuroraConfig(
  enabled: Boolean = false,
  clusterIdentifier: Option[String] = None,
  performanceInsights: Boolean = true
)

case class AzureSQLConfig(
  enabled: Boolean = false,
  serverName: Option[String] = None,
  queryStore: Boolean = true
)

/**
 * Factory for creating TracingConfig instances from Typesafe Config.
 */
object TracingConfig {
  
  /**
   * Creates a TracingConfig from the application configuration.
   * 
   * @param config The Typesafe Config instance
   * @param path The configuration path (default: "slick.tracing")
   * @return A TracingConfig instance
   */
  def fromConfig(config: Config = ConfigFactory.load(), path: String = "slick.tracing"): TracingConfig = {
    if (!config.hasPath(path)) {
      return TracingConfig() // Return default config if path doesn't exist
    }
    
    val tracingConfig = config.getConfig(path)
    
    TracingConfig(
      enabled = tracingConfig.getBoolean("enabled"),
      queryExecution = parseQueryExecutionConfig(tracingConfig),
      queryCompilation = parseQueryCompilationConfig(tracingConfig),
      sqlComments = parseSqlCommentsConfig(tracingConfig),
      sanitization = parseSanitizationConfig(tracingConfig),
      performance = parsePerformanceConfig(tracingConfig),
      cloudIntegration = parseCloudIntegrationConfig(tracingConfig)
    )
  }
  
  private def parseQueryExecutionConfig(config: Config): QueryExecutionConfig = {
    val path = "query-execution"
    if (!config.hasPath(path)) return QueryExecutionConfig()
    
    val execConfig = config.getConfig(path)
    QueryExecutionConfig(
      enabled = getBoolean(execConfig, "enabled", true),
      includeParameters = getBoolean(execConfig, "include-parameters", false),
      includeResultSetMetadata = getBoolean(execConfig, "include-resultset-metadata", true),
      connectionPoolMetrics = getBoolean(execConfig, "connection-pool-metrics", true),
      streamingMetrics = getBoolean(execConfig, "streaming-metrics", true)
    )
  }
  
  private def parseQueryCompilationConfig(config: Config): QueryCompilationConfig = {
    val path = "query-compilation"
    if (!config.hasPath(path)) return QueryCompilationConfig()
    
    val compConfig = config.getConfig(path)
    QueryCompilationConfig(
      enabled = getBoolean(compConfig, "enabled", true),
      includePhaseDetails = getBoolean(compConfig, "include-phase-details", false),
      includeAstDetails = getBoolean(compConfig, "include-ast-details", false),
      performanceMetrics = getBoolean(compConfig, "performance-metrics", true)
    )
  }
  
  private def parseSqlCommentsConfig(config: Config): SqlCommentsConfig = {
    val path = "sql-comments"
    if (!config.hasPath(path)) return SqlCommentsConfig()
    
    val commentsConfig = config.getConfig(path)
    SqlCommentsConfig(
      enabled = getBoolean(commentsConfig, "enabled", true),
      includeTraceContext = getBoolean(commentsConfig, "include-trace-context", true),
      includeApplicationTags = getBoolean(commentsConfig, "include-application-tags", true),
      customTags = parseCustomTags(commentsConfig),
      format = parseSqlCommentFormat(commentsConfig)
    )
  }
  
  private def parseSanitizationConfig(config: Config): SanitizationConfig = {
    val path = "sanitization"
    if (!config.hasPath(path)) return SanitizationConfig()
    
    val sanitConfig = config.getConfig(path)
    SanitizationConfig(
      enabled = getBoolean(sanitConfig, "enabled", true),
      sanitizeParameters = getBoolean(sanitConfig, "sanitize-parameters", true),
      sanitizeStatements = getBoolean(sanitConfig, "sanitize-statements", true),
      includeQuerySummary = getBoolean(sanitConfig, "include-query-summary", true),
      maxStatementLength = getInt(sanitConfig, "max-statement-length", 1000)
    )
  }
  
  private def parsePerformanceConfig(config: Config): PerformanceConfig = {
    val path = "performance"
    if (!config.hasPath(path)) return PerformanceConfig()
    
    val perfConfig = config.getConfig(path)
    PerformanceConfig(
      samplingRate = getDouble(perfConfig, "sampling-rate", 1.0),
      maxSpansPerTrace = getInt(perfConfig, "max-spans-per-trace", 100),
      asyncProcessing = getBoolean(perfConfig, "async-processing", true),
      bufferSize = getInt(perfConfig, "buffer-size", 1000)
    )
  }
  
  private def parseCloudIntegrationConfig(config: Config): CloudIntegrationConfig = {
    val path = "cloud-integration"
    if (!config.hasPath(path)) return CloudIntegrationConfig()
    
    val cloudConfig = config.getConfig(path)
    CloudIntegrationConfig(
      googleCloudSQL = parseGoogleCloudSQLConfig(cloudConfig),
      awsAurora = parseAWSAuroraConfig(cloudConfig),
      azureSQL = parseAzureSQLConfig(cloudConfig)
    )
  }
  
  private def parseGoogleCloudSQLConfig(config: Config): GoogleCloudSQLConfig = {
    val path = "google-cloud-sql"
    if (!config.hasPath(path)) return GoogleCloudSQLConfig()
    
    val gcsConfig = config.getConfig(path)
    GoogleCloudSQLConfig(
      enabled = getBoolean(gcsConfig, "enabled", false),
      projectId = getOptionalString(gcsConfig, "project-id"),
      instanceId = getOptionalString(gcsConfig, "instance-id"),
      queryInsights = getBoolean(gcsConfig, "query-insights", true)
    )
  }
  
  private def parseAWSAuroraConfig(config: Config): AWSAuroraConfig = {
    val path = "aws-aurora"
    if (!config.hasPath(path)) return AWSAuroraConfig()
    
    val auroraConfig = config.getConfig(path)
    AWSAuroraConfig(
      enabled = getBoolean(auroraConfig, "enabled", false),
      clusterIdentifier = getOptionalString(auroraConfig, "cluster-identifier"),
      performanceInsights = getBoolean(auroraConfig, "performance-insights", true)
    )
  }
  
  private def parseAzureSQLConfig(config: Config): AzureSQLConfig = {
    val path = "azure-sql"
    if (!config.hasPath(path)) return AzureSQLConfig()
    
    val azureConfig = config.getConfig(path)
    AzureSQLConfig(
      enabled = getBoolean(azureConfig, "enabled", false),
      serverName = getOptionalString(azureConfig, "server-name"),
      queryStore = getBoolean(azureConfig, "query-store", true)
    )
  }
  
  private def parseCustomTags(config: Config): Map[String, String] = {
    val path = "custom-tags"
    if (!config.hasPath(path)) return Map.empty
    
    Try {
      config.getConfig(path).entrySet().asScala.map { entry =>
        entry.getKey -> entry.getValue.unwrapped().toString
      }.toMap
    }.getOrElse(Map.empty)
  }
  
  private def parseSqlCommentFormat(config: Config): SqlCommentFormat = {
    val formatString = getString(config, "format", "standard").toLowerCase
    formatString match {
      case "google-cloud-sql" => SqlCommentFormat.GoogleCloudSQL
      case "aws-aurora" => SqlCommentFormat.AWSAurora
      case "azure-sql" => SqlCommentFormat.AzureSQL
      case _ => SqlCommentFormat.Standard
    }
  }
  
  // Helper methods for safe config access
  private def getBoolean(config: Config, path: String, default: Boolean): Boolean = {
    Try(config.getBoolean(path)).getOrElse(default)
  }
  
  private def getInt(config: Config, path: String, default: Int): Int = {
    Try(config.getInt(path)).getOrElse(default)
  }
  
  private def getDouble(config: Config, path: String, default: Double): Double = {
    Try(config.getDouble(path)).getOrElse(default)
  }
  
  private def getString(config: Config, path: String, default: String): String = {
    Try(config.getString(path)).getOrElse(default)
  }
  
  private def getOptionalString(config: Config, path: String): Option[String] = {
    Try(config.getString(path)).toOption.filter(_.nonEmpty)
  }
  
  /**
   * Default configuration for reference.
   */
  val default: TracingConfig = TracingConfig()
}