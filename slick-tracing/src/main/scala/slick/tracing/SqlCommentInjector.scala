package slick.tracing

import io.opentelemetry.api.trace.{Span, SpanContext}
import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/**
 * Injects SQL comments for cloud database query tagging and distributed tracing.
 * 
 * This component supports various cloud database providers:
 * - Google Cloud SQL Query Insights
 * - AWS Aurora Performance Insights
 * - Azure SQL Query Store
 * 
 * The injected comments follow the SQLCommenter format and include:
 * - OpenTelemetry trace context (traceparent, tracestate)
 * - Application metadata (framework, controller, action, etc.)
 * - Custom application tags
 */
class SqlCommentInjector(config: TracingConfig) {
  
  /**
   * Injects tracing and application metadata into SQL statements as comments.
   * 
   * @param sql The original SQL statement
   * @param tracingContext The current tracing context
   * @param applicationTags Additional application-specific tags
   * @return The SQL statement with injected comments
   */
  def injectComments(
    sql: String,
    tracingContext: TracingContext,
    applicationTags: Map[String, String] = Map.empty
  ): String = {
    if (!config.sqlComments.enabled || !tracingContext.isTracingEnabled) {
      return sql
    }
    
    val comments = buildComments(tracingContext, applicationTags)
    if (comments.isEmpty) {
      return sql
    }
    
    val commentString = formatComments(comments)
    injectCommentIntoSql(sql, commentString)
  }
  
  /**
   * Builds the comment content based on configuration and context.
   */
  private def buildComments(
    tracingContext: TracingContext,
    applicationTags: Map[String, String]
  ): Map[String, String] = {
    val comments = mutable.Map.empty[String, String]
    
    // Add OpenTelemetry trace context
    if (config.sqlComments.includeTraceContext) {
      addTraceContext(comments, tracingContext)
    }
    
    // Add application tags
    if (config.sqlComments.includeApplicationTags) {
      addApplicationTags(comments, applicationTags)
    }
    
    // Add custom configured tags
    comments ++= config.sqlComments.customTags
    
    // Add cloud provider specific tags
    addCloudProviderTags(comments)
    
    comments.toMap
  }
  
  /**
   * Adds OpenTelemetry trace context to comments.
   */
  private def addTraceContext(comments: mutable.Map[String, String], tracingContext: TracingContext): Unit = {
    val currentSpan = Span.current()
    if (currentSpan.getSpanContext.isValid) {
      val spanContext = currentSpan.getSpanContext
      
      // Add traceparent (W3C Trace Context format)
      val traceparent = formatTraceparent(spanContext)
      comments += "traceparent" -> traceparent
      
      // Add tracestate if present
      val tracestate = spanContext.getTraceState.asMap()
      if (!tracestate.isEmpty) {
        val tracestateString = tracestate.asScala.map { case (key, value) =>
          s"$key=$value"
        }.mkString(",")
        comments += "tracestate" -> tracestateString
      }
    }
  }
  
  /**
   * Formats the traceparent header according to W3C Trace Context specification.
   */
  private def formatTraceparent(spanContext: SpanContext): String = {
    val version = "00"
    val traceId = spanContext.getTraceId
    val spanId = spanContext.getSpanId
    val flags = if (spanContext.isSampled) "01" else "00"
    
    s"$version-$traceId-$spanId-$flags"
  }
  
  /**
   * Adds application-specific tags to comments.
   */
  private def addApplicationTags(comments: mutable.Map[String, String], applicationTags: Map[String, String]): Unit = {
    // Standard application tags
    val standardTags = Map(
      "db_driver" -> "slick",
      "framework" -> "slick"
    )
    
    comments ++= standardTags
    comments ++= applicationTags
  }
  
  /**
   * Adds cloud provider specific tags based on configuration.
   */
  private def addCloudProviderTags(comments: mutable.Map[String, String]): Unit = {
    // Google Cloud SQL specific tags
    if (config.cloudIntegration.googleCloudSQL.enabled) {
      config.cloudIntegration.googleCloudSQL.projectId.foreach { projectId =>
        comments += "gcp_project_id" -> projectId
      }
      config.cloudIntegration.googleCloudSQL.instanceId.foreach { instanceId =>
        comments += "gcp_instance_id" -> instanceId
      }
    }
    
    // AWS Aurora specific tags
    if (config.cloudIntegration.awsAurora.enabled) {
      config.cloudIntegration.awsAurora.clusterIdentifier.foreach { clusterId =>
        comments += "aws_cluster_id" -> clusterId
      }
    }
    
    // Azure SQL specific tags
    if (config.cloudIntegration.azureSQL.enabled) {
      config.cloudIntegration.azureSQL.serverName.foreach { serverName =>
        comments += "azure_server" -> serverName
      }
    }
  }
  
  /**
   * Formats comments according to the configured format.
   */
  private def formatComments(comments: Map[String, String]): String = {
    if (comments.isEmpty) return ""
    
    val encodedComments = comments.map { case (key, value) =>
      val encodedKey = urlEncode(key)
      val encodedValue = urlEncode(value)
      s"$encodedKey='$encodedValue'"
    }
    
    config.sqlComments.format match {
      case SqlCommentFormat.GoogleCloudSQL => formatGoogleCloudSQLComments(encodedComments)
      case SqlCommentFormat.AWSAurora => formatAWSAuroraComments(encodedComments)
      case SqlCommentFormat.AzureSQL => formatAzureSQLComments(encodedComments)
      case SqlCommentFormat.Standard => formatStandardComments(encodedComments)
    }
  }
  
  /**
   * Formats comments for Google Cloud SQL Query Insights.
   */
  private def formatGoogleCloudSQLComments(encodedComments: Iterable[String]): String = {
    s"/*${encodedComments.mkString(", ")}*/"
  }
  
  /**
   * Formats comments for AWS Aurora Performance Insights.
   */
  private def formatAWSAuroraComments(encodedComments: Iterable[String]): String = {
    s"/*${encodedComments.mkString(", ")}*/"
  }
  
  /**
   * Formats comments for Azure SQL Query Store.
   */
  private def formatAzureSQLComments(encodedComments: Iterable[String]): String = {
    s"/*${encodedComments.mkString(", ")}*/"
  }
  
  /**
   * Formats comments using standard SQL comment format.
   */
  private def formatStandardComments(encodedComments: Iterable[String]): String = {
    s"/*${encodedComments.mkString(", ")}*/"
  }
  
  /**
   * Injects the formatted comment into the SQL statement.
   */
  private def injectCommentIntoSql(sql: String, comment: String): String = {
    val trimmedSql = sql.trim
    
    // Find the position to insert the comment
    val insertPosition = findCommentInsertPosition(trimmedSql)
    
    if (insertPosition == 0) {
      // Insert at the beginning
      s"$comment $trimmedSql"
    } else {
      // Insert after the initial keyword with proper spacing
      val (prefix, suffix) = trimmedSql.splitAt(insertPosition)
      s"$prefix$comment $suffix"
    }
  }
  
  /**
   * Finds the optimal position to insert the comment in the SQL statement.
   * This ensures the comment is placed after the initial SQL keyword but before the main query.
   */
  private def findCommentInsertPosition(sql: String): Int = {
    val upperSql = sql.toUpperCase.trim
    
    // Common SQL keywords where we want to insert the comment after
    val sqlKeywords = Seq("SELECT", "INSERT", "UPDATE", "DELETE", "WITH", "CREATE", "ALTER", "DROP")
    
    sqlKeywords.find(upperSql.startsWith) match {
      case Some(keyword) =>
        // Return position right after the keyword
        keyword.length
      case None =>
        0 // Insert at the beginning if no recognized keyword is found
    }
  }
  
  /**
   * URL-encodes a string for use in SQL comments.
   */
  private def urlEncode(value: String): String = {
    URLEncoder.encode(value, StandardCharsets.UTF_8.toString)
  }
}

/**
 * Companion object with factory methods.
 */
object SqlCommentInjector {
  
  /**
   * Creates a SqlCommentInjector with the default configuration.
   */
  def default: SqlCommentInjector = new SqlCommentInjector(TracingConfig.default)
  
  /**
   * Creates a SqlCommentInjector with the specified configuration.
   */
  def apply(config: TracingConfig): SqlCommentInjector = new SqlCommentInjector(config)
}