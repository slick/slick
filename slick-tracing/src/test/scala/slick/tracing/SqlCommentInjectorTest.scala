package slick.tracing

import io.opentelemetry.api.OpenTelemetry
import io.opentelemetry.sdk.OpenTelemetrySdk
import io.opentelemetry.sdk.testing.exporter.InMemorySpanExporter
import io.opentelemetry.sdk.trace.SdkTracerProvider
import io.opentelemetry.sdk.trace.`export`.SimpleSpanProcessor
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SqlCommentInjectorTest extends AnyWordSpec with Matchers {

  val spanExporter = InMemorySpanExporter.create()
  val tracerProvider = SdkTracerProvider.builder()
    .addSpanProcessor(SimpleSpanProcessor.create(spanExporter))
    .build()
  val openTelemetry: OpenTelemetry = OpenTelemetrySdk.builder()
    .setTracerProvider(tracerProvider)
    .build()

  val defaultConfig = TracingConfig.default
  val tracingContext = new DefaultTracingContext(openTelemetry, isTracingEnabled = true)

  "SqlCommentInjector" should {

    "inject trace context into SQL statements" in {
      val config = defaultConfig.copy(
        sqlComments = defaultConfig.sqlComments.copy(
          enabled = true,
          includeTraceContext = true
        )
      )
      val injector = new SqlCommentInjector(config)

      // Create a mock span context
      val tracer = openTelemetry.getTracer("test")
      val span = tracer.spanBuilder("test-span").startSpan()
      val scope = span.makeCurrent()

      try {
        val sql = "SELECT * FROM users WHERE id = 1"
        val result = injector.injectComments(sql, tracingContext)

        result should include("SELECT")
        result should include("traceparent=")
        result should include("users")
        result should not equal sql // Should be modified
      } finally {
        span.end()
        scope.close()
      }
    }

    "inject application tags into SQL statements" in {
      val config = defaultConfig.copy(
        sqlComments = defaultConfig.sqlComments.copy(
          enabled = true,
          includeApplicationTags = true
        )
      )
      val injector = new SqlCommentInjector(config)

      val sql = "INSERT INTO products (name) VALUES ('test')"
      val applicationTags = Map("service" -> "product-service", "version" -> "1.0")
      val result = injector.injectComments(sql, tracingContext, applicationTags)

      result should include("INSERT")
      result should include("db_driver='slick'")
      result should include("framework='slick'")
      result should include("service='product-service'")
      result should include("version='1.0'")
    }

    "inject custom tags from configuration" in {
      val config = defaultConfig.copy(
        sqlComments = defaultConfig.sqlComments.copy(
          enabled = true,
          customTags = Map("environment" -> "test", "region" -> "us-west")
        )
      )
      val injector = new SqlCommentInjector(config)

      val sql = "UPDATE users SET name = 'John' WHERE id = 1"
      val result = injector.injectComments(sql, tracingContext)

      result should include("UPDATE")
      result should include("environment='test'")
      result should include("region='us-west'")
    }

    "inject Google Cloud SQL specific tags" in {
      val config = defaultConfig.copy(
        sqlComments = defaultConfig.sqlComments.copy(enabled = true),
        cloudIntegration = defaultConfig.cloudIntegration.copy(
          googleCloudSQL = GoogleCloudSQLConfig(
            enabled = true,
            projectId = Some("my-project"),
            instanceId = Some("my-instance")
          )
        )
      )
      val injector = new SqlCommentInjector(config)

      val sql = "SELECT COUNT(*) FROM orders"
      val result = injector.injectComments(sql, tracingContext)

      result should include("SELECT")
      result should include("gcp_project_id='my-project'")
      result should include("gcp_instance_id='my-instance'")
    }

    "inject AWS Aurora specific tags" in {
      val config = defaultConfig.copy(
        sqlComments = defaultConfig.sqlComments.copy(enabled = true),
        cloudIntegration = defaultConfig.cloudIntegration.copy(
          awsAurora = AWSAuroraConfig(
            enabled = true,
            clusterIdentifier = Some("my-cluster")
          )
        )
      )
      val injector = new SqlCommentInjector(config)

      val sql = "DELETE FROM temp_data WHERE created_at < NOW() - INTERVAL 1 DAY"
      val result = injector.injectComments(sql, tracingContext)

      result should include("DELETE")
      result should include("aws_cluster_id='my-cluster'")
    }

    "inject Azure SQL specific tags" in {
      val config = defaultConfig.copy(
        sqlComments = defaultConfig.sqlComments.copy(enabled = true),
        cloudIntegration = defaultConfig.cloudIntegration.copy(
          azureSQL = AzureSQLConfig(
            enabled = true,
            serverName = Some("my-server")
          )
        )
      )
      val injector = new SqlCommentInjector(config)

      val sql = "CREATE INDEX idx_user_email ON users(email)"
      val result = injector.injectComments(sql, tracingContext)

      result should include("CREATE")
      result should include("azure_server='my-server'")
    }

    "use different comment formats" in {
      val baseConfig = defaultConfig.copy(
        sqlComments = defaultConfig.sqlComments.copy(
          enabled = true,
          includeApplicationTags = true
        )
      )

      val injector = new SqlCommentInjector(baseConfig)
      val sql = "SELECT 1"

      // All formats should produce valid SQL comments
      val formats = Seq(
        SqlCommentFormat.Standard,
        SqlCommentFormat.GoogleCloudSQL,
        SqlCommentFormat.AWSAurora,
        SqlCommentFormat.AzureSQL
      )

      formats.foreach { format =>
        val config = baseConfig.copy(
          sqlComments = baseConfig.sqlComments.copy(format = format)
        )
        val injector = new SqlCommentInjector(config)
        val result = injector.injectComments(sql, tracingContext)

        result should include("/*")
        result should include("*/")
        result should include("1")
      }
    }

    "handle various SQL statement types" in {
      val config = defaultConfig.copy(
        sqlComments = defaultConfig.sqlComments.copy(
          enabled = true,
          includeApplicationTags = true
        )
      )
      val injector = new SqlCommentInjector(config)

      val testCases = Seq(
        "SELECT * FROM users",
        "INSERT INTO users (name) VALUES ('test')",
        "UPDATE users SET name = 'updated' WHERE id = 1",
        "DELETE FROM users WHERE id = 1",
        "WITH cte AS (SELECT id FROM users) SELECT * FROM cte",
        "CREATE TABLE test (id INT)",
        "ALTER TABLE test ADD COLUMN name VARCHAR(100)",
        "DROP TABLE test"
      )

      testCases.foreach { sql =>
        val result = injector.injectComments(sql, tracingContext)
        result should include("/*")
        result should include("*/")
        result should include(sql.takeWhile(!_.isWhitespace)) // Should contain the SQL keyword
      }
    }

    "return original SQL when comments are disabled" in {
      val config = defaultConfig.copy(
        sqlComments = defaultConfig.sqlComments.copy(enabled = false)
      )
      val injector = new SqlCommentInjector(config)

      val sql = "SELECT * FROM users"
      val result = injector.injectComments(sql, tracingContext)

      result shouldBe sql
    }

    "return original SQL when tracing is disabled" in {
      val disabledContext = new DefaultTracingContext(openTelemetry, isTracingEnabled = false)
      val injector = new SqlCommentInjector(defaultConfig)

      val sql = "SELECT * FROM users"
      val result = injector.injectComments(sql, disabledContext)

      result shouldBe sql
    }

    "URL encode special characters in comments" in {
      val config = defaultConfig.copy(
        sqlComments = defaultConfig.sqlComments.copy(
          enabled = true,
          includeApplicationTags = true
        )
      )
      val injector = new SqlCommentInjector(config)

      val sql = "SELECT * FROM users"
      val applicationTags = Map("service" -> "test service", "path" -> "/api/v1/users?filter=active")
      val result = injector.injectComments(sql, tracingContext, applicationTags)

      result should include("service='test+service'")
      result should include("path='%2Fapi%2Fv1%2Fusers%3Ffilter%3Dactive'")
    }

    "handle empty application tags" in {
      val config = defaultConfig.copy(
        sqlComments = defaultConfig.sqlComments.copy(
          enabled = true,
          includeApplicationTags = true
        )
      )
      val injector = new SqlCommentInjector(config)

      val sql = "SELECT * FROM users"
      val result = injector.injectComments(sql, tracingContext, Map.empty)

      result should include("SELECT")
      result should include("db_driver='slick'")
      result should include("framework='slick'")
    }

    "find correct comment insertion position" in {
      val config = defaultConfig.copy(
        sqlComments = defaultConfig.sqlComments.copy(
          enabled = true,
          includeApplicationTags = true
        )
      )
      val injector = new SqlCommentInjector(config)

      val testCases = Map(
        "SELECT * FROM users" -> "SELECT/*",
        "INSERT INTO users VALUES (1)" -> "INSERT/*",
        "UPDATE users SET name = 'test'" -> "UPDATE/*",
        "DELETE FROM users" -> "DELETE/*",
        "WITH cte AS (SELECT 1) SELECT * FROM cte" -> "WITH/*"
      )

      testCases.foreach { case (sql, expectedPrefix) =>
        val result = injector.injectComments(sql, tracingContext)
        result should startWith(expectedPrefix)
      }
    }
  }

  "SqlCommentInjector companion object" should {

    "create default injector" in {
      val injector = SqlCommentInjector.default
      injector should not be null
    }

    "create injector with custom config" in {
      val customConfig = defaultConfig.copy(
        sqlComments = defaultConfig.sqlComments.copy(enabled = false)
      )
      val injector = SqlCommentInjector(customConfig)
      injector should not be null
    }
  }
}