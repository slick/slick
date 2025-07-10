package slick.tracing

import com.typesafe.config.ConfigFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TracingConfigTest extends AnyWordSpec with Matchers {

  "TracingConfig" should {

    "load default configuration" in {
      val config = TracingConfig()

      config.enabled shouldBe false
      config.queryExecution.enabled shouldBe true
      config.queryExecution.includeParameters shouldBe false
      config.queryExecution.includeResultSetMetadata shouldBe true

      config.queryCompilation.enabled shouldBe true
      config.queryCompilation.includePhaseDetails shouldBe false
      config.queryCompilation.performanceMetrics shouldBe true

      config.sqlComments.enabled shouldBe true
      config.sqlComments.includeTraceContext shouldBe true
      config.sqlComments.includeApplicationTags shouldBe true
      config.sqlComments.format shouldBe SqlCommentFormat.Standard
      config.sqlComments.customTags shouldBe Map.empty

      config.cloudIntegration.googleCloudSQL.enabled shouldBe false
      config.cloudIntegration.awsAurora.enabled shouldBe false
      config.cloudIntegration.azureSQL.enabled shouldBe false
    }

    "load configuration from Typesafe Config" in {
      val configStr = """
        slick.tracing {
          enabled = false
          
          query-execution {
            enabled = false
            include-parameters = true
            include-resultset-metadata = false
          }
          
          query-compilation {
            enabled = false
            include-phase-details = true
            performance-metrics = false
          }
          
          sql-comments {
            enabled = false
            include-trace-context = false
            include-application-tags = false
            format = "google-cloud-sql"
            custom-tags {
              environment = "test"
              region = "us-west"
            }
          }
          
          cloud-integration {
            google-cloud-sql {
              enabled = true
              project-id = "test-project"
              instance-id = "test-instance"
              query-insights = false
            }
            
            aws-aurora {
              enabled = true
              cluster-identifier = "test-cluster"
              performance-insights = false
            }
            
            azure-sql {
              enabled = true
              server-name = "test-server"
              query-store = false
            }
          }
        }
      """

      val typesafeConfig = ConfigFactory.parseString(configStr)
      val config = TracingConfig.fromConfig(typesafeConfig)

      config.enabled shouldBe false
      config.queryExecution.enabled shouldBe false
      config.queryExecution.includeParameters shouldBe true
      config.queryExecution.includeResultSetMetadata shouldBe false

      config.queryCompilation.enabled shouldBe false
      config.queryCompilation.includePhaseDetails shouldBe true
      config.queryCompilation.performanceMetrics shouldBe false

      config.sqlComments.enabled shouldBe false
      config.sqlComments.includeTraceContext shouldBe false
      config.sqlComments.includeApplicationTags shouldBe false
      config.sqlComments.format shouldBe SqlCommentFormat.GoogleCloudSQL
      config.sqlComments.customTags shouldBe Map("environment" -> "test", "region" -> "us-west")

      config.cloudIntegration.googleCloudSQL.enabled shouldBe true
      config.cloudIntegration.googleCloudSQL.projectId shouldBe Some("test-project")
      config.cloudIntegration.googleCloudSQL.instanceId shouldBe Some("test-instance")
      config.cloudIntegration.googleCloudSQL.queryInsights shouldBe false

      config.cloudIntegration.awsAurora.enabled shouldBe true
      config.cloudIntegration.awsAurora.clusterIdentifier shouldBe Some("test-cluster")
      config.cloudIntegration.awsAurora.performanceInsights shouldBe false

      config.cloudIntegration.azureSQL.enabled shouldBe true
      config.cloudIntegration.azureSQL.serverName shouldBe Some("test-server")
      config.cloudIntegration.azureSQL.queryStore shouldBe false
    }

    "load configuration from current system config" in {
      val config = TracingConfig.fromConfig()
      // Should not throw exception and return valid config
      config should not be null
      config.enabled shouldBe a[Boolean]
    }

    "handle partial configuration" in {
      val configStr = """
        slick.tracing {
          enabled = false
          sql-comments.format = "aws-aurora"
          cloud-integration.google-cloud-sql.project-id = "partial-project"
        }
      """

      val typesafeConfig = ConfigFactory.parseString(configStr)
      val config = TracingConfig.fromConfig(typesafeConfig)

      // Explicitly set values should be honored
      config.enabled shouldBe false
      config.sqlComments.format shouldBe SqlCommentFormat.AWSAurora
      config.cloudIntegration.googleCloudSQL.projectId shouldBe Some("partial-project")

      // Unset values should use defaults
      config.queryExecution.enabled shouldBe true
      config.sqlComments.enabled shouldBe true
      config.cloudIntegration.googleCloudSQL.enabled shouldBe false
    }

    "handle missing configuration gracefully" in {
      val emptyConfig = ConfigFactory.empty()
      val config = TracingConfig.fromConfig(emptyConfig)

      // Should fall back to defaults
      config shouldBe TracingConfig()
    }

    "handle invalid SQL comment format" in {
      val configStr = """
        slick.tracing {
          enabled = true
          sql-comments {
            format = "invalid-format"
          }
        }
      """

      val typesafeConfig = ConfigFactory.parseString(configStr)

      // Should throw exception for invalid format
      intercept[IllegalArgumentException] {
        TracingConfig.fromConfig(typesafeConfig)
      }
    }

    "copy configuration with modifications" in {
      val original = TracingConfig()

      val modified = original.copy(
        enabled = false,
        queryExecution = original.queryExecution.copy(includeParameters = true),
        sqlComments = original.sqlComments.copy(format = SqlCommentFormat.GoogleCloudSQL)
      )

      modified.enabled shouldBe false
      modified.queryExecution.includeParameters shouldBe true
      modified.sqlComments.format shouldBe SqlCommentFormat.GoogleCloudSQL

      // Other values should remain unchanged
      modified.queryCompilation shouldBe original.queryCompilation
    }
  }

  "QueryExecutionConfig" should {

    "create instances with correct defaults" in {
      val queryExecution = QueryExecutionConfig()

      queryExecution.enabled shouldBe true
      queryExecution.includeParameters shouldBe false
      queryExecution.includeResultSetMetadata shouldBe true
    }

    "support copying with modifications" in {
      val original = QueryExecutionConfig()
      val modified = original.copy(includeParameters = true)

      modified.includeParameters shouldBe true
      modified.enabled shouldBe original.enabled
      modified.includeResultSetMetadata shouldBe original.includeResultSetMetadata
    }
  }

  "SqlCommentsConfig" should {

    "create instances with correct defaults" in {
      val sqlComments = SqlCommentsConfig()

      sqlComments.enabled shouldBe true
      sqlComments.includeTraceContext shouldBe true
      sqlComments.includeApplicationTags shouldBe true
      sqlComments.format shouldBe SqlCommentFormat.Standard
      sqlComments.customTags shouldBe Map.empty
    }
  }

  "CloudIntegrationConfig" should {

    "create instances with correct defaults" in {
      val cloudIntegration = CloudIntegrationConfig()

      cloudIntegration.googleCloudSQL.enabled shouldBe false
      cloudIntegration.googleCloudSQL.projectId shouldBe None
      cloudIntegration.googleCloudSQL.instanceId shouldBe None
      cloudIntegration.googleCloudSQL.queryInsights shouldBe true

      cloudIntegration.awsAurora.enabled shouldBe false
      cloudIntegration.awsAurora.clusterIdentifier shouldBe None
      cloudIntegration.awsAurora.performanceInsights shouldBe true

      cloudIntegration.azureSQL.enabled shouldBe false
      cloudIntegration.azureSQL.serverName shouldBe None
      cloudIntegration.azureSQL.queryStore shouldBe true
    }
  }

  "SqlCommentFormat" should {

    "have correct string representations" in {
      SqlCommentFormat.Standard.toString shouldBe "Standard"
      SqlCommentFormat.GoogleCloudSQL.toString shouldBe "GoogleCloudSQL"
      SqlCommentFormat.AWSAurora.toString shouldBe "AWSAurora"
      SqlCommentFormat.AzureSQL.toString shouldBe "AzureSQL"
    }
  }
}