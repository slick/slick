package slick.tracing

import io.opentelemetry.api.OpenTelemetry
import io.opentelemetry.api.trace.{Span, SpanKind, StatusCode}
import io.opentelemetry.context.Context
import io.opentelemetry.sdk.OpenTelemetrySdk
import io.opentelemetry.sdk.testing.exporter.InMemorySpanExporter
import io.opentelemetry.sdk.trace.SdkTracerProvider
import io.opentelemetry.sdk.trace.export.SimpleSpanProcessor
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.{ExecutionContext, Future}

class TracingContextTest extends AnyWordSpec with Matchers with ScalaFutures {

  implicit val ec: ExecutionContext = ExecutionContext.global

  // Test setup with in-memory span exporter
  val spanExporter = InMemorySpanExporter.create()
  val tracerProvider = SdkTracerProvider.builder()
    .addSpanProcessor(SimpleSpanProcessor.create(spanExporter))
    .build()
  val openTelemetry: OpenTelemetry = OpenTelemetrySdk.builder()
    .setTracerProvider(tracerProvider)
    .build()

  def clearSpans(): Unit = {
    spanExporter.reset()
  }

  def getSpans = spanExporter.getFinishedSpanItems

  "DefaultTracingContext" should {

    "create database spans with semantic conventions" in {
      clearSpans()
      val context = new DefaultTracingContext(openTelemetry, isTracingEnabled = true)

      val spanBuilder = context.createDatabaseSpan(
        operationName = "db.query",
        dbSystem = Some("postgresql"),
        dbName = Some("test_db"),
        dbStatement = Some("SELECT * FROM users WHERE id = 123"),
        dbUser = Some("app_user"),
        serverAddress = Some("localhost"),
        serverPort = Some(5432)
      )

      val span = spanBuilder.startSpan()
      span.end()

      val spans = getSpans
      spans.size() shouldBe 1
      
      val finishedSpan = spans.get(0)
      finishedSpan.getName shouldBe "db.query"
      finishedSpan.getKind shouldBe SpanKind.CLIENT
      
      val attributes = finishedSpan.getAttributes
      attributes.get(io.opentelemetry.api.common.AttributeKey.stringKey("db.system")) shouldBe "postgresql"
      attributes.get(io.opentelemetry.api.common.AttributeKey.stringKey("db.name")) shouldBe "test_db"
      attributes.get(io.opentelemetry.api.common.AttributeKey.stringKey("db.statement")) shouldBe "SELECT * FROM users WHERE id = ?"
      attributes.get(io.opentelemetry.api.common.AttributeKey.stringKey("db.user")) shouldBe "app_user"
      attributes.get(io.opentelemetry.api.common.AttributeKey.stringKey("server.address")) shouldBe "localhost"
      attributes.get(io.opentelemetry.api.common.AttributeKey.longKey("server.port")) shouldBe 5432L
    }

    "create compilation spans" in {
      clearSpans()
      val context = new DefaultTracingContext(openTelemetry, isTracingEnabled = true)

      val spanBuilder = context.createCompilationSpan(
        phaseName = "InferTypes",
        queryType = Some("SELECT")
      )

      val span = spanBuilder.startSpan()
      span.end()

      val spans = getSpans
      spans.size() shouldBe 1
      
      val finishedSpan = spans.get(0)
      finishedSpan.getName shouldBe "slick.compile.InferTypes"
      finishedSpan.getKind shouldBe SpanKind.INTERNAL
      
      val attributes = finishedSpan.getAttributes
      attributes.get(io.opentelemetry.api.common.AttributeKey.stringKey("slick.compilation.phase")) shouldBe "InferTypes"
      attributes.get(io.opentelemetry.api.common.AttributeKey.stringKey("slick.query.type")) shouldBe "SELECT"
    }

    "execute operations within spans" in {
      clearSpans()
      val context = new DefaultTracingContext(openTelemetry, isTracingEnabled = true)

      val spanBuilder = context.createDatabaseSpan("test.operation")
      val future = context.withSpan(spanBuilder) { span =>
        span should not be Span.getInvalid
        Future.successful("test-result")
      }

      whenReady(future) { result =>
        result shouldBe "test-result"

        val spans = getSpans
        spans.size() shouldBe 1
        spans.get(0).getName shouldBe "test.operation"
        spans.get(0).getStatus.getStatusCode shouldBe StatusCode.OK
      }
    }

    "handle failures in async spans" in {
      clearSpans()
      val context = new DefaultTracingContext(openTelemetry, isTracingEnabled = true)

      val exception = new RuntimeException("async error")
      val spanBuilder = context.createDatabaseSpan("failing.operation")
      val future = context.withSpan(spanBuilder) { _ =>
        Future.failed(exception)
      }

      whenReady(future.failed) { error =>
        error shouldBe exception

        val spans = getSpans
        spans.size() shouldBe 1
        spans.get(0).getName shouldBe "failing.operation"
        spans.get(0).getStatus.getStatusCode shouldBe StatusCode.ERROR
        spans.get(0).getStatus.getDescription shouldBe "async error"
      }
    }

    "execute synchronous operations within spans" in {
      clearSpans()
      val context = new DefaultTracingContext(openTelemetry, isTracingEnabled = true)

      val spanBuilder = context.createDatabaseSpan("sync.operation")
      val result = context.withSpanSync(spanBuilder) { span =>
        span should not be Span.getInvalid
        "sync-result"
      }

      result shouldBe "sync-result"

      val spans = getSpans
      spans.size() shouldBe 1
      spans.get(0).getName shouldBe "sync.operation"
      spans.get(0).getStatus.getStatusCode shouldBe StatusCode.OK
    }

    "handle failures in sync spans" in {
      clearSpans()
      val context = new DefaultTracingContext(openTelemetry, isTracingEnabled = true)

      val exception = new RuntimeException("sync error")
      val spanBuilder = context.createDatabaseSpan("failing.sync.operation")

      intercept[RuntimeException] {
        context.withSpanSync(spanBuilder) { _ =>
          throw exception
        }
      }

      val spans = getSpans
      spans.size() shouldBe 1
      spans.get(0).getName shouldBe "failing.sync.operation"
      spans.get(0).getStatus.getStatusCode shouldBe StatusCode.ERROR
      spans.get(0).getStatus.getDescription shouldBe "sync error"
    }

    "sanitize SQL statements" in {
      clearSpans()
      val context = new DefaultTracingContext(openTelemetry, isTracingEnabled = true)

      val spanBuilder = context.createDatabaseSpan(
        operationName = "test.query",
        dbStatement = Some("SELECT * FROM users WHERE name = 'John Doe' AND age = 25")
      )

      val span = spanBuilder.startSpan()
      span.end()

      val spans = getSpans
      spans.size() shouldBe 1
      
      val attributes = spans.get(0).getAttributes
      val sanitizedStatement = attributes.get(io.opentelemetry.api.common.AttributeKey.stringKey("db.statement"))
      sanitizedStatement should include("SELECT * FROM users WHERE name = ? AND age = ?")
    }

    "add application tags to spans" in {
      clearSpans()
      val context = new DefaultTracingContext(openTelemetry, isTracingEnabled = true)

      val span = context.tracer.spanBuilder("test.span").startSpan()
      val tags = Map("service" -> "user-service", "version" -> "1.0.0")
      
      context.addApplicationTags(span, tags)
      span.end()

      val spans = getSpans
      spans.size() shouldBe 1
      
      val attributes = spans.get(0).getAttributes
      attributes.get(io.opentelemetry.api.common.AttributeKey.stringKey("app.service")) shouldBe "user-service"
      attributes.get(io.opentelemetry.api.common.AttributeKey.stringKey("app.version")) shouldBe "1.0.0"
    }

    "return current context" in {
      val context = new DefaultTracingContext(openTelemetry, isTracingEnabled = true)
      val childContext = context.childContext()
      childContext shouldBe a[Context]
    }

    "skip operations when tracing is disabled" in {
      clearSpans()
      val context = new DefaultTracingContext(openTelemetry, isTracingEnabled = false)

      val spanBuilder = context.createDatabaseSpan("disabled.operation")
      val future = context.withSpan(spanBuilder) { span =>
        span shouldBe Span.getInvalid
        Future.successful("result")
      }

      whenReady(future) { result =>
        result shouldBe "result"
        getSpans.size() shouldBe 0
      }
    }
  }

  "NoOpTracingContext" should {

    "provide no-op implementation" in {
      NoOpTracingContext.isTracingEnabled shouldBe false
      NoOpTracingContext.openTelemetry shouldBe OpenTelemetry.noop()
      NoOpTracingContext.tracer should not be null
    }

    "skip all tracing operations" in {
      clearSpans()

      val spanBuilder = NoOpTracingContext.createDatabaseSpan("noop.operation")
      val future = NoOpTracingContext.withSpan(spanBuilder) { span =>
        span shouldBe Span.getInvalid
        Future.successful("result")
      }

      whenReady(future) { result =>
        result shouldBe "result"
        getSpans.size() shouldBe 0
      }
    }
  }

  "TracingContext.Implicits" should {

    "provide default tracing context" in {
      import TracingContext.Implicits._
      
      defaultTracingContext shouldBe NoOpTracingContext
    }

    "provide tracing syntax for any value" in {
      clearSpans()
      
      val tracingContext: TracingContext = new DefaultTracingContext(openTelemetry, isTracingEnabled = true)
      
      import TracingContext.Implicits._
      
      val value = "test-value"
      val future = value.withTracing("traced.value")(tracingContext, ec)

      whenReady(future) { result =>
        result shouldBe "test-value"

        val spans = getSpans
        spans.size() shouldBe 1
        spans.get(0).getName shouldBe "traced.value"
      }
    }
  }
}