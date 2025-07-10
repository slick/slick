package slick.tracing

import io.opentelemetry.api.OpenTelemetry
import io.opentelemetry.api.trace.{Span, StatusCode}
import io.opentelemetry.sdk.OpenTelemetrySdk
import io.opentelemetry.sdk.testing.exporter.InMemorySpanExporter
import io.opentelemetry.sdk.trace.SdkTracerProvider
import io.opentelemetry.sdk.trace.export.SimpleSpanProcessor
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import slick.dbio.DBIOAction
import scala.concurrent.{ExecutionContext, Future}

class SimpleTracingSupportTest extends AnyWordSpec with Matchers with ScalaFutures {

  implicit val ec: ExecutionContext = ExecutionContext.global

  // Test setup with in-memory span exporter
  val spanExporter = InMemorySpanExporter.create()
  val tracerProvider = SdkTracerProvider.builder()
    .addSpanProcessor(SimpleSpanProcessor.create(spanExporter))
    .build()
  val openTelemetry: OpenTelemetry = OpenTelemetrySdk.builder()
    .setTracerProvider(tracerProvider)
    .build()

  class TestTracingSupport extends SimpleTracingSupport {
    override def openTelemetry: OpenTelemetry = SimpleTracingSupportTest.this.openTelemetry
    override def tracingEnabled: Boolean = true
  }

  class DisabledTracingSupport extends SimpleTracingSupport {
    override def openTelemetry: OpenTelemetry = SimpleTracingSupportTest.this.openTelemetry
    override def tracingEnabled: Boolean = false
  }

  def clearSpans(): Unit = {
    spanExporter.reset()
  }

  def getSpans = spanExporter.getFinishedSpanItems

  "SimpleTracingSupport" should {

    "create spans when tracing is enabled" in {
      clearSpans()
      val support = new TestTracingSupport

      val span = support.createSpan("test-operation")
      span should not be Span.getInvalid
      span.end()

      val spans = getSpans
      spans.size() shouldBe 1
      spans.get(0).getName shouldBe "test-operation"
    }

    "return invalid span when tracing is disabled" in {
      val support = new DisabledTracingSupport

      val span = support.createSpan("test-operation")
      span shouldBe Span.getInvalid
    }

    "wrap DBIOAction with tracing" in {
      clearSpans()
      val support = new TestTracingSupport

      val action = DBIOAction.successful(42)
      val tracedAction = support.withTracing("db-operation", action)

      // Verify the traced action is created without error
      tracedAction should not be null

      // The actual span creation happens during execution
      // For a complete test, we'd need a real database execution context
    }

    "wrap Future with tracing" in {
      clearSpans()
      val support = new TestTracingSupport

      val future = Future.successful("test-result")
      val tracedFuture = support.withTracingFuture("future-operation", future)

      whenReady(tracedFuture) { result =>
        result shouldBe "test-result"

        val spans = getSpans
        spans.size() shouldBe 1
        spans.get(0).getName shouldBe "future-operation"
        spans.get(0).getStatus.getStatusCode shouldBe StatusCode.OK
      }
    }

    "handle Future failures with tracing" in {
      clearSpans()
      val support = new TestTracingSupport

      val exception = new RuntimeException("test error")
      val future = Future.failed(exception)
      val tracedFuture = support.withTracingFuture("failing-operation", future)

      whenReady(tracedFuture.failed) { error =>
        error shouldBe exception

        val spans = getSpans
        spans.size() shouldBe 1
        spans.get(0).getName shouldBe "failing-operation"
        spans.get(0).getStatus.getStatusCode shouldBe StatusCode.ERROR
        spans.get(0).getStatus.getDescription shouldBe "test error"
      }
    }

    "execute synchronous operations with spans" in {
      clearSpans()
      val support = new TestTracingSupport

      val result = support.withSpanSync("sync-operation") { span =>
        span should not be Span.getInvalid
        "sync-result"
      }

      result shouldBe "sync-result"

      val spans = getSpans
      spans.size() shouldBe 1
      spans.get(0).getName shouldBe "sync-operation"
      spans.get(0).getStatus.getStatusCode shouldBe StatusCode.OK
    }

    "handle synchronous exceptions with spans" in {
      clearSpans()
      val support = new TestTracingSupport

      val exception = new RuntimeException("sync error")

      intercept[RuntimeException] {
        support.withSpanSync("failing-sync-operation") { _ =>
          throw exception
        }
      }

      val spans = getSpans
      spans.size() shouldBe 1
      spans.get(0).getName shouldBe "failing-sync-operation"
      spans.get(0).getStatus.getStatusCode shouldBe StatusCode.ERROR
      spans.get(0).getStatus.getDescription shouldBe "sync error"
    }

    "skip tracing when disabled" in {
      clearSpans()
      val support = new DisabledTracingSupport

      val future = Future.successful("result")
      val tracedFuture = support.withTracingFuture("disabled-operation", future)

      whenReady(tracedFuture) { result =>
        result shouldBe "result"
        getSpans.size() shouldBe 0
      }
    }
  }

  "TracedH2Profile" should {

    "create a working profile with tracing" in {
      val profile = new TracedH2Profile(openTelemetry)
      profile should not be null
      profile.openTelemetry shouldBe openTelemetry
      profile.tracingEnabled shouldBe true
    }
  }

  "TracedPostgresProfile" should {

    "create a working profile with tracing" in {
      val profile = new TracedPostgresProfile(openTelemetry)
      profile should not be null
      profile.openTelemetry shouldBe openTelemetry
      profile.tracingEnabled shouldBe true
    }
  }

  "TracedMySQLProfile" should {

    "create a working profile with tracing" in {
      val profile = new TracedMySQLProfile(openTelemetry)
      profile should not be null
      profile.openTelemetry shouldBe openTelemetry
      profile.tracingEnabled shouldBe true
    }
  }

  "TracedProfiles factory" should {

    "create H2 profile" in {
      val profile = TracedProfiles.h2(openTelemetry)
      profile shouldBe a[TracedH2Profile]
      profile.openTelemetry shouldBe openTelemetry
    }

    "create PostgreSQL profile" in {
      val profile = TracedProfiles.postgres(openTelemetry)
      profile shouldBe a[TracedPostgresProfile]
      profile.openTelemetry shouldBe openTelemetry
    }

    "create MySQL profile" in {
      val profile = TracedProfiles.mysql(openTelemetry)
      profile shouldBe a[TracedMySQLProfile]
      profile.openTelemetry shouldBe openTelemetry
    }
  }
}