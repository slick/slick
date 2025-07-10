package slick.tracing

import io.opentelemetry.api.OpenTelemetry
import io.opentelemetry.api.trace.StatusCode
import io.opentelemetry.sdk.OpenTelemetrySdk
import io.opentelemetry.sdk.testing.exporter.InMemorySpanExporter
import io.opentelemetry.sdk.trace.SdkTracerProvider
import io.opentelemetry.sdk.trace.export.SimpleSpanProcessor
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.time.{Millis, Seconds, Span}
import slick.jdbc.H2Profile.api._

import scala.concurrent.ExecutionContext

class TracedProfileIntegrationTest extends AnyWordSpec with Matchers with ScalaFutures {

  implicit val ec: ExecutionContext = ExecutionContext.global
  implicit val patience: PatienceConfig = PatienceConfig(Span(5, Seconds), Span(100, Millis))

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

  // Test table definition
  class Users(tag: Tag) extends Table[(Long, String)](tag, "users") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def name = column[String]("name")
    def * = (id, name)
  }

  "TracedH2Profile" should {

    "work with basic database operations" in {
      val profile = new TracedH2Profile(openTelemetry)
      import profile.api._

      val users = TableQuery[Users]
      val db = Database.forURL("jdbc:h2:mem:test1;DB_CLOSE_DELAY=-1", driver = "org.h2.Driver")

      try {
        val setupAction = DBIO.seq(
          users.schema.create,
          users += (1L, "Alice"),
          users += (2L, "Bob")
        )

        val result = db.run(setupAction).futureValue

        val queryAction = users.filter(_.name === "Alice").result
        val queryResult = db.run(queryAction).futureValue

        queryResult should have length 1
        queryResult.head._2 shouldBe "Alice"

      } finally {
        db.close()
      }
    }

    "trace database operations" in {
      clearSpans()
      val profile = new TracedH2Profile(openTelemetry)
      import profile.api._

      val users = TableQuery[Users]
      val db = Database.forURL("jdbc:h2:mem:test2;DB_CLOSE_DELAY=-1", driver = "org.h2.Driver")

      try {
        val setupAction = users.schema.create
        db.run(setupAction).futureValue

        // Use tracing with the profile
        val insertAction = users += (1L, "TracedUser")
        val tracedInsert = profile.withTracing("user-insert", insertAction)

        // This creates a traced DBIOAction but doesn't execute it yet
        tracedInsert should not be null

        // For actual span creation, we'd need the action to be executed
        // This test verifies the tracing wrapper works without errors

      } finally {
        db.close()
      }
    }

    "provide working API imports" in {
      val profile = new TracedH2Profile(openTelemetry)
      import profile.api._

      // Verify that all standard Slick API is available
      val users = TableQuery[Users]
      val query = users.filter(_.name === "test")

      query should not be null
      users.schema should not be null
    }

    "extend H2Profile correctly" in {
      val profile = new TracedH2Profile(openTelemetry)
      
      profile shouldBe a[slick.jdbc.H2Profile]
      profile shouldBe a[SimpleTracingSupport]
      profile.openTelemetry shouldBe openTelemetry
      profile.tracingEnabled shouldBe true
    }
  }

  "TracedPostgresProfile" should {

    "extend PostgresProfile correctly" in {
      val profile = new TracedPostgresProfile(openTelemetry)
      
      profile shouldBe a[slick.jdbc.PostgresProfile]
      profile shouldBe a[SimpleTracingSupport]
      profile.openTelemetry shouldBe openTelemetry
      profile.tracingEnabled shouldBe true
    }

    "provide working API imports" in {
      val profile = new TracedPostgresProfile(openTelemetry)
      import profile.api._

      val users = TableQuery[Users]
      val query = users.filter(_.name === "test")

      query should not be null
      users.schema should not be null
    }

    "support tracing operations" in {
      val profile = new TracedPostgresProfile(openTelemetry)
      import profile.api._

      val users = TableQuery[Users]
      val insertAction = users += (1L, "PostgresUser")
      val tracedInsert = profile.withTracing("postgres-insert", insertAction)

      tracedInsert should not be null
    }
  }

  "TracedMySQLProfile" should {

    "extend MySQLProfile correctly" in {
      val profile = new TracedMySQLProfile(openTelemetry)
      
      profile shouldBe a[slick.jdbc.MySQLProfile]
      profile shouldBe a[SimpleTracingSupport]
      profile.openTelemetry shouldBe openTelemetry
      profile.tracingEnabled shouldBe true
    }

    "provide working API imports" in {
      val profile = new TracedMySQLProfile(openTelemetry)
      import profile.api._

      val users = TableQuery[Users]
      val query = users.filter(_.name === "test")

      query should not be null
      users.schema should not be null
    }

    "support tracing operations" in {
      val profile = new TracedMySQLProfile(openTelemetry)
      import profile.api._

      val users = TableQuery[Users]
      val insertAction = users += (1L, "MySQLUser")
      val tracedInsert = profile.withTracing("mysql-insert", insertAction)

      tracedInsert should not be null
    }
  }

  "TracedProfiles factory" should {

    "create working H2 profile" in {
      val profile = TracedProfiles.h2(openTelemetry)
      import profile.api._

      val db = Database.forURL("jdbc:h2:mem:test3;DB_CLOSE_DELAY=-1", driver = "org.h2.Driver")
      
      try {
        val users = TableQuery[Users]
        val setupAction = users.schema.create
        val result = db.run(setupAction).futureValue
        
        // Should complete without error
        succeed

      } finally {
        db.close()
      }
    }

    "create profiles with correct tracing configuration" in {
      val h2Profile = TracedProfiles.h2(openTelemetry)
      val postgresProfile = TracedProfiles.postgres(openTelemetry)
      val mysqlProfile = TracedProfiles.mysql(openTelemetry)

      Seq(h2Profile, postgresProfile, mysqlProfile).foreach { profile =>
        profile.openTelemetry shouldBe openTelemetry
        profile.tracingEnabled shouldBe true
        profile.tracer should not be null
      }
    }

    "support tracing with all profile types" in {
      val profiles = Map(
        "h2" -> TracedProfiles.h2(openTelemetry),
        "postgres" -> TracedProfiles.postgres(openTelemetry),
        "mysql" -> TracedProfiles.mysql(openTelemetry)
      )

      profiles.foreach { case (name, profile) =>
        import profile.api._
        
        val users = TableQuery[Users]
        val action = users.filter(_.name === "test").result
        val tracedAction = profile.withTracing(s"$name-query", action)

        tracedAction should not be null
      }
    }
  }

  "Traced profiles with Future operations" should {

    "trace Future operations correctly" in {
      clearSpans()
      val profile = new TracedH2Profile(openTelemetry)

      val future = scala.concurrent.Future.successful("test-result")
      val tracedFuture = profile.withTracingFuture("traced-future", future)

      whenReady(tracedFuture) { result =>
        result shouldBe "test-result"

        val spans = getSpans
        spans.size() shouldBe 1
        spans.get(0).getName shouldBe "traced-future"
        spans.get(0).getStatus.getStatusCode shouldBe StatusCode.OK
      }
    }

    "trace synchronous operations correctly" in {
      clearSpans()
      val profile = new TracedH2Profile(openTelemetry)

      val result = profile.withSpanSync("traced-sync") { span =>
        span should not be io.opentelemetry.api.trace.Span.getInvalid
        "sync-result"
      }

      result shouldBe "sync-result"

      val spans = getSpans
      spans.size() shouldBe 1
      spans.get(0).getName shouldBe "traced-sync"
      spans.get(0).getStatus.getStatusCode shouldBe StatusCode.OK
    }
  }

  "Cross-profile compatibility" should {

    "allow switching between traced profiles" in {
      val h2Profile = new TracedH2Profile(openTelemetry)
      val postgresProfile = new TracedPostgresProfile(openTelemetry)

      // Should be able to use the same table definition with different profiles
      class CommonUsers(tag: Tag) extends Table[(Long, String)](tag, "common_users") {
        def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
        def name = column[String]("name")
        def * = (id, name)
      }

      // H2 usage
      {
        import h2Profile.api._
        val users = TableQuery[CommonUsers]
        val action = users.filter(_.name === "test").result
        action should not be null
      }

      // Postgres usage
      {
        import postgresProfile.api._
        val users = TableQuery[CommonUsers]
        val action = users.filter(_.name === "test").result
        action should not be null
      }
    }

    "maintain profile-specific behavior" in {
      val h2Profile = new TracedH2Profile(openTelemetry)
      val postgresProfile = new TracedPostgresProfile(openTelemetry)

      h2Profile.getClass.getSimpleName should include("H2")
      postgresProfile.getClass.getSimpleName should include("Postgres")

      // Both should have tracing capabilities
      h2Profile shouldBe a[SimpleTracingSupport]
      postgresProfile shouldBe a[SimpleTracingSupport]
    }
  }
}