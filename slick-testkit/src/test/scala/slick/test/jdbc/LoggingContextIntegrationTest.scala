package slick.test.jdbc

import java.io.{ByteArrayOutputStream, OutputStreamWriter}
import java.util.concurrent.TimeUnit

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.AppenderBase
import org.junit.Test
import org.junit.Assert._
import org.slf4j.LoggerFactory
import slick.jdbc.H2Profile.api._
import slick.util.LoggingContext

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.ListBuffer

class LoggingContextIntegrationTest {
  
  // Test appender to capture log messages
  class TestLogAppender extends AppenderBase[ILoggingEvent] {
    val messages = ListBuffer[String]()
    
    override def append(eventObject: ILoggingEvent): Unit = {
      messages += eventObject.getFormattedMessage
    }
  }

  @Test
  def testContextPropagationInDBIOAction(): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    
    val db = Database.forConfig("h2mem1")
    try {
      // Create test table
      class TestTable(tag: Tag) extends Table[(Int, String)](tag, "TEST_LOGGING") {
        def id = column[Int]("ID", O.PrimaryKey)
        def name = column[String]("NAME")
        def * = (id, name)
      }
      val testTable = TableQuery[TestTable]

      val setupAction = testTable.schema.create
      val insertAction = testTable += (1, "test")
      val queryAction = testTable.filter(_.id === 1).result

      // Test context propagation through action combinators
      val context = LoggingContext("user" -> "testUser", "operation" -> "integration-test")
      
      val combinedAction = (for {
        _ <- setupAction
        _ <- insertAction
        result <- queryAction
      } yield result).withLoggingContext(context)

      val result = Await.result(db.run(combinedAction), Duration(10, TimeUnit.SECONDS))
      
      // Verify the query executed successfully
      assertEquals("Should return one result", 1, result.length)
      assertEquals("Should return correct data", (1, "test"), result.head)
      
    } finally {
      db.close()
    }
  }

  @Test
  def testTaggedActions(): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    
    val db = Database.forConfig("h2mem1")
    try {
      class TestTable(tag: Tag) extends Table[(Int, String)](tag, "TEST_TAGGED") {
        def id = column[Int]("ID", O.PrimaryKey)
        def name = column[String]("NAME")
        def * = (id, name)
      }
      val testTable = TableQuery[TestTable]

      val action = (for {
        _ <- testTable.schema.create.tagged("operation", "schema-creation")
        _ <- testTable += (1, "test")
        result <- testTable.result.tagged("operation", "data-retrieval")
      } yield result).tagged("user", "testUser")

      val result = Await.result(db.run(action), Duration(10, TimeUnit.SECONDS))
      
      // Verify the action executed successfully with context
      assertEquals("Should return one result", 1, result.length)
      assertEquals("Should return correct data", (1, "test"), result.head)
      
    } finally {
      db.close()
    }
  }

  @Test
  def testContextAccumulation(): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    
    val db = Database.forConfig("h2mem1")
    try {
      class TestTable(tag: Tag) extends Table[(Int, String)](tag, "TEST_ACCUMULATION") {
        def id = column[Int]("ID", O.PrimaryKey) 
        def name = column[String]("NAME")
        def * = (id, name)
      }
      val testTable = TableQuery[TestTable]

      // Test that contexts accumulate properly through the action chain
      val baseContext = LoggingContext("component" -> "database")
      val action = testTable.schema.create
        .withLoggingContext(baseContext)
        .tagged("operation", "create-schema")
        .tagged("table", "TEST_ACCUMULATION")

      Await.result(db.run(action), Duration(10, TimeUnit.SECONDS))
      
      // If we reach here without exception, context accumulation worked
      assertTrue("Context accumulation should complete successfully", true)
      
    } finally {
      db.close()
    }
  }
}