import slick.jdbc.H2Profile.api._
import slick.util.LoggingContext
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._

object TestContextLogging extends App {
  implicit val ec = ExecutionContext.global

  // Create an in-memory H2 database
  val db = Database.forURL("jdbc:h2:mem:test", driver = "org.h2.Driver")

  // Define a simple table
  class Users(tag: Tag) extends Table[(Int, String)](tag, "users") {
    def id = column[Int]("id", O.PrimaryKey)
    def name = column[String]("name")
    def * = (id, name)
  }
  val users = TableQuery[Users]

  try {
    println("=== Testing Context-Aware Logging ===")
    
    // Test 1: Regular action without context
    println("\n1. Creating table without context:")
    val createTableAction = users.schema.create
    Await.result(db.run(createTableAction), 5.seconds)
    
    // Test 2: Action with context parameter
    println("\n2. Inserting data with context parameter:")
    val context1 = LoggingContext("orgId" -> "tenant-123", "userId" -> "user-456")
    val insertAction = users += (1, "John Doe")
    Await.result(db.run(insertAction, context1), 5.seconds)
    
    // Test 3: Action with withContext method
    println("\n3. Querying data with withContext method:")
    val queryAction = users.filter(_.id === 1).result.withContext("requestId" -> "req-789")
    val result = Await.result(db.run(queryAction), 5.seconds)
    println(s"Query result: $result")
    
    // Test 4: Multiple context entries
    println("\n4. Update with multiple context entries:")
    val updateAction = users.filter(_.id === 1).map(_.name).update("Jane Doe")
      .withContext("orgId" -> "tenant-456", "operation" -> "user-update", "version" -> "v2.1")
    Await.result(db.run(updateAction), 5.seconds)
    
    // Test 5: Context merging with nested actions
    println("\n5. Nested actions with context merging:")
    val outerContext = LoggingContext("session" -> "sess-123")
    val nestedAction = (users += (2, "Bob Smith")).withContext("action" -> "create-user")
    Await.result(db.run(nestedAction, outerContext), 5.seconds)
    
    println("\n=== Test completed successfully ===")
    
  } finally {
    db.close()
  }
}