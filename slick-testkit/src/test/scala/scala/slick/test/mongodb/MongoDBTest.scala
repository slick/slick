package scala.slick.test.mongodb

import org.junit.runner.RunWith
import com.typesafe.slick.testkit.util.{RelationalTestDB, DriverTest, Testkit}
import scala.slick.mongodb.{MongoBackend, MongoDriver}
import org.junit.Assert

@RunWith(classOf[Testkit])
class MongoDBTest extends DriverTest(MongoDBTest.TestDB)


object MongoDBTest {

  abstract class MongoTestDB extends RelationalTestDB {
    type Driver = MongoDriver.type
    val driver = MongoDriver

    // url
    def assertNotTablesExist(tables: String*)(implicit session: MongoBackend#Session) {
      for (t <- tables) {
        try {
          if (session.database.tableExists(t)) Assert.fail(s"Table '$t' should not exist.")
        } catch { case _: Exception => }
      }
    }

    def assertTablesExist(tables: String*)(implicit session: MongoBackend#Session) {
      for (t <- tables) {
        if (!session.database.tableExists(t)) Assert.fail(s"Table '$t' should exist.")
      }
    }

    val confName: String = "MongoDB"

    /** Create the Database object for this test database configuration */
    def createDB() = MongoBackend.Database.forConnection()

    /** This method is called between individual test methods to remove all
      * database artifacts that were created by the test. */
    def dropUserArtifacts(implicit session: MongoBackend#Session) {
      session.database.createConnection().dropDatabase()
    }
  }

  def TestDB = new MongoTestDB {}
}

