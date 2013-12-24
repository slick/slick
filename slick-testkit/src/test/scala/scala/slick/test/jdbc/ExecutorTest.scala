package scala.slick.test.jdbc

import scala.slick.testutil._
import scala.slick.testutil.TestDBs._
import com.typesafe.slick.testkit.util.JdbcTestDB

object ExecutorTest extends DBTestObject()
class ExecutorTest(val tdb: JdbcTestDB) extends DBTest {
  import tdb.profile.backend.Database.dynamicSession
  import tdb.profile.simple._
  def all[E](q: Query[_, E]) = {
  	// static tests if the implicit conversions can be applied
    q.list
    q.run
  }
}
