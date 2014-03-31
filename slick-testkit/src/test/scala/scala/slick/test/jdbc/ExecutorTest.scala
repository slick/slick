package scala.slick.test.jdbc

import scala.language.higherKinds
import scala.slick.testutil._
import scala.slick.testutil.TestDBs._
import com.typesafe.slick.testkit.util.JdbcTestDB

object ExecutorTest extends DBTestObject()
class ExecutorTest(val tdb: JdbcTestDB) extends DBTest {
  import tdb.profile.backend.Database.dynamicSession
  import tdb.profile.simple._
  def all[E, C[_]](q: Query[_, E, C]) = {
  	// static tests if the implicit conversions can be applied
    q.list
    q.run
  }
}
