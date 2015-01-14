package scala.slick.test.jdbc

import scala.language.higherKinds
import scala.slick.testutil._
import com.typesafe.slick.testkit.util.{DBTest, DBTestObject, JdbcTestDB}

object ExecutorTest extends DBTestObject()

@deprecated("Using deprecated .simple API", "3.0")
class ExecutorTest(val tdb: JdbcTestDB) extends DBTest {
  import tdb.profile.backend.Database.dynamicSession
  import tdb.profile.simple._
  def all[E, C[_]](q: Query[_, E, C]) = {
  	// static tests if the implicit conversions can be applied
    q.list
    q.run
  }
}
