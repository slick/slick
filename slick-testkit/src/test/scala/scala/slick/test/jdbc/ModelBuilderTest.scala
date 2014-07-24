package scala.slick.test.jdbc

import org.junit.Test
import org.junit.Assert._
import scala.slick.testutil._
import com.typesafe.slick.testkit.util.{DBTest, DBTestObject, JdbcTestDB}
import com.typesafe.slick.testkit.util.StandardTestDBs._
import scala.slick.jdbc.StaticQuery.interpolation

object ModelBuilderTest extends DBTestObject(H2Mem)

class ModelBuilderTest(val tdb: JdbcTestDB) extends DBTest {
  @Test def test(): Unit = db withSession { implicit s =>
    sqlu"""create table SUPPLIERS (FOO VARCHAR(255) DEFAULT CURRENT_USER)""".execute
    try{
      tdb.profile.createModel(ignoreInvalidDefaults=false)
      assert(false)
    } catch {
      case e:scala.slick.SlickException if e.getMessage.contains("not parse default") =>
    }
  }
}
