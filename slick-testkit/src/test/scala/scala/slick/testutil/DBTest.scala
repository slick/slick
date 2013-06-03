package scala.slick.testutil

import scala.collection.JavaConversions
import org.junit.{Before, After}
import org.junit.runner.{JUnitCore, RunWith}
import org.junit.runners.Parameterized
import org.junit.runners.Parameterized.Parameters
import com.typesafe.slick.testkit.util.{JdbcTestDB, TestDB}

@RunWith(classOf[Parameterized])
abstract class DBTest {
  val tdb: JdbcTestDB

  lazy val db = tdb.createDB()

  println("[Using test database "+tdb+"]")

  @Before def beforeDBTest = tdb.cleanUpBefore()
  @After def afterDBTest = tdb.cleanUpAfter()
}

abstract class DBTestObject(dbs: TestDB.TestDBSpec*) {
  val testClassName = {
    val s = getClass.getName
    s.substring(0, s.length-1)
  }
  def main(args: Array[String]) = JUnitCore.main(testClassName)
  @Parameters def parameters = JavaConversions.seqAsJavaList(dbs.map(n => n(testClassName)).filter(_.isEnabled).map(to => Array(to)))
}
