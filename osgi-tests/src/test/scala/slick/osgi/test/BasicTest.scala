package slick.osgi.test

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.Assert._
import org.ops4j.pax.exam
import org.ops4j.pax.exam.Configuration
import org.ops4j.pax.exam.junit.PaxExam
import org.ops4j.pax.exam.spi.reactors.{AllConfinedStagedReactorFactory, ExamReactorStrategy}
import slick.osgi.testutil._
import slick.util.GlobalConfig

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

@RunWith(classOf[PaxExam])
@ExamReactorStrategy(Array(classOf[AllConfinedStagedReactorFactory]))
class BasicTest extends SlickOsgiHelper {
  @Configuration def config() = standardOptions

  @Test
  def testPlainSQL: Unit = wrap {
    import slick.driver.H2Driver.api._
    val a = sql"select {fn database()}".as[String].head.map(res => assertEquals("TEST-OSGI", res))
    val db = Database.forURL("jdbc:h2:mem:test-osgi")
    try Await.result(db.run(a), Duration.Inf) finally db.close
  }

  @Test
  def testConfig: Unit = wrap {
    assertFalse(GlobalConfig.driverConfig("MySQL").isEmpty)
  }
}
