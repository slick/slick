package slick.osgi.test

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.Assert._
import org.ops4j.pax.exam
import org.ops4j.pax.exam.junit.{Configuration, ExamReactorStrategy, JUnit4TestRunner}
import org.ops4j.pax.exam.spi.reactors.AllConfinedStagedReactorFactory
import slick.osgi.testutil._
import slick.util.GlobalConfig

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

@RunWith(classOf[JUnit4TestRunner])
@ExamReactorStrategy(Array(classOf[AllConfinedStagedReactorFactory]))
class BasicTest extends SlickOsgiHelper {

  @Configuration
  def config(): Array[exam.Option] = {
    standardOptions
  }

  @Test
  def testPlainSQL: Unit = {
    import slick.driver.H2Driver.api._
    val a = sql"select {fn database()}".as[String].head.map(res => assertEquals("TEST-OSGI", res))
    val db = Database.forURL("jdbc:h2:mem:test-osgi")
    try Await.result(db.run(a), Duration.Inf) finally db.close
  }
  @Test
  def testConfig: Unit = {
    assertFalse(GlobalConfig.driverConfig("MySQL").isEmpty)
  }
}
