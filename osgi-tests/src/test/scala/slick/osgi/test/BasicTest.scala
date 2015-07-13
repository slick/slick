package slick.osgi.test

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.Assert._
import org.ops4j.pax.exam
import org.ops4j.pax.exam.junit.{Configuration, ExamReactorStrategy, JUnit4TestRunner}
import org.ops4j.pax.exam.spi.reactors.AllConfinedStagedReactorFactory
import slick.SlickException
import slick.osgi.testutil._
import slick.util.GlobalConfig

@RunWith(classOf[JUnit4TestRunner])
@ExamReactorStrategy(Array(classOf[AllConfinedStagedReactorFactory]))
class BasicTest extends SlickOsgiHelper {

  @Configuration
  def config(): Array[exam.Option] = {
    standardOptions
  }

  @Test
  def testPlainSQL: Unit = {
    import slick.jdbc.JdbcBackend._
    import slick.jdbc.StaticQuery.interpolation
    Database.forURL("jdbc:h2:mem:test-osgi") withSession { implicit session =>
      assertEquals("TEST-OSGI", sql"select {fn database()}".as[String].first)
    }
  }
  @Test
  def testConfig: Unit = {
    assertFalse(GlobalConfig.driverConfig("MySQL").isEmpty)
  }
}
