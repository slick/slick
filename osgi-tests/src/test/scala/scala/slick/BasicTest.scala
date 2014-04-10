package scala.slick

import org.junit.Assert._
import org.ops4j.pax.exam.CoreOptions._
 
import org.junit.Test
import org.junit.runner.RunWith
import org.ops4j.pax.exam
import org.ops4j.pax.exam.junit.{
  Configuration,
  ExamReactorStrategy,
  JUnit4TestRunner
}
import org.ops4j.pax.exam.spi.reactors.AllConfinedStagedReactorFactory
import org.ops4j.pax.swissbox.framework.ServiceLookup
import org.osgi.framework.BundleContext
import scala.slick.SlickException

@RunWith(classOf[JUnit4TestRunner])
@ExamReactorStrategy(Array(classOf[AllConfinedStagedReactorFactory]))
class BasicTest extends testutil.SlickOsgiHelper {

  @Configuration
  def config(): Array[exam.Option] = {
    standardOptions
  }
 
  @Test
  def everythingLoads(): Unit = println("Running PAX-Exam test.")


  @Test(expected=classOf[SlickException])  
  def canDoSomethingSlick(): Unit = {
    throw new SlickException("Test failure")
  }
}