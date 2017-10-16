package slick.osgi.testutil

import org.ops4j.pax.exam.CoreOptions._
import org.ops4j.pax.exam
import java.io.{StringWriter, PrintWriter, File}

import scala.util.control.NonFatal

/**
 * Helper to communicate with promoted bundles from our sbtbuild.
 */
trait SlickOsgiHelper {
  private def makeBundle(file: File): exam.Option = 
    bundle(file.toURI.toASCIIString)

  private def allBundleFiles: Array[File] = {
    val paths = Option(sys.props("slick.osgi.bundlepath")).getOrElse("")
    paths.split("@").map(new File(_))
  }

  def standardOptions: Array[exam.Option] =
    allBundleFiles.map(makeBundle) ++ Array[exam.Option](junitBundles())

  def wrap(f: => Unit): Unit =
    try f catch { case NonFatal(t) =>
      // Force all classes required for printing the exception to be loaded before the bundles are released
      t.printStackTrace(new PrintWriter(new StringWriter()))
      throw t
    }
}
