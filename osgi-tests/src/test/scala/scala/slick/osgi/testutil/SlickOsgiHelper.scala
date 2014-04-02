package scala.slick.osgi.testutil

import org.ops4j.pax.exam.CoreOptions._
import org.ops4j.pax.exam
import java.io.File

/**
 * Helper to communicate with promoted bundles from our sbtbuild.
 */
trait SlickOsgiHelper {
  private def makeBundle(file: File): exam.Option = 
    bundle(file.toURI.toASCIIString)


  private def allBundleFiles: Array[File] =
    Option(sys.props("slick.osgi.bundlepath")).getOrElse("").split(":").map(new File(_))

  def standardOptions: Array[exam.Option] = {
    val bundles = (allBundleFiles map makeBundle)
    bundles ++ Array[exam.Option](felix(), equinox(), junitBundles())
    // to change the local repo used (for some operations, but not all -- which is why I didn't bother):
    // systemProperty("org.ops4j.pax.url.mvn.localRepository").value(sys.props("maven.repo.local")))
  }
}
