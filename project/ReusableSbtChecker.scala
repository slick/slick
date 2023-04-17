import java.io.File

import sbt.MessageOnlyException
import sbt.util.Logger
import sbtsdlc.Checker


/**
 * Based on the default task value of `sdlc` but modified
 * so a single project can easily run it on multiple outputs
 */
class ReusableSbtChecker(val scaladocDir: String,
                         val scanDir: String,
                         val linkBase: String,
                         log: Logger) extends Checker {
  var ok = true

  override def debug(msg: => String): Unit = log.debug(msg)

  override def info(msg: => String): Unit = log.info(msg)

  override def error(msg: => String): Unit = {
    ok = false
    log.error(msg)
  }


  def run(): Unit = {
    if (!new File(scaladocDir).exists())
      throw new MessageOnlyException("sdlcDocDir '" + scaladocDir + "' does not exist")

    detect212()

    buildModel()
    scanPages()

    if (!ok) throw new MessageOnlyException("There were errors during scaladoc link checking")
    else info(s"Scaladoc link checking successful.")
  }
}
