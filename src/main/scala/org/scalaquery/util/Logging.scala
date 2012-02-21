package org.scalaquery.util

import org.slf4j.LoggerFactory
import org.slf4j.spi.{ LocationAwareLogger => Slf4jLocationAwareLogger }
import com.weiglewilczek.slf4s._
import org.scalaquery.ast.NodeGenerator

trait SlickLogger extends Logger {
  def debug(msg: => String, ng: => NodeGenerator): Unit = debug(msg+"\n"+ng.dumpString(prefix = "  "))
}

trait Logging {
  protected lazy val logger: SlickLogger = LoggerFactory.getLogger(getClass) match {
    case l: Slf4jLocationAwareLogger => new DefaultLocationAwareLogger(l)
    case l => new SlickLogger { override protected val slf4jLogger = l }
  }
}

final class DefaultLocationAwareLogger(override protected val slf4jLogger: Slf4jLocationAwareLogger)
  extends LocationAwareLogger with SlickLogger {
  override protected val wrapperClassName = classOf[DefaultLocationAwareLogger].getName
}
