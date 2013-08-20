package scala.slick.util

import org.slf4j.{ Logger => Slf4jLogger, LoggerFactory }
import scala.slick.ast.Node
import scala.slick.ast.Dump

final class SlickLogger(val slf4jLogger: Slf4jLogger) {
  @inline
  def debug(msg: => String, n: => Node): Unit = debug(msg+"\n"+Dump.get(n, prefix = "  "))

  @inline
  def isDebugEnabled = slf4jLogger.isDebugEnabled()

  @inline
  def error(msg: => String) { if (slf4jLogger.isErrorEnabled) slf4jLogger.error(msg) }

  @inline
  def error(msg: => String, t: Throwable) { if (slf4jLogger.isErrorEnabled) slf4jLogger.error(msg, t) }

  @inline
  def warn(msg: => String) { if (slf4jLogger.isWarnEnabled) slf4jLogger.warn(msg) }

  @inline
  def warn(msg: => String, t: Throwable) { if (slf4jLogger.isWarnEnabled) slf4jLogger.warn(msg, t) }

  @inline
  def info(msg: => String) { if (slf4jLogger.isInfoEnabled) slf4jLogger.info(msg) }

  @inline
  def info(msg: => String, t: Throwable) { if (slf4jLogger.isInfoEnabled) slf4jLogger.info(msg, t) }

  @inline
  def debug(msg: => String) { if (slf4jLogger.isDebugEnabled) slf4jLogger.debug(msg) }

  @inline
  def debug(msg: => String, t: Throwable) { if (slf4jLogger.isDebugEnabled) slf4jLogger.debug(msg, t) }

  @inline
  def trace(msg: => String) { if (slf4jLogger.isTraceEnabled) slf4jLogger.trace(msg) }

  @inline
  def trace(msg: => String, t: Throwable) { if (slf4jLogger.isTraceEnabled) slf4jLogger.trace(msg, t) }
}

trait Logging {
  protected[this] lazy val logger = new SlickLogger(LoggerFactory.getLogger(getClass))
}
