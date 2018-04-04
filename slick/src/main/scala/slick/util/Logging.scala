package slick.util

import org.slf4j.{Logger => Slf4jLogger, LoggerFactory}

import scala.reflect.ClassTag

final class SlickLogger(val slf4jLogger: Slf4jLogger) {
  @inline
  def debug(msg: => String, n: => Dumpable): Unit = debug(msg+"\n"+SlickLogger.treePrinter.get(n))

  @inline
  def debug(msg: => String, n: => Dumpable, mark: (Dumpable => Boolean)): Unit =
    debug(msg+"\n"+SlickLogger.treePrinter.copy(mark = mark).get(n))

  @inline
  def isDebugEnabled = slf4jLogger.isDebugEnabled()

  @inline
  def error(msg: => String): Unit = { if (slf4jLogger.isErrorEnabled) slf4jLogger.error(msg) }

  @inline
  def error(msg: => String, t: Throwable): Unit = { if (slf4jLogger.isErrorEnabled) slf4jLogger.error(msg, t) }

  @inline
  def warn(msg: => String): Unit = { if (slf4jLogger.isWarnEnabled) slf4jLogger.warn(msg) }

  @inline
  def warn(msg: => String, t: Throwable): Unit = { if (slf4jLogger.isWarnEnabled) slf4jLogger.warn(msg, t) }

  @inline
  def warn(msg: => String, n: => Dumpable): Unit = warn(msg+"\n"+SlickLogger.treePrinter.get(n))

  @inline
  def info(msg: => String): Unit = { if (slf4jLogger.isInfoEnabled) slf4jLogger.info(msg) }

  @inline
  def info(msg: => String, t: Throwable): Unit = { if (slf4jLogger.isInfoEnabled) slf4jLogger.info(msg, t) }

  @inline
  def debug(msg: => String): Unit = { if (slf4jLogger.isDebugEnabled) slf4jLogger.debug(msg) }

  @inline
  def debug(msg: => String, t: Throwable): Unit = { if (slf4jLogger.isDebugEnabled) slf4jLogger.debug(msg, t) }

  @inline
  def trace(msg: => String): Unit = { if (slf4jLogger.isTraceEnabled) slf4jLogger.trace(msg) }

  @inline
  def trace(msg: => String, t: Throwable): Unit = { if (slf4jLogger.isTraceEnabled) slf4jLogger.trace(msg, t) }
}

object SlickLogger {
  private val treePrinter =
    new TreePrinter(prefix = DumpInfo.highlight(if(GlobalConfig.unicodeDump) "\u2503 " else "| "))

  def apply[T](implicit ct: ClassTag[T]): SlickLogger =
    new SlickLogger(LoggerFactory.getLogger(ct.runtimeClass))
}

trait Logging {
  protected[this] lazy val logger = {
    val n = getClass.getName
    val cln = if(n endsWith "$") n.substring(0, n.length-1) else n
    new SlickLogger(LoggerFactory.getLogger(cln))
  }
}
