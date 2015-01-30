package scala.slick.util

/** Utilities for logging and creating tree & table dumps. */
private[slick] object LogUtil {
  val (cNormal, cGreen, cYellow, cBlue, cCyan) =
    if(GlobalConfig.ansiDump) ("\u001B[0m", "\u001B[32m", "\u001B[33m", "\u001B[34m", "\u001B[36m")
    else ("", "", "", "", "")

  private[this] val multi = if(GlobalConfig.unicodeDump) "\u2507 " else "  "
  private[this] val multilineBorderPrefix = cYellow + multi + cNormal

  def multilineBorder(s: String): String =
    multilineBorderPrefix + s.replace("\n", "\n" + multilineBorderPrefix)
}
