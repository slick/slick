package slick.util

/** Utilities for logging and creating tree & table dumps. */
private[slick] object LogUtil {
  val (cNormal, cBlack, cRed, cGreen, cYellow, cBlue, cMagenta, cCyan) =
    if(GlobalConfig.ansiDump) ("\u001B[0m", "\u001B[30m", "\u001B[31m", "\u001B[32m", "\u001B[33m", "\u001B[34m", "\u001B[35m", "\u001B[36m")
    else ("", "", "", "", "", "", "", "")
  val (bRed, bGreen, bYellow, bBlue, bMagenta, bCyan) =
    if(GlobalConfig.ansiDump) ("\u001B[41m", "\u001B[42m", "\u001B[43m", "\u001B[44m", "\u001B[45m", "\u001B[46m")
    else ("", "", "", "", "", "")

  private[this] val multi = if(GlobalConfig.unicodeDump) "\u2507 " else "  "
  private[this] val multilineBorderPrefix = cYellow + multi + cNormal

  def multilineBorder(s: String): String =
    multilineBorderPrefix + s.replace("\n", "\n" + multilineBorderPrefix)
}
