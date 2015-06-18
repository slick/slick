package slick

import slick.util.{GlobalConfig, DumpInfo, TreePrinter, Dumpable}

/** All Exceptions that are thrown directly by Slick are of type `SlickException`.
  * Other Exceptions originating in non-Slick code are generally not wrapped but
  * passed on directly.
  *
  * @param msg The error message
  * @param parent An optional parent Exception or `null`
  */
class SlickException(msg: String, parent: Throwable = null) extends RuntimeException(msg, parent)

/** A SlickException that contains a `Dumpable` with more details. */
class SlickTreeException(msg: String, detail: Dumpable, parent: Throwable = null, mark: (Dumpable => Boolean) = null, removeUnmarked: Boolean = true)
  extends SlickException(SlickTreeException.format(msg, detail, mark, removeUnmarked), parent)

private[slick] object SlickTreeException {
  val treePrinter = new TreePrinter(prefix = DumpInfo.highlight(if(GlobalConfig.unicodeDump) "\u2503 " else "| "))

  def format(msg: String, detail: Dumpable, _mark: (Dumpable => Boolean), removeUnmarked: Boolean): String =
    if(detail eq null) msg else msg + {
      try {
        val mark = if(_mark eq null) ((_: Dumpable) => false) else _mark
        val tp = treePrinter.copy(mark = mark)
        val markedTop =
          if(!removeUnmarked || (_mark eq null)) detail else tp.findMarkedTop(detail)
        "\n" + tp.get(markedTop)
      } catch { case t: Throwable => " <Error formatting detail: " + t + ">" }
    }
}
