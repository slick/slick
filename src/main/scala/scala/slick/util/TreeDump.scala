package scala.slick.util

import java.io.{OutputStreamWriter, StringWriter, PrintWriter}

/** Create a readable printout of a tree. */
object TreeDump {
  private[util] val (normal, green, yellow, blue, cyan) =
    if(GlobalConfig.ansiDump) ("\u001B[0m", "\u001B[32m", "\u001B[33m", "\u001B[34m", "\u001B[36m")
    else ("", "", "", "", "")

  private[this] val (childPrefix1, childPrefix2, lastChildPrefix1, lastChildPrefix2) =
    if(GlobalConfig.unicodeDump) ("\u2523 ", "\u2503 ", "\u2517 ", "  ")
    else ("  ", "  ", "  ", "  ")

  def get(n: Dumpable, name: String = "", prefix: String = "") = {
    val buf = new StringWriter
    apply(n, name, prefix, new PrintWriter(buf))
    buf.getBuffer.toString
  }

  def apply(n: Dumpable, name: String = "", prefix: String = "", out: PrintWriter = new PrintWriter(new OutputStreamWriter(System.out))) {
    def dump(value: Dumpable, prefix1: String, prefix2: String, name: String, level: Int) {
      val di = value.getDumpInfo
      out.println(
        prefix1 +
        cyan + (if(name.nonEmpty) name + ": " else "") +
        yellow + di.name + (if(di.name.nonEmpty && di.mainInfo.nonEmpty) " " else "") +
        normal + di.mainInfo +
        (if(di.attrInfo.isEmpty) "" else " " + blue + di.attrInfo + normal)
      )
      val children = di.children.toIndexedSeq
      children.zipWithIndex.foreach { case ((name, value), idx) =>
        val (p1, p2) = if(idx == children.size-1) (lastChildPrefix1, lastChildPrefix2) else (childPrefix1, childPrefix2)
        val (cp1, cp2) = if(level % 2 == 0) (blue + p1, blue + p2) else (green + p1, green + p2)
        dump(value, prefix2 + cp1, prefix2 + cp2, name, level + 1)
      }
    }
    dump(n, prefix, prefix, name, 0)
    out.flush()
  }
}

/** Interface for types that can be used in a tree dump */
trait Dumpable {
  /** Return the name, main info, attribute info and named children */
  def getDumpInfo: DumpInfo
}

/** The information required for dumping a single object */
case class DumpInfo(name: String, mainInfo: String = "", attrInfo: String = "", children: Iterable[(String, Dumpable)] = Vector.empty) {
  def getNamePlusMainInfo = if(name.nonEmpty && mainInfo.nonEmpty) name + " " + mainInfo else name + mainInfo
}

object DumpInfo {
  def highlight(s: String) = TreeDump.green + s + TreeDump.normal
}
