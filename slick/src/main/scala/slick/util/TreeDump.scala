package slick.util

import java.io.{OutputStreamWriter, StringWriter, PrintWriter}
import LogUtil._

/** Create a readable printout of a tree. */
object TreeDump {
  private[this] val (childPrefix1, childPrefix2, lastChildPrefix1, lastChildPrefix2, multi1, multi2) =
    if(GlobalConfig.unicodeDump) ("\u2523 ", "\u2503 ", "\u2517 ", "  ", "\u250f ", "\u2507 ")
    else ("  ", "  ", "  ", "  ", ": ", ": ")

  def get(n: Dumpable, name: String = "", prefix: String = "", firstPrefix: String = null, narrow: (Dumpable => Dumpable) = identity) = {
    val buf = new StringWriter
    apply(n, name, prefix, firstPrefix, new PrintWriter(buf), narrow)
    buf.getBuffer.toString
  }

  def apply(n: Dumpable, name: String = "", prefix: String = "", firstPrefix: String = null, out: PrintWriter = new PrintWriter(new OutputStreamWriter(System.out)), narrow: (Dumpable => Dumpable) = identity) {
    def dump(baseValue: Dumpable, prefix1: String, prefix2: String, name: String, level: Int) {
      val value = narrow(baseValue)
      val di =
        if(value eq null) DumpInfo("<error: narrowed to null>", "baseValue = "+baseValue)
        else value.getDumpInfo
      val multiLine = di.mainInfo contains '\n'
      out.print(
        prefix1 +
        cCyan + (if(name.nonEmpty) name + ": " else "") +
        cYellow + (if(multiLine) multi1 else "") + di.name + (if(di.name.nonEmpty && di.mainInfo.nonEmpty) " " else "") +
        cNormal
      )
      if(multiLine) {
        val lines = di.mainInfo.replace("\r", "").split('\n')
        out.println(if(di.attrInfo.isEmpty) "" else cBlue + di.attrInfo + cNormal)
        val p = prefix2 + Iterator.fill(name.length + (if(name.length == 0) 0 else 2))(' ').mkString + cYellow + multi2 + cNormal
        lines.foreach { l => out.println(p + l) }
      } else {
        out.println(di.mainInfo + (if(di.attrInfo.isEmpty) "" else " " + cBlue + di.attrInfo + cNormal))
      }
      val children = di.children.toIndexedSeq
      children.zipWithIndex.foreach { case ((name, value), idx) =>
        val (p1, p2) = if(idx == children.size-1) (lastChildPrefix1, lastChildPrefix2) else (childPrefix1, childPrefix2)
        val (cp1, cp2) = if(level % 2 == 0) (cBlue + p1, cBlue + p2) else (cGreen + p1, cGreen + p2)
        dump(value, prefix2 + cp1, prefix2 + cp2, name, level + 1)
      }
    }
    dump(n, if(firstPrefix ne null) firstPrefix else prefix, prefix, name, 0)
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
  def simpleNameFor(cl: Class[_]): String = cl.getName.replaceFirst(".*\\.", "")

  def highlight(s: String) = cGreen + s + cNormal
}
