package scala.slick.util

import java.io.{OutputStreamWriter, StringWriter, PrintWriter}

/** Create a readable printout of a tree. */
object TreeDump {
  private[util] val (normal, green, yellow, blue, cyan) = {
    if(GlobalConfig.ansiDump) ("\u001B[0m", "\u001B[32m", "\u001B[33m", "\u001B[34m", "\u001B[36m")
    else ("", "", "", "", "")
  }

  def get(n: Dumpable, name: String = "", prefix: String = "") = {
    val buf = new StringWriter
    apply(n, name, prefix, new PrintWriter(buf))
    buf.getBuffer.toString
  }

  def apply(n: Dumpable, name: String = "", prefix: String = "", out: PrintWriter = new PrintWriter(new OutputStreamWriter(System.out))) {
    def dump(value: Dumpable, prefix: String, name: String, topLevel: Boolean) {
      val di = value.getDumpInfo
      out.println(
        prefix +
        cyan + (if(name.nonEmpty) name + ": " else "") +
        yellow + di.name + (if(di.name.nonEmpty && di.mainInfo.nonEmpty) " " else "") +
        normal + di.mainInfo +
        (if(di.attrInfo.isEmpty) "" else " " + blue + di.attrInfo + normal)
      )
      di.children.foreach { case (name, value) => dump(value, prefix + "  ", name, false) }
    }
    dump(n, prefix, name, true)
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
