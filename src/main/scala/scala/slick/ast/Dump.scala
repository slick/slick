package scala.slick.ast

import java.io.{OutputStreamWriter, StringWriter, PrintWriter}
import scala.collection.mutable.HashSet
import Util.nodeToNodeOps
import scala.sys.BooleanProp

/**
 * Create a readable printout of an AST
 */
object Dump {
  private[ast] val (normal, green, yellow, blue) = {
    if(BooleanProp.valueIsTrue("scala.slick.ansiDump")) ("\u001B[0m", "\u001B[32m", "\u001B[33m", "\u001B[34m")
    else ("", "", "", "")
  }
  private[ast] val dumpPaths: Boolean = BooleanProp.valueIsTrue("scala.slick.dumpPaths")

  def get(n: Node, name: String = "", prefix: String = "") = {
    val buf = new StringWriter
    apply(n, name, prefix, new PrintWriter(buf))
    buf.getBuffer.toString
  }

  def apply(n: Node, name: String = "", prefix: String = "", to: PrintWriter = null, typed: Boolean = true) {
    val out = if(to eq null) new PrintWriter(new OutputStreamWriter(System.out)) else to
    val dc = new DumpContext(out, typed)
    dc.dump(n, prefix, name, true)
    out.flush()
  }
}

class DumpContext(val out: PrintWriter, val typed: Boolean = true) {
  def dump(tree: Node, prefix: String, name: String, topLevel: Boolean) {
    import Dump._
    val tpe = tree.nodePeekType
    val typeInfo = if(typed && tpe != UnassignedType) blue + " : " + tpe.toString + normal else ""
    val start = yellow + prefix + name + normal
    def tl(s: Any) = if(topLevel) green + s + normal else s
    out.println(start + tl(tree) + typeInfo)
    tree match {
      // Omit path details unless dumpPaths is set
      case Path(l @ (_ :: _ :: _)) if !dumpPaths => ()
      case _ =>
        for((chg, n) <- tree.nodeChildren.zip(tree.nodeChildNames))
          dump(chg, prefix + "  ", n+": ", false)
    }
  }
}
