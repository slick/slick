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
    for(i <- dc.orphans) dc.dump(i.target, prefix, "/"+i.name+": ", true)
    out.flush()
  }
}

class DumpContext(val out: PrintWriter, val typed: Boolean = true) {
  private[this] val refs = new HashSet[IntrinsicSymbol]
  private[this] val defs = new HashSet[IntrinsicSymbol]

  def addRef(s: Symbol) = s match {
    case s: IntrinsicSymbol => refs.add(s)
    case _ =>
  }
  def addDef(s: Symbol) = s match {
    case s: IntrinsicSymbol => defs.add(s)
    case _ =>
  }

  def orphans: Set[IntrinsicSymbol] = {
    val newRefs = new HashSet[IntrinsicSymbol] ++ refs.collect { case g: IntrinsicSymbol => g }
    def scan(): Unit = {
      val toScan = newRefs.toSet
      newRefs.clear()
      toScan.foreach { _.target.foreach {
        case Ref(g: IntrinsicSymbol) if !refs.contains(g) =>
          refs += g
          newRefs += g
        case _ =>
      }}
    }
    while(!newRefs.isEmpty) scan()
    (refs -- defs).toSet
  }

  def dump(tree: Node, prefix: String, name: String, topLevel: Boolean) {
    import Dump._
    tree match {
      case n: DefNode => n.nodeGenerators.foreach(t => addDef(t._1))
      case Ref(s) => addRef(s)
      case _ =>
    }
    val tpe = tree.nodePeekType
    val typeInfo = if(typed && tpe != UnassignedType) blue + " : " + tpe.toString + normal else ""
    val start = yellow + prefix + name + normal
    def tl(s: Any) = if(topLevel) green + s + normal else s
    out.println(start + tl(tree) + typeInfo)
    tree match {
      // Omit path details unless dumpPaths is set
      case Path(l @ (_ :: _ :: _)) if !dumpPaths =>
        tree.foreach {
          case Ref(s) => addRef(s)
          case _ =>
        }
      case _ =>
        for((chg, n) <- tree.nodeChildren.zip(tree.nodeChildNames))
          dump(chg, prefix + "  ", n+": ", false)
    }
  }
}
