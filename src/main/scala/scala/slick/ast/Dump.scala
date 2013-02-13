package scala.slick.ast

import java.io.{OutputStreamWriter, StringWriter, PrintWriter}
import scala.collection.mutable.HashSet
import Util.nodeToNodeOps

/**
 * Create a readable printout of an AST
 */
object Dump {
  def get(n: NodeGenerator, name: String = "", prefix: String = "") = {
    val buf = new StringWriter
    apply(n, name, prefix, new PrintWriter(buf))
    buf.getBuffer.toString
  }

  def apply(n: NodeGenerator, name: String = "", prefix: String = "", to: PrintWriter = null, typed: Boolean = true) {
    val out = if(to eq null) new PrintWriter(new OutputStreamWriter(System.out)) else to
    val dc = new DumpContext(out, typed)
    dc.dump(Node(n), prefix, name)
    for(i <- dc.orphans) dc.dump(i.target, prefix, "/"+i.name+": ")
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
        case r @ RefNode(g: IntrinsicSymbol) if !refs.contains(g) =>
          refs += g
          newRefs += g
        case _ =>
      }}
    }
    while(!newRefs.isEmpty) scan()
    (refs -- defs).toSet
  }

  def dump(tree: Node, prefix: String, name: String) {
    tree match {
      case n: DefNode => n.nodeGenerators.foreach(t => addDef(t._1))
      case RefNode(s) => addRef(s)
      case _ =>
    }
    val tpe = tree match {
      case t: Typed => t.tpe
      case t => t.nodeType
    }
    val typeInfo = if(typed && tpe != UnassignedType) " : " + tpe.toString else ""
    tree match {
      case Path(l @ (_ :: _ :: _)) =>
        // Print paths on a single line
        out.println(prefix + name + Path.toString(l) + typeInfo)
        tree.foreach { case RefNode(s) => addRef(s) }
      case _ =>
        out.println(prefix + name + tree + typeInfo)
        for((chg, n) <- tree.nodeChildren.zip(tree.nodeChildNames))
          dump(Node(chg), prefix + "  ", n+": ")
    }
  }
}
