package scala.slick.ast

import java.io.{OutputStreamWriter, StringWriter, PrintWriter}
import scala.collection.mutable.HashSet
import scala.slick.ast.opt.Util.nodeToNodeOps

/**
 * Create a readable printout of an AST
 */
object Dump {
  def get(n: NodeGenerator, name: String = "", prefix: String = "") = {
    val buf = new StringWriter
    apply(n, name, prefix, new PrintWriter(buf))
    buf.getBuffer.toString
  }

  def apply(n: NodeGenerator, name: String = "", prefix: String = "", to: PrintWriter = null) {
    val out = if(to eq null) new PrintWriter(new OutputStreamWriter(System.out)) else to
    val dc = new DumpContext(out)
    dc.dump(Node(n), prefix, name)
    for(i <- dc.orphans) dc.dump(i.target, prefix, "/"+i.name+": ")
    out.flush()
  }
}

class DumpContext(val out: PrintWriter) {
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
        case r: RefNode =>
          r.nodeReferences.foreach {
            case g: IntrinsicSymbol =>
              if(!refs.contains(g)) {
                refs += g
                newRefs += g
              }
            case _ =>
          }
        case _ =>
      }}
    }
    while(!newRefs.isEmpty) scan()
    (refs -- defs).toSet
  }

  def dump(tree: Node, prefix: String, name: String) {
    tree match {
      case n: DefNode => n.nodeGenerators.foreach(t => addDef(t._1))
      case n: RefNode => n.nodeReferences.foreach(addRef)
      case _ =>
    }
    tree match {
      case Path(l @ (_ :: _ :: _)) =>
        // Print paths on a single line
        out.println(prefix + name + Path.toString(l))
        tree.foreach { case n: RefNode => n.nodeReferences.foreach(addRef) }
      case _ =>
        out.println(prefix + name + tree)
        for((chg, n) <- tree.nodeChildren.zip(tree.nodeChildNames))
          dump(Node(chg), prefix + "  ", n+": ")
    }
  }
}
