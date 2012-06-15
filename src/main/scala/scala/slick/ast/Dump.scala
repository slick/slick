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
    for(GlobalSymbol(name, node) <- dc.orphans)
      dc.dump(node, prefix, "/"+name+": ")
    out.flush()
  }
}

class DumpContext(val out: PrintWriter) {
  private[this] val refs = new HashSet[Symbol]
  private[this] val defs = new HashSet[Symbol]

  def addRef(s: Symbol) = refs.add(s)
  def addDef(s: Symbol) = defs.add(s)

  def orphans: Set[Symbol] = {
    val newRefs = new HashSet[GlobalSymbol] ++ refs.collect { case g: GlobalSymbol => g }
    def scan(): Unit = {
      val toScan = newRefs.toSet
      newRefs.clear()
      toScan.foreach { _.target.foreach {
        case r: RefNode =>
          r.nodeReferences.foreach {
            case g: GlobalSymbol =>
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
