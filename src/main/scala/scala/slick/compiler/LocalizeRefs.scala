package scala.slick.compiler

import scala.collection.mutable.HashMap
import scala.slick.ast._
import Util._

/** Replace IntrinsicSymbols by AnonSymbols and collect them in a LetDynamic */
class LocalizeRefs extends Phase {
  val name = "localizeRefs"

  def apply(state: CompilerState) = state.map { tree =>
    val map = new HashMap[AnonSymbol, Node]
    val newNodes = new HashMap[AnonSymbol, Node]
    val symbolFor = memoized[IntrinsicSymbol, AnonSymbol] { _ => n => new AnonSymbol }
    lazy val tr: PartialFunction[Node, Node] = {
      case r @ RefNode(i @ IntrinsicSymbol(target)) =>
        val s = symbolFor(i)
        map += s -> target
        newNodes += s -> target
        r.nodeBuildTypedNode(r.nodeWithReference(s), r.nodeType).nodeMapChildren(_.replace(tr))
    }
    val tree2 = tree.replace(tr)
    while(!newNodes.isEmpty) {
      val m = newNodes.toMap
      newNodes.clear()
      m.foreach { case (sym, n) => map += sym -> n.replace(tr) }
    }
    if(map.isEmpty) tree2 else LetDynamic(map.toSeq, tree2)
  }
}
