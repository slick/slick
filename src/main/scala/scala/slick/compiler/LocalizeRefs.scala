package scala.slick.compiler

import scala.collection.mutable.HashMap
import scala.slick.ast._

/** Replace IntrinsicSymbols by AnonSymbols and collect them in a LetDynamic */
class LocalizeRefs extends Phase {
  val name = "localizeRefs"

  def apply(state: CompilerState) = state.map { tree =>
    val map = new HashMap[AnonSymbol, Node]
    val newNodes = new HashMap[AnonSymbol, Node]
    val tr = new Transformer {
      def replace = {
        case r @ RefNode(IntrinsicSymbol(target)) =>
          val s = new AnonSymbol
          map += s -> target
          newNodes += s -> target
          r.nodeBuildTypedNode(r.nodeWithReference(s), r.nodeType)
      }
    }
    val tree2 = tr.once(tree)
    while(!newNodes.isEmpty) {
      val m = newNodes.toMap
      newNodes.clear()
      m.foreach { case (sym, n) => map += sym -> tr.once(n) }
    }
    if(map.isEmpty) tree2 else LetDynamic(map.toSeq, tree2)
  }
}
