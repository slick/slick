package scala.slick.compiler

import scala.collection.mutable.HashMap
import scala.slick.ast._
import Util._

/** Replace IntrinsicSymbols by AnonSymbols and collect them in a LetDynamic */
class LocalizeRefs extends Phase {
  val name = "localizeRefs"

  def apply(state: CompilerState) = state.map { tree =>
    val map = new HashMap[AnonSymbol, Node]
    lazy val symbolFor = memoized[IntrinsicSymbol, AnonSymbol] { _ => i =>
      val s = new AnonSymbol
      map += s -> i.target.replace(tr)
      s
    }
    lazy val tr: PartialFunction[Node, Node] = {
      case r @ Ref(i: IntrinsicSymbol) => Ref(symbolFor(i)).nodeTyped(r.nodeType)
    }
    val tree2 = tree.replace(tr)
    if(map.isEmpty) tree2 else LetDynamic(map.toSeq, tree2)
  }
}
