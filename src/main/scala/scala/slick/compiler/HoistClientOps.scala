package scala.slick.compiler

import scala.slick.ast._
import Util.nodeToNodeOps

/** Lift operations that are preferred to be performed on the client side
  * out of sub-queries. */
class HoistClientOps extends Phase {
  val name = "hoistClientOps"

  def apply(tree: Node, state: CompilationState): Node = {
    val defs = tree.collectAll[(Symbol, Option[(Node, (Node => Node))])] { case d: DefNode =>
      d.nodeGenerators.map { case (s, n) =>
        val u = unwrap(n)
        if(u._1 eq n) (s, None) else (s, Some(u))
      }
    }.collect { case (s, Some(u)) => (s, u) }.toMap
    logger.debug("Unwrappable defs: "+defs)

    lazy val tr: PartialFunction[Node, Node] =  {
      case p @ Path(h :: _) if defs.contains(h) =>
        val (_, wrap) = defs(h)
        wrap(p)
      case d: DefNode => d.nodeMapScopedChildren {
        case (Some(sym), n) if defs.contains(sym) =>
          unwrap(n)._1.replace(tr)
        case (_, n) => n.replace(tr)
      }
    }

    val Comprehension(_, _, _, _, Some(Pure(ProductNode(treeProjChildren))), _, _) = tree
    val base = new AnonSymbol
    val proj = ProductNode(1.to(treeProjChildren.length).map(i => Select(Ref(base), new ElementSymbol(i))))
    val t2 = ResultSetMapping(base, tree, proj)
    t2.replace(tr)
  }

  def unwrap(n: Node): (Node, (Node => Node)) = n match {
    case r @ GetOrElse(ch, default) =>
      val (recCh, recTr) = unwrap(ch)
      (recCh, { sym => GetOrElse(recTr(sym), default).nodeTyped(r.nodeType) })
    case r @ OptionApply(ch) =>
      val (recCh, recTr) = unwrap(ch)
      (recCh, { sym => OptionApply(recTr(sym)).nodeTyped(r.nodeType) })
    case n => (n, identity)
  }
}
