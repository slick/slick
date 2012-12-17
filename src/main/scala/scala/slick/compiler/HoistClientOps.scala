package scala.slick.compiler

import scala.slick.ast._
import Util.nodeToNodeOps

/** Lift operations that are preferred to be performed on the client side
  * out of sub-queries. */
class HoistClientOps extends Phase {
  val name = "hoistClientOps"

  def apply(tree: Node, state: CompilationState): Node = {
    /* Temporarily create a comprehension that selects a StructNode instead of
     * a ProductNode at the top level. This makes the actual hoisting simpler. */
    val withStruct = Phase.fuseComprehensions.ensureStruct(tree.asInstanceOf[Comprehension])
    val Some(Pure(StructNode(ch))) = withStruct.select
    val base = new AnonSymbol
    val proj = ProductNode(ch.map { case (sym, _) => Select(Ref(base), sym) })
    val t2 = ResultSetMapping(base, withStruct, proj)
    val t3 = hoist(t2)
    //TODO: Optimize the case where t3.eq(t2)
    // Change it back to ProductNode form
    val ResultSetMapping(_, newFrom @ Comprehension(_, _, _, _, Some(Pure(StructNode(str))), _, _), newProj) = t3
    val symMap = ch.zipWithIndex.map { case ((sym, _), i) => (sym, ElementSymbol(i+1)) }.toMap
    ResultSetMapping(base, newFrom.copy(select = Some(Pure(ProductNode(str.map(_._2)).withComputedTypeNoRec).withComputedTypeNoRec)),
      newProj.replace { case Select(in, f) if symMap.contains(f) => Select(in, symMap(f)) }).nodeWithComputedType(SymbolScope.empty, false)
  }

  def hoist(tree: Node): Node = {
    logger.debug("Hoisting in:", tree)
    val defs = tree.collectAll[(Symbol, Option[(Node, (Node => Node))])] { case StructNode(ch) =>
      ch.map { case (s, n) =>
        val u = unwrap(n)
        logger.debug("Unwrapped "+n+" to "+u)
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

    tree.replace(tr)
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
