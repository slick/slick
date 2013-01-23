package scala.slick.compiler

import scala.slick.ast._
import Util.nodeToNodeOps
import TypeUtil.typeToTypeUtil
import util.control.NonFatal
import slick.SlickException

/** Lift operations that are preferred to be performed on the client side
  * out of sub-queries. */
class HoistClientOps extends Phase {
  val name = "hoistClientOps"

  def apply(state: CompilerState) = state.map { tree =>
    /* Temporarily create a comprehension that selects a StructNode instead of
     * a ProductNode at the top level. This makes the actual hoisting simpler. */
    val withStruct = Phase.fuseComprehensions.ensureStruct(tree.asInstanceOf[Comprehension])
    val Some(Pure(StructNode(ch))) = withStruct.select
    val base = new AnonSymbol
    val proj = ProductNode(ch.map { case (sym, _) => Select(Ref(base), sym) })
    val t2 = ResultSetMapping(base, withStruct, proj)
    val t3 = hoist(t2)
    val (rsmFrom, rsmProj) =
      if(t3 eq t2) {
        // Use original ProductNode form
        val Comprehension(_, _, _, _, Some(Pure(ProductNode(treeProjChildren))), _, _) = tree
        val idxproj = ProductNode(1.to(treeProjChildren.length).map(i => Select(Ref(base), new ElementSymbol(i))))
        (tree, idxproj)
      } else {
        // Change it back to ProductNode form
        val ResultSetMapping(_, newFrom @ Comprehension(_, _, _, _, Some(Pure(StructNode(str))), _, _), newProj) = t3
        val symMap = ch.zipWithIndex.map { case ((sym, _), i) => (sym, ElementSymbol(i+1)) }.toMap
        (newFrom.copy(select = Some(Pure(ProductNode(str.map(_._2)).withComputedTypeNoRec).withComputedTypeNoRec)),
          newProj.replace { case Select(in, f) if symMap.contains(f) => Select(in, symMap(f)) })
      }
    ResultSetMapping(base, rewriteDBSide(rsmFrom), rsmProj).nodeWithComputedType(SymbolScope.empty, false)
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

    if(defs.isEmpty) tree else {
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
  }

  def rewriteDBSide(tree: Node): Node = tree match {
    case OptionApply(ch) =>
      val ch2 = rewriteDBSide(ch)
      ch2.nodeTypedOrCopy(OptionType(ch2.nodeType))
    case GetOrElse(ch, default) =>
      val ch2 = rewriteDBSide(ch)
      val tpe = ch2.nodeType.asOptionType.elementType
      val d = try default() catch {
        case NonFatal(ex) => throw new SlickException(
          "Caught exception while computing default value for Rep[Option[_]].getOrElse -- "+
            "This cannot be done lazily when the value is needed on the database side", ex)
      }
      Library.IfNull.typed(tpe, ch2, LiteralNode.apply(tpe, d))
    case n => n.nodeMapChildrenKeepType(rewriteDBSide)
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
