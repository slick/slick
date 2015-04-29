package slick.compiler

import scala.util.control.NonFatal
import slick.{SlickTreeException, SlickException}
import slick.ast._
import Util._
import TypeUtil._

/** Lift operations that are preferred to be performed on the client side
  * out of sub-queries. */
class HoistClientOps extends Phase {
  val name = "hoistClientOps"

  def apply(state: CompilerState) = state.map { tree =>
    ClientSideOp.mapResultSetMapping(tree, false) { case rsm @ ResultSetMapping(_, ss, _) =>
      val (comp, cons) = ss match {
        case CollectionCast(comp: Comprehension, cons) => (comp, Some(cons))
        case comp: Comprehension => (comp, None)
        case n => throw new SlickTreeException("Expected Comprehension at top level of ResultSetMapping", rsm, mark = (_ eq n))
      }
      val Some(Pure(StructNode(defs1), _)) = comp.select
      val base = new AnonSymbol
      val proj = StructNode(defs1.map { case (s, _) => (s, Select(Ref(base), s)) })
      val ResultSetMapping(_, rsmFrom, rsmProj) = hoist(ResultSetMapping(base, comp, proj))
      val rsm2 = ResultSetMapping(base, rewriteDBSide(rsmFrom), rsmProj).nodeWithComputedType(SymbolScope.empty, false, true)
      val rsm3 = fuseResultSetMappings(rsm.copy(from = rsm2)).nodeWithComputedType(retype = true)
      cons match {
        case Some(cons) =>
          rsm3 :@ CollectionType(cons, rsm3.nodeType.asCollectionType.elementType)
        case None => rsm3
      }
    }
  }

  /** Fuse nested ResultSetMappings. Only the outer one may contain nested
    * structures. Inner ResultSetMappings must produce a linearized
    * ProductNode. */
  def fuseResultSetMappings(rsm: ResultSetMapping): ResultSetMapping = rsm.from match {
    case ResultSetMapping(gen2, from2, StructNode(ch2)) =>
      logger.debug("Fusing ResultSetMapping:", rsm)
      val ch2m = ch2.toMap
      val nmap = rsm.map.replace({
        case Select(Ref(sym), ElementSymbol(idx)) if sym == rsm.generator => ch2(idx-1)._2
        case Select(Ref(sym), f) if sym == rsm.generator => ch2m(f)
        case n @ Library.SilentCast(ch :@ tpe2) :@ tpe =>
          if(tpe.structural == tpe2.structural) ch else {
            logger.debug(s"SilentCast cannot be elided: $tpe != $tpe2")
            n
          }
      }, bottomUp = true)
      fuseResultSetMappings(ResultSetMapping(gen2, from2, nmap))
    case n => rsm
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
    //case CollectionCast(ch, _) :@ tpe =>
    //  rewriteDBSide(ch).nodeTypedOrCopy(tpe)
    case GetOrElse(ch, default) =>
      val ch2 = rewriteDBSide(ch)
      val tpe = ch2.nodeType.asOptionType.elementType
      val d = try default() catch {
        case NonFatal(ex) => throw new SlickException(
          "Caught exception while computing default value for Rep[Option[_]].getOrElse -- "+
            "This cannot be done lazily when the value is needed on the database side", ex)
      }
      Library.IfNull.typed(tpe, ch2, LiteralNode.apply(tpe, d))
    case n => n.nodeMapChildren(rewriteDBSide, keepType = true)
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
