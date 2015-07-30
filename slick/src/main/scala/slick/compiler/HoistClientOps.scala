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
      val CollectionType(cons, NominalType(_, StructType(defs1))) = ss.nodeType
      val base = new AnonSymbol
      val proj = StructNode(defs1.map { case (s, _) => (s, Select(Ref(base), s)) })
      val ResultSetMapping(_, rsmFrom, rsmProj) = hoist(ResultSetMapping(base, ss, proj))
      logger.debug("Hoisted projection:", rsmProj)
      logger.debug("Rewriting remaining DB side:", rsmFrom)
      val rsm2 = ResultSetMapping(base, rewriteDBSide(rsmFrom), rsmProj).infer()
      fuseResultSetMappings(rsm.copy(from = rsm2)).infer()
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
      }, bottomUp = true, keepType = true)
      fuseResultSetMappings(ResultSetMapping(gen2, from2, nmap))
    case n => rsm
  }

  def hoist(tree: Node): Node = {
    logger.debug("Hoisting in:", tree)
    val defs = tree.collectAll[(TermSymbol, Option[(Node => Node)])] { case StructNode(ch) =>
      ch.map { case (s, n) =>
        val u = unwrap(n)
        logger.debug("Unwrapped "+n+" to "+u)
        if(u._1 eq n) (s, None) else (s, Some(u._2))
      }
    }.collect { case (s, Some(u)) => (s, u) }.toMap
    logger.debug("Unwrappable defs: "+defs)

    if(defs.isEmpty) tree else {
      lazy val tr: PartialFunction[Node, Node] =  {
        case p @ Path(elems @ (h :: _)) if defs.contains(h) =>
          defs(h).apply(Path(elems)) // wrap an untyped copy
        case d: DefNode => d.mapScopedChildren {
          case (Some(sym), n) if defs.contains(sym) =>
            unwrap(n)._1.replace(tr)
          case (_, n) => n.replace(tr)
        }
      }
      tree.replace(tr)
    }
  }

  def rewriteDBSide(tree: Node): Node = tree match {
    case GetOrElse(ch, default) =>
      val d = try default() catch {
        case NonFatal(ex) => throw new SlickException(
          "Caught exception while computing default value for Rep[Option[_]].getOrElse -- "+
            "This cannot be done lazily when the value is needed on the database side", ex)
      }
      val ch2 :@ OptionType(tpe) = rewriteDBSide(ch)
      Library.IfNull.typed(tpe, ch2, LiteralNode.apply(tpe, d)).infer()
    case n => n.mapChildren(rewriteDBSide, keepType = true)
  }

  def unwrap(n: Node): (Node, (Node => Node)) = n match {
    case GetOrElse(ch, default) =>
      val (recCh, recTr) = unwrap(ch)
      (recCh, { sym => GetOrElse(recTr(sym), default) })
    case OptionApply(ch) =>
      val (recCh, recTr) = unwrap(ch)
      (recCh, { sym => OptionApply(recTr(sym)) })
    case n => (n, identity)
  }
}
