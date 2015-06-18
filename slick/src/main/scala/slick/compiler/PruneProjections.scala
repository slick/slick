package slick.compiler

import slick.ast._
import Util._
import TypeUtil._

/** Remove unreferenced fields from StructNodes. */
class PruneProjections extends Phase {
  val name = "pruneProjections"

  def apply(state: CompilerState) = state.map { n => ClientSideOp.mapServerSide(n, true) { n =>
    val referenced = n.collect[(TypeSymbol, TermSymbol)] { case Select(_ :@ NominalType(s, _), f) => (s, f) }.toSet
    val allTSyms = n.collect[TypeSymbol] { case Pure(_, _) :@ CollectionType(_, NominalType(ts, _)) => ts }.toSet
    val unrefTSyms = allTSyms -- referenced.map(_._1)
    logger.debug(s"Unreferenced: ${unrefTSyms.mkString(", ")}; Field refs: ${referenced.mkString(", ")}")
    def tr(n: Node): Node =  n.replace {
      case (p @ Pure(s @ StructNode(ch), pts)) :@ CollectionType(_, NominalType(ts, _)) =>
        val ch2 = ch.collect { case (sym, n) if unrefTSyms.contains(ts) || referenced.contains((ts, sym)) => (sym, tr(n)) }
        if(ch2 == ch) p else Pure(StructNode(ch2), pts)
    }
    tr(n).infer()
  }}
}
