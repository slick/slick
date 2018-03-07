package slick.compiler

import slick.ast._
import Util._
import TypeUtil._

/** Remove unreferenced fields from StructNodes. */
class PruneProjections extends Phase {
  val name = "pruneProjections"

  def apply(state: CompilerState) = state.map { n => ClientSideOp.mapServerSide(n, true) { n =>
    val referenced = n.collect[(TypeSymbol, TermSymbol)] { case Select(_ :@ NominalType(s, _), f) => (s, f) }.toSet
    val allTSyms = n.collect[TypeSymbol] { case p: Pure => p.identity }.toSet
    val unrefTSyms = allTSyms -- referenced.map(_._1)
    logger.debug(s"Unreferenced: ${unrefTSyms.mkString(", ")}; Field refs: ${referenced.mkString(", ")}")
    n.replaceInvalidate {
      case Pure(s @ StructNode(ch), pts) if !unrefTSyms.contains(pts) =>
        val ch2 = ch.filter(d => referenced.contains((pts, d._1)))
        if(ch2.length == ch.length) null else (Pure(StructNode(ch2), pts), pts)
    }.infer()
  }}
}
