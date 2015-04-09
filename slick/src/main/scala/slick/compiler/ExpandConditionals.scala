package slick.compiler

import slick.ast._
import Util._
import TypeUtil._

/** Expand multi-column conditional expressions and SilentCasts created by expandSums.
  * Single-column conditionals involving NULL values are optimized away where possible. */
class ExpandConditionals extends Phase {
  val name = "expandConditionals"

  def apply(state: CompilerState) = state.map(tr)

  def tr(n: Node): Node = n.nodeMapChildren(tr, keepType = true) match {
    // Expand multi-column SilentCasts
    case cast @ Library.SilentCast(ch) :@ Type.Structural(ProductType(typeCh)) =>
      val elems = typeCh.zipWithIndex.map { case (t, idx) => tr(Library.SilentCast.typed(t, ch.select(ElementSymbol(idx+1)).nodeWithComputedType())) }
      ProductNode(elems).nodeWithComputedType()
    case Library.SilentCast(ch) :@ Type.Structural(StructType(typeCh)) =>
      val elems = typeCh.map { case (sym, t) => (sym, tr(Library.SilentCast.typed(t, ch.select(sym).nodeWithComputedType()))) }
      StructNode(elems).nodeWithComputedType()

    // Optimize trivial SilentCasts
    case Library.SilentCast(v :@ tpe) :@ tpe2 if tpe.structural == tpe2.structural => v
    case Library.SilentCast(Library.SilentCast(ch)) :@ tpe => tr(Library.SilentCast.typed(tpe, ch))
    case Library.SilentCast(LiteralNode(None)) :@ (tpe @ OptionType.Primitive(_)) => LiteralNode(tpe, None)

    // Expand multi-column IfThenElse
    case (cond @ IfThenElse(_)) :@ Type.Structural(ProductType(chTypes)) =>
      val ch = (1 to chTypes.length).map { idx =>
        val sym = ElementSymbol(idx)
        tr(cond.mapResultClauses(n => n.select(sym)).nodeWithComputedType())
      }
      ProductNode(ch).nodeWithComputedType()
    case (cond @ IfThenElse(_)) :@ Type.Structural(StructType(chTypes)) =>
      val ch = chTypes.map { case (sym, _) =>
        (sym, tr(cond.mapResultClauses(n => n.select(sym)).nodeWithComputedType()))
      }
      StructNode(ch).nodeWithComputedType()

    // Optimize null-propagating single-column IfThenElse
    case IfThenElse(Seq(Library.==(r, LiteralNode(null)), Library.SilentCast(LiteralNode(None)), c @ Library.SilentCast(r2))) if r == r2 => c

    // Fix Untyped nulls in else clauses
    case cond @ IfThenElse(clauses) if (clauses.last match { case LiteralNode(None) :@ OptionType(ScalaBaseType.nullType) => true; case _ => false }) =>
      cond.copy(clauses.init :+ LiteralNode(cond.nodeType, None))

    // Resolve Selects into ProductNodes and StructNodes
    case Select(ProductNode(ch), ElementSymbol(idx)) => ch(idx-1)
    case Select(StructNode(ch), sym) => ch.find(_._1 == sym).get._2

    case n => n
  }
}
