package slick.compiler

import slick.ast._
import Util._
import TypeUtil._

import scala.collection.mutable

/** Expand multi-column conditional expressions and SilentCasts created by expandSums.
  * Single-column conditionals involving NULL values are optimized away where possible. */
class ExpandConditionals extends Phase {
  val name = "expandConditionals"

  def apply(state: CompilerState) = state.map(expand)

  def expand(n: Node): Node = {
    val invalid = mutable.HashSet.empty[TypeSymbol]
    def invalidate(n: Node): Unit = invalid ++= n.nodeType.collect { case NominalType(ts, _) => ts }

    def tr(n: Node): Node = n.mapChildren(tr, keepType = true) match {
      // Expand multi-column SilentCasts
      case cast @ Library.SilentCast(ch) :@ Type.Structural(ProductType(typeCh)) =>
        invalidate(ch)
        val elems = typeCh.zipWithIndex.map { case (t, idx) => tr(Library.SilentCast.typed(t, ch.select(ElementSymbol(idx+1)).infer()).infer()) }
        ProductNode(elems).infer()
      case Library.SilentCast(ch) :@ Type.Structural(StructType(typeCh)) =>
        invalidate(ch)
        val elems = typeCh.map { case (sym, t) => (sym, tr(Library.SilentCast.typed(t, ch.select(sym).infer()).infer())) }
        StructNode(elems).infer()

      // Optimize trivial SilentCasts
      case Library.SilentCast(v :@ tpe) :@ tpe2 if tpe.structural == tpe2.structural =>
        invalidate(v)
        v
      case Library.SilentCast(Library.SilentCast(ch)) :@ tpe => tr(Library.SilentCast.typed(tpe, ch).infer())
      case Library.SilentCast(LiteralNode(None)) :@ (tpe @ OptionType.Primitive(_)) => LiteralNode(tpe, None).infer()

      // Expand multi-column IfThenElse
      case (cond @ IfThenElse(_)) :@ Type.Structural(ProductType(chTypes)) =>
        val ch = (1 to chTypes.length).map { idx =>
          val sym = ElementSymbol(idx)
          tr(cond.mapResultClauses(n => n.select(sym)).infer())
        }
        ProductNode(ch).infer()
      case (cond @ IfThenElse(_)) :@ Type.Structural(StructType(chTypes)) =>
        val ch = chTypes.map { case (sym, _) =>
          (sym, tr(cond.mapResultClauses(n => n.select(sym)).infer()))
        }
        StructNode(ch).infer()

      // Optimize null-propagating single-column IfThenElse
      case IfThenElse(Seq(Library.==(r, LiteralNode(null)), Library.SilentCast(LiteralNode(None)), c @ Library.SilentCast(r2))) if r == r2 => c

      // Fix Untyped nulls in else clauses
      case cond @ IfThenElse(clauses) if (clauses.last match { case LiteralNode(None) :@ OptionType(ScalaBaseType.nullType) => true; case _ => false }) =>
        cond.copy(clauses.init :+ LiteralNode(cond.nodeType, None))

      // Resolve Selects into ProductNodes and StructNodes
      case Select(ProductNode(ch), ElementSymbol(idx)) => ch(idx-1)
      case Select(StructNode(ch), sym) => ch.find(_._1 == sym).get._2

      case n2 @ Pure(_, ts) if n2 ne n =>
        invalid += ts
        n2

      case n => n
    }

    val n2 = tr(n)
    logger.debug("Invalidated TypeSymbols: "+invalid.mkString(", "))
    n2.replace({
      case n @ (_: Ref | _: Select) :@ tpe if invalid.intersect(tpe.collect { case NominalType(ts, _) => ts }.toSet).nonEmpty =>
        n.untyped
    }, bottomUp = true).infer()
  }
}
