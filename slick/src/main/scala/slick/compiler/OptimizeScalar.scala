package slick.compiler

import slick.ast.TypeUtil._
import slick.ast.Util._
import slick.ast._
import slick.util.ConstArray

/** Optimize scalar expressions */
class OptimizeScalar extends Phase {
  val name = "optimizeScalar"

  def apply(state: CompilerState) = state.map(_.tree.replace({
    // (if(p) a else b) == v
    case n @ Library.==(IfThenElse(ConstArray(p, Const(a), Const(b))), Const(v)) =>
      val checkTrue = v == a
      val checkFalse = v == b
      val res =
        if(checkTrue && checkFalse) LiteralNode(true)
        else if(checkTrue && !checkFalse) p
        else if(checkFalse) Library.Not.typed(p.nodeType, p)
        else LiteralNode(false)
      cast(n.nodeType, res).infer()

    // if(v != null) v else null
    case n @ IfThenElse(ConstArray(Library.Not(Library.==(v, LiteralNode(null))), v2, LiteralNode(z)))
        if v == v2 && (z == null || z == None) =>
      v

    // Redundant cast to non-nullable within OptionApply
    case o @ OptionApply(Library.SilentCast(n)) if o.nodeType == n.nodeType => n

    // Rownum comparison with offset 1, arising from zipWithIndex
    case n @ Library.<(Library.-(r: RowNumber, LiteralNode(1L)), v) =>
      Library.<=.typed(n.nodeType, r, v).infer()

    // Some(v).getOrElse(_)
    case n @ Library.IfNull(OptionApply(ch), _) =>
      cast(n.nodeType, ch)

  }, keepType = true, bottomUp = true))

  object Const {
    def unapply(n: Node): Option[Node] = n match {
      case _: LiteralNode => Some(n)
      case Apply(Library.SilentCast, ConstArray(ch)) => unapply(ch)
      case OptionApply(ch) => unapply(ch)
      case _ => None
    }
  }

  def cast(tpe: Type, n: Node): Node = {
    val n2 = n.infer()
    if(n2.nodeType == tpe) n2 else Library.SilentCast.typed(tpe, n2)
  }
}
