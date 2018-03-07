package slick.compiler

import slick.ast.Util._
import slick.ast._
import slick.util.ConstArray

/** Optimize scalar expressions */
class OptimizeScalar extends Phase {
  val name = "optimizeScalar"

  def apply(state: CompilerState) = state.map(_.tree.replace({
    case n @ Library.==(IfThenElse(ConstArray(p, Const(a), Const(b))), LiteralNode(null)) =>
      logger.debug("Optimizing: (if(p) a else b).isNull", n)
      val (checkTrue, checkFalse) = (a.isEmpty, b.isEmpty)
      logger.debug(s"a=$a, b=$b")
      val res =
        if(checkTrue && checkFalse) LiteralNode(true)
        else if(checkTrue && !checkFalse) p
        else if(checkFalse) Library.Not.typed(p.nodeType, p)
        else LiteralNode(false)
      cast(n.nodeType, res).infer()

    case n @ IfThenElse(ConstArray(Library.Not(Library.==(v, LiteralNode(null))), v2, LiteralNode(z)))
      if v == v2 && (z == null || z == None) =>
      logger.debug("Optimizing: if(v != null) v else null", n)
      v

    case n @ IfThenElse(ConstArray(Library.Not(LiteralNode(false)), v, _)) =>
      logger.debug("Optimizing: if(!false) v else _", n)
      v

    case o @ OptionApply(Library.SilentCast(n)) if o.nodeType == n.nodeType =>
      logger.debug("Optimizing: Redundant cast to non-nullable within OptionApply", o)
      n

    case n @ Library.<(Library.-(r: RowNumber, LiteralNode(1L)), v) =>
      logger.debug("Optimizing: Rownum comparison with offset 1, arising from zipWithIndex", n)
      Library.<=.typed(n.nodeType, r, v).infer()

    case n @ Library.IfNull(OptionApply(ch), _) =>
      logger.debug("Optimizing: Some(v).getOrElse(_)", n)
      cast(n.nodeType, ch)

    case n: Comprehension if n.where == Some(LiteralNode(true)) =>
      logger.debug("Optimizing: WHERE TRUE", n)
      n.copy(where = None) :@ n.nodeType

  }, keepType = true, bottomUp = true))

  object Const {
    def unapply(n: Node): Option[Option[Any]] = n match {
      case LiteralNode(null) => Some(None)
      case LiteralNode(v) =>
        Some(if(n.nodeType.structural.isInstanceOf[OptionType]) v.asInstanceOf[Option[Any]] else Some(v))
      case Apply(Library.SilentCast, ConstArray(ch)) => unapply(ch)
      case OptionApply(ch) => unapply(ch).map(_.map(Option.apply _))
      case _ => None
    }
  }

  def cast(tpe: Type, n: Node): Node = {
    val n2 = n.infer()
    if(n2.nodeType == tpe) n2 else Library.SilentCast.typed(tpe, n2)
  }
}
