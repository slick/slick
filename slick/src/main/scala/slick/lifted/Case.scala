package slick.lifted

import slick.ast.{LiteralNode, IfThenElse, Node, BaseTypedType, OptionType, TypedType, OptionApply}
import slick.util.ConstArray

/** `Case` provides a DSL for conditional statements in the query language.
  * An arbitrary number of `If`...`Then` expressions can be chained, optionally
  * followed by `Else`, e.g.:
  * {{{
  *   Case If u.id < 3 Then "low" If u.id < 6 Then "medium" Else "high"
  * }}}
  * All result expressions have to be of compatible type (modulo nullability).
  * If at least one of them is an `Option` type or the `Else` branch is
  * missing, the result is also an `Option`.  */
object Case {

  def If[C <: Rep[_] : CanBeQueryCondition](cond: C) = new UntypedWhen(cond.toNode)

  final class UntypedWhen(cond: Node) {
    def Then[P, B](res: Rep[P])(implicit om: OptionMapperDSL.arg[B, P]#to[B, P], bType: BaseTypedType[B]) =
      new TypedCase[B, P](ConstArray(cond, res.toNode))(bType, om.liftedType(bType))
  }

  final class TypedCase[B : TypedType, T : TypedType](clauses: ConstArray[Node]) extends Rep.TypedRep[Option[B]] {
    def toNode: IfThenElse = {
      val cl =
        if(implicitly[TypedType[T]].isInstanceOf[OptionType]) clauses
        else clauses.zipWithIndex.map { case (n, i) => if(i % 2 == 0) n else OptionApply(n) }
      IfThenElse(cl :+ LiteralNode(null))
    }
    def If[C <: Rep[_] : CanBeQueryCondition](cond: C) = new TypedWhen[B,T](cond.toNode, clauses)
    def Else(res: Rep[T]): Rep[T] = Rep.forNode(IfThenElse(clauses :+ res.toNode))
  }

  final class TypedWhen[B : TypedType, T : TypedType](cond: Node, parentClauses: ConstArray[Node]) {
    def Then(res: Rep[T]) = new TypedCase[B,T](parentClauses ++ ConstArray(cond, res.toNode))
  }
}
