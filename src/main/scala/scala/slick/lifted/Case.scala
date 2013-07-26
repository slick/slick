package scala.slick.lifted

import scala.slick.ast.{LiteralNode, IfThen, Node, ConditionalExpr, BaseTypedType, OptionTypedType, TypedType}
import scala.slick.SlickException

object Case {

  def If[C <: Column[_] : CanBeQueryCondition](cond: C) = new UntypedWhen(cond.toNode)

  final class UntypedWhen(cond: Node) {
    def Then[B : BaseTypedType](res: Column[B]) = new TypedCase[B,B](IndexedSeq(new IfThen(cond, res.toNode)))

    def Then[B](res: Column[Option[B]]) = res.tpe match {
      case tmt: OptionTypedType[_] =>
        new TypedCase[B,Option[B]](IndexedSeq(new IfThen(cond, res.toNode)))(tmt.elementType, tmt)
      case tm => throw new SlickException("Unexpected non-Option TypedType "+tm+" for Option type")
    }
  }

  final class TypedCase[B : TypedType, T : TypedType](clauses: IndexedSeq[Node])
  extends Column[Option[B]] {
    def toNode = ConditionalExpr(clauses, LiteralNode(null))

    def If[C <: Column[_] : CanBeQueryCondition](cond: C) = new TypedWhen[B,T](cond.toNode, clauses)

    def Else(res: Column[T]): Column[T] = new TypedCaseWithElse[T](clauses, res.toNode)
  }

  final class TypedWhen[B : TypedType, T : TypedType](cond: Node, parentClauses: IndexedSeq[Node]) {
    def Then(res: Column[T]) = new TypedCase[B,T](parentClauses :+ new IfThen(cond, res.toNode))
  }

  final class TypedCaseWithElse[T : TypedType](clauses: IndexedSeq[Node], elseClause: Node) extends Column[T] {
    def toNode = ConditionalExpr(clauses, elseClause)
  }
}
