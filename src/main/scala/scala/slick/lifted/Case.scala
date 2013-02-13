package scala.slick.lifted

import scala.slick.ast.{LiteralNode, IfThen, Node, ConditionalExpr, BaseTypedType, OptionTypedType, TypedType}
import scala.slick.SlickException

object Case {

  def If[C <: Column[_] : CanBeQueryCondition](cond: C) = new UntypedWhen(Node(cond))

  final class UntypedWhen(cond: Node) {
    def Then[B : BaseTypedType](res: Column[B]) = new TypedCase[B,B](IndexedSeq(new IfThen(cond, Node(res))))

    def Then[B](res: Column[Option[B]]) = res.tpe match {
      case tmt: OptionTypedType[_] =>
        new TypedCase[B,Option[B]](IndexedSeq(new IfThen(cond, Node(res))))(tmt.elementType, tmt)
      case tm => throw new SlickException("Unexpected non-Option TypedType "+tm+" for Option type")
    }
  }

  final class TypedCase[B : TypedType, T : TypedType](clauses: IndexedSeq[Node])
  extends Column[Option[B]] {
    def nodeDelegate = ConditionalExpr(clauses, LiteralNode(null))

    def If[C <: Column[_] : CanBeQueryCondition](cond: C) = new TypedWhen[B,T](Node(cond), clauses)

    def Else(res: Column[T]): Column[T] = new TypedCaseWithElse[T](clauses, Node(res))
  }

  final class TypedWhen[B : TypedType, T : TypedType](cond: Node, parentClauses: IndexedSeq[Node]) {
    def Then(res: Column[T]) = new TypedCase[B,T](new IfThen(cond, Node(res)) +: parentClauses)
  }

  final class TypedCaseWithElse[T : TypedType](clauses: IndexedSeq[Node], elseClause: Node) extends Column[T] {
    def nodeDelegate = ConditionalExpr(clauses, elseClause)
  }
}
