package scala.slick.lifted

import scala.slick.ast.{LiteralNode, IfThen, Node, ConditionalExpr}
import scala.slick.SlickException

object Case {

  def If[C <: Column[_] : CanBeQueryCondition](cond: C) = new UntypedWhen(Node(cond))

  final class UntypedWhen(cond: Node) {
    def Then[B : BaseTypeMapper](res: Column[B]) = new TypedCase[B,B](IndexedSeq(new IfThen(cond, Node(res))))

    def Then[B](res: Column[Option[B]]) = res.typeMapper match {
      case tmt: OptionTypeMapper[_] =>
        new TypedCase[B,Option[B]](IndexedSeq(new IfThen(cond, Node(res))))(tmt.base, tmt)
      case tm => throw new SlickException("Unexpected non-Option TypeMapper "+tm+" for Option type")
    }
  }

  final class TypedCase[B : TypeMapper, T : TypeMapper](clauses: IndexedSeq[Node])
  extends Column[Option[B]] {
    def nodeDelegate = ConditionalExpr(clauses, LiteralNode(null))

    def If[C <: Column[_] : CanBeQueryCondition](cond: C) = new TypedWhen[B,T](Node(cond), clauses)

    def Else(res: Column[T]): Column[T] = new TypedCaseWithElse[T](clauses, Node(res))
  }

  final class TypedWhen[B : TypeMapper, T : TypeMapper](cond: Node, parentClauses: IndexedSeq[Node]) {
    def Then(res: Column[T]) = new TypedCase[B,T](new IfThen(cond, Node(res)) +: parentClauses)
  }

  final class TypedCaseWithElse[T : TypeMapper](clauses: IndexedSeq[Node], elseClause: Node) extends Column[T] {
    def nodeDelegate = ConditionalExpr(clauses, elseClause)
  }
}
