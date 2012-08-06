package scala.slick.lifted

import scala.slick.ast.{Node, BinaryNode}
import scala.slick.SlickException

object Case {

  final case class WhenNode(val left: Node, val right: Node) extends BinaryNode {
    protected[this] def nodeRebuild(left: Node, right: Node): Node = copy(left = left, right = right)
  }

  final case class CaseNode(val clauses: IndexedSeq[Node], val elseClause: Node) extends Node {
    val nodeChildren = elseClause +: clauses
    def nodeMapChildren(f: Node => Node): Node = {
      val e = f(elseClause)
      val c = nodeMapNodes(clauses, f)
      if(e.ne(elseClause) || c.isDefined) CaseNode(c.getOrElse(clauses), e)
      else this
    }
  }

  def If[C <: Column[_] : CanBeQueryCondition](cond: C) = new UntypedWhen(Node(cond))
  @deprecated("Use If instead of when", "0.10.0-M2")
  def when[C <: Column[_] : CanBeQueryCondition](cond: C) = If(cond)

  final class UntypedWhen(cond: Node) {
    def Then[B : BaseTypeMapper](res: Column[B]) = new TypedCase[B,B](IndexedSeq(new WhenNode(cond, Node(res))))
    @deprecated("Use Then instead of then", "0.10.0-M2")
    def `then`[B : BaseTypeMapper](res: Column[B]) = Then(res)

    def Then[B](res: Column[Option[B]]) = res.typeMapper match {
      case tmt: OptionTypeMapper[_] =>
        new TypedCase[B,Option[B]](IndexedSeq(new WhenNode(cond, Node(res))))(tmt.base, tmt)
      case tm => throw new SlickException("Unexpected non-Option TypeMapper "+tm+" for Option type")
    }
    @deprecated("Use Then instead of then", "0.10.0-M2")
    def `then`[B](res: Column[Option[B]]) = Then(res)
  }

  final class TypedCase[B : TypeMapper, T : TypeMapper](clauses: IndexedSeq[Node])
  extends Column[Option[B]] {
    def nodeDelegate = CaseNode(clauses, ConstColumn.NULL)

    def If[C <: Column[_] : CanBeQueryCondition](cond: C) = new TypedWhen[B,T](Node(cond), clauses)
    @deprecated("Use If instead of when", "0.10.0-M2")
    def when[C <: Column[_] : CanBeQueryCondition](cond: C) = If(cond)

    def Else(res: Column[T]): Column[T] = new TypedCaseWithElse[T](clauses, Node(res))
    @deprecated("Use Else instead of otherwise", "0.10.0-M2")
    def otherwise(res: Column[T]): Column[T] = Else(res)
  }

  final class TypedWhen[B : TypeMapper, T : TypeMapper](cond: Node, parentClauses: IndexedSeq[Node]) {
    def Then(res: Column[T]) = new TypedCase[B,T](new WhenNode(cond, Node(res)) +: parentClauses)
    @deprecated("Use Then instead of then", "0.10.0-M2")
    def `then`(res: Column[T]) = Then(res)
  }

  final class TypedCaseWithElse[T : TypeMapper](clauses: IndexedSeq[Node], elseClause: Node) extends Column[T] {
    def nodeDelegate = CaseNode(clauses, elseClause)
  }
}
