package org.scalaquery.ql

import org.scalaquery.util.{SimpleNode, Node, BinaryNode}

object Case {

  final case class WhenNode(val left: Node, val right: Node) extends BinaryNode {
    protected[this] def nodeRebuild(left: Node, right: Node): Node = copy(left = left, right = right)
  }

  final case class CaseNode(val clauses: IndexedSeq[Node], val elseClause: Node) extends Node {
    protected[this] def nodeChildGenerators = elseClause +: clauses
    def nodeMapChildren(f: Node => Node): Node = {
      val e = f(elseClause)
      val c = nodeMapNodes(clauses, f)
      if(e.ne(elseClause) || c.isDefined) CaseNode(c.getOrElse(clauses), e)
      else this
    }
  }

  def when[C <: Column[_] : CanBeQueryCondition](cond: C) = new UntypedWhen(Node(cond))

  final class UntypedWhen(cond: Node) {
    def then[B : BaseTypeMapper](res: Column[B]) = new TypedCase[B,B](IndexedSeq(new WhenNode(cond, Node(res))))
    def then[B](res: Column[Option[B]]) = res.typeMapper match {
      case tmt: OptionTypeMapper[_] =>
        new TypedCase[B,Option[B]](IndexedSeq(new WhenNode(cond, Node(res))))(tmt.base, tmt)
    }
  }

  final class TypedCase[B : TypeMapper, T : TypeMapper](clauses: IndexedSeq[Node])
  extends Column[Option[B]] {
    def nodeDelegate = CaseNode(clauses, ConstColumn.NULL)
    def when[C <: Column[_] : CanBeQueryCondition](cond: C) = new TypedWhen[B,T](Node(cond), clauses)
    def otherwise(res: Column[T]): Column[T] = new TypedCaseWithElse[T](clauses, Node(res))
  }

  final class TypedWhen[B : TypeMapper, T : TypeMapper](cond: Node, parentClauses: IndexedSeq[Node]) {
    def then(res: Column[T]) = new TypedCase[B,T](new WhenNode(cond, Node(res)) +: parentClauses)
  }

  final class TypedCaseWithElse[T : TypeMapper](clauses: IndexedSeq[Node], elseClause: Node) extends Column[T] {
    def nodeDelegate = CaseNode(clauses, elseClause)
  }
}
