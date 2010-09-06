package org.scalaquery.ql

import org.scalaquery.util.{Node, BinaryNode}

object Case {

  class WhenNode(val left: Node, val right: Node) extends BinaryNode

  abstract class CaseColumn[T : TypeMapper](val clauses: List[WhenNode], val elseClause: Node) extends Column[T] {
    def nodeChildren = elseClause :: clauses
  }

  def when[C <: Column[_] : CanBeQueryCondition](cond: C) = new UntypedWhen(Node(cond))

  class UntypedWhen(cond: Node) {
    def then[B : BaseTypeMapper](res: Column[B]) = new TypedCase[B,B](new WhenNode(cond, Node(res)) :: Nil)
    def then[B](res: Column[Option[B]]) = res.typeMapper match {
      case tmt: OptionTypeMapper[_] =>
        new TypedCase[B,Option[B]](new WhenNode(cond, Node(res)) :: Nil)(tmt.base, tmt)
    }
  }

  class TypedCase[B : TypeMapper, T : TypeMapper](clauses: List[WhenNode])
  extends CaseColumn[Option[B]](clauses, ConstColumn.NULL) {
    def when[C <: Column[_] : CanBeQueryCondition](cond: C) = new TypedWhen[B,T](cond, this)
    def otherwise(res: Column[T]): Column[T] = new TypedCaseWithElse[T](clauses, Node(res))
  }

  class TypedWhen[B : TypeMapper, T : TypeMapper](cond: Node, parent: TypedCase[B,T]) {
    def then(res: Column[T]) = new TypedCase[B,T](new WhenNode(cond, Node(res)) :: parent.clauses)
  }

  class TypedCaseWithElse[T : TypeMapper](clauses: List[WhenNode], elseClause: Node) extends CaseColumn[T](clauses, elseClause)
}
