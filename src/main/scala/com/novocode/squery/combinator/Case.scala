package com.novocode.squery.combinator

object Case {

  class WhenNode(val left: Node, val right: Node) extends BinaryNode

  abstract class CaseColumn[T](val clauses: List[WhenNode], val elseClause: Node) extends Column[T] {
    def nodeChildren = elseClause :: clauses
  }

  def when[C <: Column[_]](cond: C)(implicit wt: Query.WhereType[C]) = new UntypedWhen(Node(cond))

  class UntypedWhen(cond: Node) {
    def then[B](res: Column[B])(implicit tmb: BaseTypeMapper[B]) =
      new TypedCase[B,B](new WhenNode(cond, Node(res)) :: Nil, tmb, tmb)
    def then[B](res: Column[Option[B]]) = res.typeMapper match {
      case tmt: OptionTypeMapper[_] =>
        new TypedCase[B,Option[B]](new WhenNode(cond, Node(res)) :: Nil, tmt.base, tmt)
    }
  }

  class TypedCase[B,T](clauses: List[WhenNode], val tmb: TypeMapper[B],
                       val tmt: TypeMapper[T]) extends CaseColumn[Option[B]](clauses, ConstColumn.NULL) {
    lazy val typeMapper = tmb.createOptionTypeMapper
    def when[C <: Column[_]](cond: C)(implicit wt: Query.WhereType[C]) = new TypedWhen[B,T](cond, this)
    def otherwise(res: Column[T]): Column[T] = new TypedCaseWithElse[T](clauses, tmt, Node(res))
  }

  class TypedWhen[B,T](cond: Node, parent: TypedCase[B,T]) {
    def then(res: Column[T]) = new TypedCase[B,T](new WhenNode(cond, Node(res)) :: parent.clauses, parent.tmb, parent.tmt)
  }

  class TypedCaseWithElse[T](clauses: List[WhenNode], val typeMapper: TypeMapper[T],
                             elseClause: Node) extends CaseColumn[T](clauses, elseClause)
}
