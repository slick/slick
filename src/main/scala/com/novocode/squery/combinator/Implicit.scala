package com.novocode.squery.combinator

import com.novocode.squery.session.TypeMapper

object Implicit {
  implicit def columnOfBooleanToBooleanColumnOps(c: Column[Boolean]): BooleanColumnOps = c match {
    case o: BooleanColumnOps => o
    case _ => new BooleanColumnOps { protected[this] val leftOperand = Node(c) }
  }

  implicit def valueToConstColumn[T](v: T)(implicit tm: TypeMapper[T]) = new ConstColumn[T](v)(tm)

  implicit def tableToQuery[T <: TableBase.T_](t: T) = Query(t.withOp(new Table.Alias(Node(t))))

  // Not implicit to work around bug #1579
  def queryToSubQuery[C <: Column.T_](q: Query[C]): C = q.value.withOp(q)

  implicit def queryToQueryInvoker[T](q: Query[ConvertibleColumn[T]]): StatementCombinatorQueryInvoker[T] = new StatementCombinatorQueryInvoker(q)
  implicit def queryToDeleteInvoker[T](q: Query[Table[T]]): DeleteInvoker[T] = new DeleteInvoker(q)
  implicit def queryToUpdateInvoker[T](q: Query[Projection[T]]): CombinatorUpdateInvoker[T] = new CombinatorUpdateInvoker(q)
  implicit def tableToDDLInvoker[T](t: Table[T]): DDLInvoker[T] = new DDLInvoker(t)
  implicit def convertibleColumnToInsertInvoker[T](c: ConvertibleColumn[T]) = new CombinatorInsertInvoker(c)

  implicit def queryToQueryOfColumnOps[E <: Column.T_](q: Query[E]) = new QueryOfColumnOps(q)
}
