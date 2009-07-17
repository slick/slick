package com.novocode.squery.combinator

import com.novocode.squery.session.TypeMapper

object Implicit {

  implicit object WhereBoolean extends Query.WhereType[Boolean]
  implicit object WhereBooleanOption extends Query.WhereType[Option[Boolean]]

  implicit def getOptionMapperTT[T] = OptionMapper.plain.asInstanceOf[OptionMapper[T, T, T, T]]
  implicit def getOptionMapperTO[T] = OptionMapper.option.asInstanceOf[OptionMapper[T, T, Option[T], Option[T]]]
  implicit def getOptionMapperOT[T] = OptionMapper.option.asInstanceOf[OptionMapper[T, Option[T], T, Option[T]]]
  implicit def getOptionMapperOO[T] = OptionMapper.option.asInstanceOf[OptionMapper[T, Option[T], Option[T], Option[T]]]

  implicit def columnOfBooleanToBooleanColumnOps[P1](c: Column[P1]): BooleanColumnOps[P1] = c match {
    case o: BooleanColumnOps[P1] => o
    case _ => new BooleanColumnOps[P1] { protected[this] val leftOperand = Node(c) }
  }

  implicit def columnToSimpleColumn[T](c: Column[T])(implicit tm: TypeMapper[T]): SimpleColumn[T] = c match {
    case t: SimpleColumn[T] => t
    case _ => new WrappedColumn(c, tm)
  }

  implicit def columnToOptionColumn[T](c: Column[T])(implicit tm: TypeMapper[T]): SimpleColumn[Option[T]] =
    columnToSimpleColumn(c)(tm).?

  implicit def valueToConstColumn[T](v: T)(implicit tm: TypeMapper[T]) = new ConstColumn[T](v)(tm)

  implicit def tableToQuery[T <: TableBase.T_](t: T) = Query(t.mapOp(n => new Table.Alias(Node(n))))

  // Not implicit to work around bug #1579
  def queryToSubQuery[C <: Column.T_](q: Query[C]): C = q.value.mapOp(_ => q)

  implicit def queryToQueryInvoker[T](q: Query[ConvertibleColumn[T]]): StatementCombinatorQueryInvoker[T] = new StatementCombinatorQueryInvoker(q)
  implicit def queryToDeleteInvoker[T](q: Query[Table[T]]): DeleteInvoker[T] = new DeleteInvoker(q)
  implicit def queryToUpdateInvoker[T](q: Query[Projection[T]]): CombinatorUpdateInvoker[T] = new CombinatorUpdateInvoker(q)
  implicit def tableToDDLInvoker[T](t: Table[T]): DDLInvoker[T] = new DDLInvoker(t)
  implicit def convertibleColumnToInsertInvoker[T](c: ConvertibleColumn[T]) = new CombinatorInsertInvoker(c)

  implicit def queryToQueryOfColumnOps[E <: Column.T_](q: Query[E]) = new QueryOfColumnOps(q)
}
