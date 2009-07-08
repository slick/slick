package com.novocode.squery.combinator

object Implicit {
  implicit def columnOfBooleanToBooleanColumn(c: Column[Boolean]): BooleanColumn = c match {
    case c: BooleanColumn => c
    case s: SimpleColumn[Boolean] => new WrappedColumn(c) with BooleanColumn { override def nullValue = s.nullValue }
    case _ => new WrappedColumn(c) with BooleanColumn
  }

  implicit def columnOfIntToIntColumn(c: Column[Int]): IntColumn = c match {
    case c: IntColumn => c
    case s: SimpleColumn[Int] => new WrappedColumn(c) with IntColumn { override def nullValue = s.nullValue }
    case _ => new WrappedColumn(c) with IntColumn
  }

  implicit def columnOfStringToStringColumn(c: Column[String]): StringColumn = c match {
    case c: StringColumn => c
    case s: SimpleColumn[String] => new WrappedColumn(c) with StringColumn { override def nullValue = s.nullValue }
    case _ => new WrappedColumn(c) with StringColumn
  }

  implicit def intToConstColumn(v: Int) = new ConstColumn(v) with IntColumn
  implicit def booleanToConstColumn(v: Boolean) = new ConstColumn(v) with BooleanColumn
  implicit def stringToConstColumn(v: String) = new ConstColumn(v) with StringColumn

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
