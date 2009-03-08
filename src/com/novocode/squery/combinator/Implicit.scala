package com.novocode.squery.combinator

object Implicit {
  implicit def columnOfBooleanToBooleanColumn(c: Column[java.lang.Boolean]): BooleanColumn = c match {
    case c: BooleanColumn => c
    case _ => new WrappedColumn(c) with BooleanColumn
  }

  implicit def columnOfIntToIntColumn(c: Column[java.lang.Integer]): IntColumn = c match {
    case c: IntColumn => c
    case _ => new WrappedColumn(c) with IntColumn
  }

  implicit def columnOfStringToStringColumn(c: Column[String]): StringColumn = c match {
    case c: StringColumn => c
    case _ => new WrappedColumn(c) with StringColumn
  }

  implicit def intToConstColumn(v: Int) = new ConstColumn(java.lang.Integer.valueOf(v)) with IntColumn
  implicit def integerToConstColumn(v: java.lang.Integer) = new ConstColumn(v) with IntColumn
  implicit def stringToConstColumn(v: String) = new ConstColumn(v) with StringColumn

  implicit def tableToQuery[T <: TableBase.T_](t: T) = Query(t.withOp(new ColumnOp.BaseTableQueryOp(t)))

  // Not implicit to work around bug #1579
  def queryToSubQuery[C <: Column.T_](q: Query[C]): C = q.value.withOp(ColumnOp.SubQueryOp(q))

  implicit def queryToQueryInvoker[T](q: Query[ConvertibleColumn[T]]): StatementCombinatorQueryInvoker[T] = new StatementCombinatorQueryInvoker(q)
  implicit def queryToDeleteInvoker[T](q: Query[Table[T]]): DeleteInvoker[T] = new DeleteInvoker(q)
  implicit def tableToDDLInvoker[T](t: Table[T]): DDLInvoker[T] = new DDLInvoker(t)
  implicit def convertibleColumnToInsertUpdateInvoker[T](c: ConvertibleColumn[T]) = new CombinatorInsertUpdateInvoker(c)
}
