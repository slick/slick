package com.novocode.squery.combinator.basic

import com.novocode.squery.combinator.{Query, Column, ColumnBase, OptionMapper, AllColumnOps, BooleanColumnOps, StringColumnOps}
import com.novocode.squery.session.{TypeMapper, BaseTypeMapper, NumericType}

trait BasicImplicitConversions[DriverType <: BasicProfile] {

  implicit object WhereBooleanColumn extends Query.WhereType[Column[Boolean]] {
    def apply(value: Column[Boolean], l: List[Column[_]]): List[Column[_]] = value :: l
  }
  implicit object WhereBooleanOptionColumn extends Query.WhereType[Column[Option[Boolean]]] {
    def apply(value: Column[Option[Boolean]], l: List[Column[_]]): List[Column[_]] = value :: l
  }
  implicit object WhereBoolean extends Query.WhereType[Boolean] {
    def apply(value: Boolean, l: List[Column[_]]): List[Column[_]] =
      if(value) l else valueToConstColumn(false)(TypeMapper.BooleanTypeMapper) :: Nil
  }

  implicit def getOptionMapperTT[B1, B2, BR](implicit tm: BaseTypeMapper[B2]) = OptionMapper.plain.asInstanceOf[OptionMapper[B1, B2, BR, B1, B2, BR]]
  implicit def getOptionMapperTO[B1, B2, BR](implicit tm: BaseTypeMapper[B2]) = OptionMapper.option.asInstanceOf[OptionMapper[B1, B2, BR, B1, Option[B2], Option[BR]]]
  implicit def getOptionMapperOT[B1, B2, BR](implicit tm: BaseTypeMapper[B2]) = OptionMapper.option.asInstanceOf[OptionMapper[B1, B2, BR, Option[B1], B2, Option[BR]]]
  implicit def getOptionMapperOO[B1, B2, BR](implicit tm: BaseTypeMapper[B2]) = OptionMapper.option.asInstanceOf[OptionMapper[B1, B2, BR, Option[B1], Option[B2], Option[BR]]]

  implicit def baseColumnToAllColumnOps[B1](c: Column[B1])(implicit tm: BaseTypeMapper[B1]): AllColumnOps[B1, B1] = c match {
    case o: AllColumnOps[_,_] => o.asInstanceOf[AllColumnOps[B1, B1]]
    case _ => new AllColumnOps[B1, B1] { protected[this] val leftOperand = Node(c) }
  }

  implicit def optionColumnToAllColumnOps[B1](c: Column[Option[B1]]): AllColumnOps[B1, Option[B1]] = c match {
    case o: AllColumnOps[_,_] => o.asInstanceOf[AllColumnOps[B1, Option[B1]]]
    case _ => new AllColumnOps[B1, Option[B1]] { protected[this] val leftOperand = Node(c) }
  }

  implicit def columnOfBooleanToBooleanColumnOps(c: ColumnBase[Boolean]): BooleanColumnOps[Boolean] = c match {
    case o: BooleanColumnOps[_] => o.asInstanceOf[BooleanColumnOps[Boolean]]
    case _ => new BooleanColumnOps[Boolean] { protected[this] val leftOperand = Node(c) }
  }

  implicit def columnOfBooleanOptionToBooleanColumnOps(c: ColumnBase[Option[Boolean]]): BooleanColumnOps[Option[Boolean]] = c match {
    case o: BooleanColumnOps[_] => o.asInstanceOf[BooleanColumnOps[Option[Boolean]]]
    case _ => new BooleanColumnOps[Option[Boolean]] { protected[this] val leftOperand = Node(c) }
  }

  implicit def columnOfStringToStringColumnOps(c: ColumnBase[String]): StringColumnOps[String] = c match {
    case o: StringColumnOps[_] => o.asInstanceOf[StringColumnOps[String]]
    case _ => new StringColumnOps[String] { protected[this] val leftOperand = Node(c) }
  }

  implicit def columnOfStringOptionToStringColumnOps(c: ColumnBase[Option[String]]): StringColumnOps[Option[String]] = c match {
    case o: StringColumnOps[_] => o.asInstanceOf[StringColumnOps[Option[String]]]
    case _ => new StringColumnOps[Option[String]] { protected[this] val leftOperand = Node(c) }
  }

  implicit def columnToOptionColumn[T](c: Column[T])(implicit tm: BaseTypeMapper[T]): Column[Option[T]] = c.?

  implicit def valueToConstColumn[T](v: T)(implicit tm: TypeMapper[T]) = new ConstColumn[T](v)(tm)

  implicit def tableToQuery[T <: TableBase[_]](t: T) = Query(t.mapOp(n => new Table.Alias(Node(n))))

  implicit def columnToOrdering(c: Column[_]): Ordering = Ordering.Asc(Node(c))

  implicit def queryToQueryInvoker[T](q: Query[ColumnBase[T]]): BasicQueryInvoker[T] = new BasicQueryInvoker(q, squeryDriver)
  implicit def queryToDeleteInvoker[T](q: Query[Table[T]]): BasicDeleteInvoker[T] = new BasicDeleteInvoker(q, squeryDriver)
  implicit def queryToUpdateInvoker[T](q: Query[Projection[T]]): BasicUpdateInvoker[T] = new BasicUpdateInvoker(q, squeryDriver)
  implicit def tableToDDLInvoker[T](t: Table[T]): BasicDDLInvoker[T] = new BasicDDLInvoker(t, squeryDriver)
  implicit def columnBaseToInsertInvoker[T](c: ColumnBase[T]) = new BasicInsertInvoker(c, squeryDriver)
  implicit def sequenceToSequenceDDLInvoker(seq: Sequence[_]): BasicSequenceDDLInvoker = new BasicSequenceDDLInvoker(seq, squeryDriver)

  implicit def queryToQueryOfColumnBaseOps[E <: ColumnBase[_]](q: Query[E]) = new QueryOfColumnBaseOps(q)
  implicit def queryToQueryOfColumnOps[E <: Column[_]](q: Query[E]) = new QueryOfColumnOps(q)

  implicit val squeryDriver: DriverType
}
