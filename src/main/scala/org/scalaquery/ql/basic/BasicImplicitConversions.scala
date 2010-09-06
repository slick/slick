package org.scalaquery.ql.basic

import org.scalaquery.ql._
import org.scalaquery.util.Node

trait BasicImplicitConversions[DriverType <: BasicProfile] {

  implicit object BooleanColumnCanBeQueryCondition extends CanBeQueryCondition[Column[Boolean]] {
    def apply(value: Column[Boolean], l: List[Column[_]]): List[Column[_]] = value :: l
  }
  implicit object BooleanOptionColumnCanBeQueryCondition extends CanBeQueryCondition[Column[Option[Boolean]]] {
    def apply(value: Column[Option[Boolean]], l: List[Column[_]]): List[Column[_]] = value :: l
  }
  implicit object BooleanCanBeQueryCondition extends CanBeQueryCondition[Boolean] {
    def apply(value: Boolean, l: List[Column[_]]): List[Column[_]] =
      if(value) l else valueToConstColumn(false)(TypeMapper.BooleanTypeMapper) :: Nil
  }

  implicit def getOptionMapper2TT[B1, B2 : BaseTypeMapper, BR] = OptionMapper2.plain .asInstanceOf[OptionMapper2[B1, B2, BR, B1,         B2,         BR]]
  implicit def getOptionMapper2TO[B1, B2 : BaseTypeMapper, BR] = OptionMapper2.option.asInstanceOf[OptionMapper2[B1, B2, BR, B1,         Option[B2], Option[BR]]]
  implicit def getOptionMapper2OT[B1, B2 : BaseTypeMapper, BR] = OptionMapper2.option.asInstanceOf[OptionMapper2[B1, B2, BR, Option[B1], B2,         Option[BR]]]
  implicit def getOptionMapper2OO[B1, B2 : BaseTypeMapper, BR] = OptionMapper2.option.asInstanceOf[OptionMapper2[B1, B2, BR, Option[B1], Option[B2], Option[BR]]]

  implicit def getOptionMapper3TTT[B1, B2 : BaseTypeMapper, B3 : BaseTypeMapper, BR] = OptionMapper3.plain .asInstanceOf[OptionMapper3[B1, B2, B3, BR, B1,         B2,         B3,         BR]]
  implicit def getOptionMapper3TTO[B1, B2 : BaseTypeMapper, B3 : BaseTypeMapper, BR] = OptionMapper3.option.asInstanceOf[OptionMapper3[B1, B2, B3, BR, B1,         B2,         Option[B3], Option[BR]]]
  implicit def getOptionMapper3TOT[B1, B2 : BaseTypeMapper, B3 : BaseTypeMapper, BR] = OptionMapper3.option.asInstanceOf[OptionMapper3[B1, B2, B3, BR, B1,         Option[B2], B3,         Option[BR]]]
  implicit def getOptionMapper3TOO[B1, B2 : BaseTypeMapper, B3 : BaseTypeMapper, BR] = OptionMapper3.option.asInstanceOf[OptionMapper3[B1, B2, B3, BR, B1,         Option[B2], Option[B3], Option[BR]]]
  implicit def getOptionMapper3OTT[B1, B2 : BaseTypeMapper, B3 : BaseTypeMapper, BR] = OptionMapper3.option.asInstanceOf[OptionMapper3[B1, B2, B3, BR, Option[B1], B2,         B3,         Option[BR]]]
  implicit def getOptionMapper3OTO[B1, B2 : BaseTypeMapper, B3 : BaseTypeMapper, BR] = OptionMapper3.option.asInstanceOf[OptionMapper3[B1, B2, B3, BR, Option[B1], B2,         Option[B3], Option[BR]]]
  implicit def getOptionMapper3OOT[B1, B2 : BaseTypeMapper, B3 : BaseTypeMapper, BR] = OptionMapper3.option.asInstanceOf[OptionMapper3[B1, B2, B3, BR, Option[B1], Option[B2], B3,         Option[BR]]]
  implicit def getOptionMapper3OOO[B1, B2 : BaseTypeMapper, B3 : BaseTypeMapper, BR] = OptionMapper3.option.asInstanceOf[OptionMapper3[B1, B2, B3, BR, Option[B1], Option[B2], Option[B3], Option[BR]]]

  implicit def baseColumnToColumnOps[B1 : BaseTypeMapper](c: Column[B1]): ColumnOps[B1, B1] = c match {
    case o: ColumnOps[_,_] => o.asInstanceOf[ColumnOps[B1, B1]]
    case _ => new ColumnOps[B1, B1] { protected[this] val leftOperand = Node(c) }
  }

  implicit def optionColumnToColumnOps[B1](c: Column[Option[B1]]): ColumnOps[B1, Option[B1]] = c match {
    case o: ColumnOps[_,_] => o.asInstanceOf[ColumnOps[B1, Option[B1]]]
    case _ => new ColumnOps[B1, Option[B1]] { protected[this] val leftOperand = Node(c) }
  }

  implicit def columnToOptionColumn[T : BaseTypeMapper](c: Column[T]): Column[Option[T]] = c.?

  implicit def valueToConstColumn[T : TypeMapper](v: T) = new ConstColumn[T](v)

  implicit def tableToQuery[T <: TableBase[_]](t: T) = Query(t.mapOp(n => new AbstractTable.Alias(Node(n))))

  implicit def columnToOrdering(c: Column[_]): Ordering = Ordering.Asc(Node(c))

  implicit def queryToQueryInvoker[T](q: Query[ColumnBase[T]]): BasicQueryInvoker[T] = new BasicQueryInvoker(q, scalaQueryDriver)
  implicit def queryToDeleteInvoker[T](q: Query[BasicTable[T]]): BasicDeleteInvoker[T] = new BasicDeleteInvoker(q, scalaQueryDriver)
  implicit def productQueryToUpdateInvoker[T <: Product](q: Query[Projection[T]]): BasicUpdateInvoker[T] = new BasicUpdateInvoker(q, scalaQueryDriver)
  implicit def namedColumnQueryToUpdateInvoker[T](q: Query[NamedColumn[T]]): BasicUpdateInvoker[T] = new BasicUpdateInvoker(q, scalaQueryDriver)
  implicit def columnBaseToInsertInvoker[T](c: ColumnBase[T]) = new BasicInsertInvoker(c, scalaQueryDriver)

  implicit val scalaQueryDriver: DriverType
}
