package org.scalaquery.ql.basic

import org.scalaquery.ql._
import org.scalaquery.util.Node

trait BasicImplicitConversions[DriverType <: BasicProfile] {

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
