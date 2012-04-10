package org.scalaquery.ql.basic

import org.scalaquery.ql._
import org.scalaquery.ast.Node

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

  implicit def tableToQuery[T <: AbstractTable[_]](t: T) = Query[T, TableNothing, T](t)(Packing.unpackTable)

  implicit def columnToOrdered[T](c: Column[T]): ColumnOrdered[T] = c.asc

  implicit def queryToQueryInvoker[T, U](q: Query[T, _ <: U]): BasicQueryInvoker[T, U] = new BasicQueryInvoker(q, scalaQueryDriver)
  implicit def queryToDeleteInvoker(q: Query[_ <: AbstractBasicTable[_], _]): BasicDeleteInvoker = new BasicDeleteInvoker(q, scalaQueryDriver)

  // We should really constrain the 2nd type parameter of Query but that won't
  // work for queries on implicitly lifted tables. This conversion is needed
  // for mapped tables.
  implicit def tableQueryToUpdateInvoker[T](q: Query[_ <: AbstractBasicTable[T], _]): BasicUpdateInvoker[T] = new BasicUpdateInvoker(q.asInstanceOf[Query[AbstractBasicTable[T], T]], scalaQueryDriver)

  // This conversion only works for fully packed types
  implicit def productQueryToUpdateInvoker[T](q: Query[_ <: ColumnBase[T], T]): BasicUpdateInvoker[T] = new BasicUpdateInvoker(q, scalaQueryDriver)

  implicit def columnBaseToInsertInvoker[T](c: ColumnBase[T]) = new BasicInsertInvoker(Unpackable.unpackableValueToUnpackable(c), scalaQueryDriver)
  implicit def unpackableToInsertInvoker[T, U](u: Unpackable[T, U]) = new BasicInsertInvoker(u, scalaQueryDriver)

  implicit val scalaQueryDriver: DriverType

  // Work-around for SI-3346
  implicit def anyToToUnpackable[T](value: T) = new ToUnpackable[T](value)
}
