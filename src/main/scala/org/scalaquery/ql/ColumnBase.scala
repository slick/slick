package org.scalaquery.ql

import org.scalaquery.SQueryException
import org.scalaquery.ql.basic.BasicProfile
import org.scalaquery.session.{PositionedResult, PositionedParameters}
import org.scalaquery.util.{Node, WithOp}

/**
 * Common base trait for columns, tables and projections (but not unions and joins).
 */
trait ColumnBase[T] extends Node with WithOp {
  override def nodeDelegate: Node = if(op eq null) this else op.nodeDelegate

  def getResult(profile: BasicProfile, rs: PositionedResult): T
  def updateResult(profile: BasicProfile, rs: PositionedResult, value: T): Unit
  def setParameter(profile: BasicProfile, ps: PositionedParameters, value: Option[T]): Unit
}

/**
 * Base trait for columns.
 */
abstract class Column[T : TypeMapper] extends ColumnBase[T] {
  final val typeMapper = implicitly[TypeMapper[T]]
  def getResult(profile: BasicProfile, rs: PositionedResult): T = {
    val tmd = typeMapper(profile)
    tmd.nextValueOrElse(tmd.zero, rs)
  }
  def updateResult(profile: BasicProfile, rs: PositionedResult, value: T) = typeMapper(profile).updateValue(value, rs)
  final def setParameter(profile: BasicProfile, ps: PositionedParameters, value: Option[T]): Unit = typeMapper(profile).setOption(value, ps)
  def orElse(n: =>T): Column[T] = new WrappedColumn[T](this) {
    override def getResult(profile: BasicProfile, rs: PositionedResult): T = typeMapper(profile).nextValueOrElse(n, rs)
  }
  final def orFail = orElse { throw new SQueryException("Read NULL value for column "+this) }
  def ? : Column[Option[T]] = new WrappedColumn(this)(typeMapper.createOptionTypeMapper)

  def getOr[U](n: => U)(implicit ev: Option[U] =:= T): Column[U] = new WrappedColumn[U](this)(typeMapper.getBaseTypeMapper) {
    override def getResult(profile: BasicProfile, rs: PositionedResult): U = typeMapper(profile).nextValueOrElse(n, rs)
  }
  def get[U](implicit ev: Option[U] =:= T): Column[U] = getOr[U] { throw new SQueryException("Read NULL value for column "+this) }
  final def ~[U](b: Column[U]) = new Projection2[T, U](this, b)

  // Functions which don't need an OptionMapper
  def in(e: Query[Column[_]]) = ColumnOps.In(Node(this), Node(e))
  def notIn(e: Query[Column[_]]) = ColumnOps.Not(Node(ColumnOps.In(Node(this), Node(e))))
  def count = ColumnOps.Count(Node(this))
  def isNull = ColumnOps.Is(Node(this), ConstColumn.NULL)
  def isNotNull = ColumnOps.Not(Node(ColumnOps.Is(Node(this), ConstColumn.NULL)))
  def countDistinct = ColumnOps.CountDistinct(Node(this))
  def asColumnOf[U : TypeMapper]: Column[U] = ColumnOps.AsColumnOf[U](Node(this), None)
  def asColumnOfType[U : TypeMapper](typeName: String): Column[U] = ColumnOps.AsColumnOf[U](Node(this), Some(typeName))

  def asc = new Ordering.Asc(Node(this))
  def desc = new Ordering.Desc(Node(this))
}

/**
 * A column with a constant value which is inserted into an SQL statement as a literal.
 */
case class ConstColumn[T : TypeMapper](value: T) extends Column[T] {
  def nodeChildren = Nil
  override def toString = value match {
    case null => "ConstColumn null"
    case a: AnyRef => "ConstColumn["+a.getClass.getName+"] "+a
    case _ => "ConstColumn "+value
  }
  def bind = new BindColumn(value)
}

object ConstColumn {
  def NULL = new ConstColumn[Null](null)(TypeMapper.NullTypeMapper)
}

/**
 * A column with a constant value which gets turned into a bind variable.
 */
case class BindColumn[T : TypeMapper](value: T) extends Column[T] {
  def nodeChildren = Nil
  override def toString = value match {
    case null => "BindColumn null"
    case a: AnyRef => "BindColumn["+a.getClass.getName+"] "+a
    case _ => "BindColumn "+value
  }
}

/**
 * A parameter from a QueryTemplate which gets turned into a bind variable.
 */
case class ParameterColumn[T : TypeMapper](idx: Int) extends Column[T] {
  def nodeChildren = Nil
  override def toString = "ParameterColumn "+idx
}

/**
 * A column which gets created as the result of applying an operator.
 */
abstract class OperatorColumn[T : TypeMapper] extends Column[T] {
  protected[this] val leftOperand: Node = Node(this)
}

/**
 * A WrappedColumn can be used to change a column's nullValue.
 */
class WrappedColumn[T : TypeMapper](parent: ColumnBase[_]) extends Column[T] {
  override def nodeDelegate = if(op eq null) Node(parent) else op.nodeDelegate
  def nodeChildren = nodeDelegate :: Nil
}

/**
 * A column which is part of a Table.
 */
class NamedColumn[T : TypeMapper](val table: Node, val name: String, val options: ColumnOption[T, _]*)
extends Column[T] {
  def nodeChildren = table :: Nil
  override def toString = "NamedColumn " + name
  override def nodeNamedChildren = (table, "table") :: Nil
}

object NamedColumn {
  def unapply[T](n: NamedColumn[T]) = Some((n.table, n.name, n.options))
}
