package com.novocode.squery.combinator

import com.novocode.squery.session.{PositionedResult, PositionedParameters, TypeMapper}

/**
 * Common base trait for columns, tables and projections (but not unions and joins).
 */
trait ColumnBase[T] extends Node with WithOp {
  override def nodeDelegate: Node = if(op eq null) this else op.nodeDelegate

  def getResult(rs: PositionedResult): T
  def getResultOption(rs: PositionedResult): Option[T]
  def setParameter(ps: PositionedParameters, value: Option[T]): Unit

  // Operators
  def count = Operator.Count(Node(this))
}

object ColumnBase {
  type T_ = ColumnBase[_]
}

/**
 * Base trait for columns.
 */
trait Column[T] extends ColumnBase[T] {
  val typeMapper: TypeMapper[T]
  def nullValue: T = typeMapper.zero
  final def getResult(rs: PositionedResult): T = typeMapper.nextValue(rs)
  final def getResultOption(rs: PositionedResult): Option[T] = typeMapper.nextOption(rs)
  final def setParameter(ps: PositionedParameters, value: Option[T]): Unit = typeMapper.setOption(value, ps)
  def orElse(n: =>T): Column[T] = new WrappedColumn(this, typeMapper) {
    override def nullValue = n
  }
  final def orFail = orElse { throw new SQueryException("Read NULL value for column "+this) }
  def ? : Column[Option[T]] = new WrappedColumn(this, TypeMapper.typeMapperToOptionTypeMapper(typeMapper)) {
    override def nullValue = None
  }
  final def ~[U](b: Column[U]) = new Projection2[T, U](this, b)

  // Functions which don't need an OptionMapper
  def in(e: Query[Column[_]]) = Operator.In(Node(this), Node(e))
  def notIn(e: Query[Column[_]]) = Operator.Not(Node(Operator.In(Node(this), Node(e))))
  def max = mapOp(Operator.Max(_))
  def isNull = Operator.Is(Node(this), ConstColumn.NULL)
  def isNotNull = Operator.Not(Node(Operator.Is(Node(this), ConstColumn.NULL)))
}

/**
 * A column with a constant value which is inserted into an SQL statement as a literal.
 */
case class ConstColumn[T](val value: T)(implicit val typeMapper: TypeMapper[T]) extends Column[T] {
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
case class BindColumn[T](val value: T)(implicit val typeMapper: TypeMapper[T]) extends Column[T] {
  def nodeChildren = Nil
  override def toString = value match {
    case null => "BindColumn null"
    case a: AnyRef => "BindColumn["+a.getClass.getName+"] "+a
    case _ => "BindColumn "+value
  }
}

/**
 * A column which gets created as the result of applying an operator.
 */
abstract class OperatorColumn[T](implicit val typeMapper: TypeMapper[T]) extends Column[T] {
  protected[this] val leftOperand: Node = Node(this)
}

/**
 * A WrappedColumn can be used to change a column's nullValue.,
 */
class WrappedColumn[T](parent: ColumnBase[_], val typeMapper: TypeMapper[T]) extends Column[T] {
  override def nodeDelegate = if(op eq null) Node(parent) else op.nodeDelegate
  def nodeChildren = nodeDelegate :: Nil
}

/**
 * A column which is part of a Table.
 */
class NamedColumn[T](val table: Node, val name: String, val typeMapper: TypeMapper[T], val options: ColumnOption[T]*) extends
    Column[T] {
  def nodeChildren = table :: Nil
  override def toString = "NamedColumn " + name
}
