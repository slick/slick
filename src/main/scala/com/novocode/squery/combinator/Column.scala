package com.novocode.squery.combinator

import com.novocode.squery.session.{PositionedResult, PositionedParameters, TypeMapper}


trait Column[T] extends Node with WithOp {
  override def nodeDelegate: Node = if(op eq null) this else op.nodeDelegate
  def count = Operator.Count(Node(this))
  def max = Operator.Max(Node(this))
  def is[E](e: Column[E]) = Operator.Is(Node(this), Node(e))
  def in[E](e: Query[Column[E]]) = Operator.In(Node(this), Node(e))
  def notIn[E](e: Query[Column[E]]) = Operator.Not(Node(Operator.In(Node(this), Node(e))))
  def isNot[E](e: Column[E]) = Operator.Not(Node(Operator.Is(Node(this), Node(e))))
}

object Column {
  type T_ = Column[_]
}

trait ConvertibleColumn[T] extends Column[T] {
  def getResult(rs: PositionedResult): T
  def getResultOption(rs: PositionedResult): Option[T]
  def setParameter(ps: PositionedParameters, value: Option[T]): Unit
}

trait SimpleColumn[T] extends ConvertibleColumn[T] {
  val typeMapper: TypeMapper[T]
  def nullValue: T = typeMapper.zero
  final def getResult(rs: PositionedResult): T = typeMapper.nextValue(rs)
  final def getResultOption(rs: PositionedResult): Option[T] = typeMapper.nextOption(rs)
  final def setParameter(ps: PositionedParameters, value: Option[T]): Unit = typeMapper.setOption(value, ps)
  def orElse(n: =>T): SimpleColumn[T] = new WrappedColumn(this, typeMapper) {
    override def nullValue = n
  }
  final def orFail = orElse { throw new SQueryException("Read NULL value for column "+this) }
  def ? : SimpleColumn[Option[T]] = new WrappedColumn(this, TypeMapper.typeMapperToOptionTypeMapper(typeMapper)) {
    override def nullValue = None
  }
  final def ~[U](b: SimpleColumn[U]) = new Projection2[T, U](this, b)
}

case class ConstColumn[T](val value: T)(implicit val typeMapper: TypeMapper[T]) extends SimpleColumn[T] {
  def nodeChildren = Nil
  override def toString = value match {
    case null => "null"
    case a: AnyRef => "ConstColumn["+a.getClass.getName+"] "+a
    case _ => "ConstColumn "+value
  }
}

abstract class OperatorColumn[T](implicit val typeMapper: TypeMapper[T]) extends SimpleColumn[T] {
  protected[this] val leftOperand: Node = Node(this)
}

class WrappedColumn[T](parent: Column[_], val typeMapper: TypeMapper[T]) extends SimpleColumn[T] {
  override def nodeDelegate = if(op eq null) Node(parent) else op.nodeDelegate
  def nodeChildren = nodeDelegate :: Nil
}

class NamedColumn[T](val table: Node, val name: String, val typeMapper: TypeMapper[T], val options: ColumnOption[T]*) extends
    SimpleColumn[T] {
  def nodeChildren = table :: Nil
  override def toString = "NamedColumn " + name
}
