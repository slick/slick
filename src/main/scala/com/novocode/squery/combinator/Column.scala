package com.novocode.squery.combinator

import com.novocode.squery.session.{PositionedResult, PositionedParameters, TypeMapper}


trait Column[T] extends Node with WithOp {
  def count = Operator.Count(Node(this))
  def max = Operator.Max(Node(this))
  def is[E](e: Column[E]) = Operator.Is(Node(this), Node(e))
  def in[E](e: Query[Column[E]]) = Operator.In(Node(this), Node(e))
  def notIn[E](e: Query[Column[E]]) = Operator.Not(Node(Operator.In(Node(this), Node(e))))
  def isNot[E](e: Column[E]) = Operator.Not(Node(Operator.Is(Node(this), Node(e))))
  def sortBy[CT](c: Column[CT]): this.type = withOp(Operator.Ordering(Node(this), Node(c), false))
  def sortByDesc[CT](c: Column[CT]): this.type = withOp(Operator.Ordering(Node(this), Node(c), true))
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
  def ~[U](b: SimpleColumn[U]) = new Projection2[T, U](this, b)
  def nullValue: T
  def orElse(nullValue: =>T): SimpleColumn[T]
  def orFail = orElse { throw new SQueryException("Read NULL value for column "+this) }
  def ? = new OptionColumn(this, None)
}

object SimpleColumn {
  type T_ = SimpleColumn[_]
}

class OptionColumn[T](parent: SimpleColumn[T], nullVal: Option[T]) extends SimpleColumn[Option[T]] {
  def nodeChildren = Node(parent) :: Nil
  def nullValue = nullVal
  def orElse(nullValue: =>Option[T]) = new OptionColumn(parent, nullValue)
  def getResult(rs: PositionedResult): Option[T] = parent.getResultOption(rs) match {
    case None => nullValue
    case s => s
  }
  def getResultOption(rs: PositionedResult): Option[Option[T]] = Some(getResult(rs))
  def setParameter(ps: PositionedParameters, value: Option[Option[T]]): Unit = parent.setParameter(ps, value getOrElse None)
}

case class ConstColumn[T](val value: T)(implicit val typeMapper: TypeMapper[T]) extends TypeMappedColumn[T] {
  def nodeChildren = Nil
  override def toString = value match {
    case null => "null"
    case a: AnyRef => "ConstColumn["+a.getClass.getName+"] "+a
    case _ => "ConstColumn "+value
  }
}

trait TypeMappedColumn[T] extends SimpleColumn[T] { self =>
  val typeMapper: TypeMapper[T]
  def nullValue: T = typeMapper.zero
  def sqlType = typeMapper.sqlType
  final def getResult(rs: PositionedResult): T = typeMapper.nextValue(rs)
  final def getResultOption(rs: PositionedResult): Option[T] = typeMapper.nextOption(rs)
  final def setParameter(ps: PositionedParameters, value: Option[T]): Unit = typeMapper.setOption(value, ps)
  final def valueToSQLLiteral(value: T): String = typeMapper.valueToSQLLiteral(value)
  def orElse(n: =>T) = new WrappedColumn(this) with TypeMappedColumn[T] {
    override def nullValue = n
    val typeMapper = self.typeMapper
  }
}

abstract class OperatorColumn[T](implicit val typeMapper: TypeMapper[T]) extends TypeMappedColumn[T] {
  protected[this] val leftOperand: Node = Node(this)
}

abstract class WrappedColumn[T](val parent: SimpleColumn[T]) extends ConvertibleColumn[T] { self: SimpleColumn[T] =>
  def nodeChildren = parent :: Nil
}

class NamedColumn[T](val table: Node, val name: String, val options: ColumnOption[T]*)(implicit val typeMapper: TypeMapper[T]) extends
    TypeMappedColumn[T] { self =>
  def nodeChildren = table :: Nil
  override def toString = "NamedColumn " + name
}
