package com.novocode.squery.combinator

import com.novocode.squery.session.{PositionedResult, PositionedParameters}


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
  def setParameter(ps: PositionedParameters, value: T): Unit
}

trait SimpleColumn[T] extends ConvertibleColumn[T] {
  def ~[U](b: SimpleColumn[U]) = new Projection2[T, U](this, b)
}

abstract case class ConstColumn[T](val value: T) extends ConvertibleColumn[T] { self: SimpleColumn[T] =>
  def nodeChildren = Nil
  override def toString = value match {
    case null => "null"
    case a: AnyRef => "ConstColumn["+a.getClass.getName+"] "+a
    case _ => "ConstColumn "+value
  }
}

trait BooleanColumn extends SimpleColumn[java.lang.Boolean] {
  def &&(b: BooleanColumn) = Operator.And(Node(this), Node(b))
  def ||(b: BooleanColumn) = Operator.Or(Node(this), Node(b))
  def not = Operator.Not(Node(this))
  def getResult(rs: PositionedResult): java.lang.Boolean = rs.nextBooleanOrNull
  def setParameter(ps: PositionedParameters, value: java.lang.Boolean): Unit = ps.setBoolean(value)
  def valueToSQLLiteral(value: java.lang.Boolean): String = value.toString
}

trait IntColumn extends SimpleColumn[java.lang.Integer] {
  def getResult(rs: PositionedResult): java.lang.Integer = rs.nextIntegerOrNull
  def setParameter(ps: PositionedParameters, value: java.lang.Integer): Unit = ps.setInt(value)
  def valueToSQLLiteral(value: java.lang.Integer): String = value.toString
}

trait StringColumn extends SimpleColumn[String] {
  def getResult(rs: PositionedResult): String = rs.nextString
  def setParameter(ps: PositionedParameters, value: String): Unit = ps.setString(value)

  def valueToSQLLiteral(value: String): String = {
    val sb = new StringBuilder
    StringColumn.createStringLiteral(value, sb)
    sb.toString
  }
}

object StringColumn {
  def createStringLiteral(s: String, sb: StringBuilder) {
    sb append '\''
    for(c <- s) c match {
      case '\'' => sb append "''"
      case _ => sb append c
    }
    sb append '\''
  }
}

abstract class WrappedColumn[T](val parent: SimpleColumn[T]) extends ConvertibleColumn[T] { self: SimpleColumn[T] =>
  def nodeChildren = parent :: Nil
}

abstract class NamedColumn[T](val table: Node, val name: String, val options: ColumnOption[T]*) extends SimpleColumn[T] {
  def nodeChildren = table :: Nil
  override def toString = "NamedColumn " + name
  def valueToSQLLiteral(value: T): String
}
