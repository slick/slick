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

abstract case class ConstColumn[T](val value: T) extends ConvertibleColumn[T] { self: SimpleColumn[T] =>
  def nodeChildren = Nil
  override def toString = value match {
    case null => "null"
    case a: AnyRef => "ConstColumn["+a.getClass.getName+"] "+a
    case _ => "ConstColumn "+value
  }
}

trait BooleanColumn extends SimpleColumn[Boolean] {
  def sqlType = java.sql.Types.BOOLEAN
  def nullValue = false
  def orElse(n: =>Boolean) = new WrappedColumn(this) with BooleanColumn { override def nullValue = n }
  def &&(b: BooleanColumn) = Operator.And(Node(this), Node(b))
  def ||(b: BooleanColumn) = Operator.Or(Node(this), Node(b))
  def not = Operator.Not(Node(this))
  def getResult(rs: PositionedResult): Boolean = rs.nextBooleanOrElse(nullValue)
  def getResultOption(rs: PositionedResult): Option[Boolean] = rs.nextBooleanOption
  def setParameter(ps: PositionedParameters, value: Option[Boolean]): Unit = ps.setBooleanOption(value)
  def valueToSQLLiteral(value: Boolean): String = value.toString
}

trait IntColumn extends SimpleColumn[Int] {
  def sqlType = java.sql.Types.INTEGER
  def nullValue = 0
  def orElse(n: =>Int) = new WrappedColumn(this) with IntColumn { override def nullValue = n }
  def getResult(rs: PositionedResult): Int = rs.nextIntOrElse(nullValue)
  def getResultOption(rs: PositionedResult): Option[Int] = rs.nextIntOption
  def setParameter(ps: PositionedParameters, value: Option[Int]): Unit = ps.setIntOption(value)
  def valueToSQLLiteral(value: Int): String = value.toString
}

trait StringColumn extends SimpleColumn[String] {
  def sqlType = java.sql.Types.VARCHAR
  def nullValue = ""
  def orElse(n: =>String) = new WrappedColumn(this) with StringColumn { override def nullValue = n }
  def getResult(rs: PositionedResult): String = rs.nextStringOrElse(nullValue)
  def getResultOption(rs: PositionedResult): Option[String] = rs.nextStringOption
  def setParameter(ps: PositionedParameters, value: Option[String]): Unit = ps.setStringOption(value)

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
