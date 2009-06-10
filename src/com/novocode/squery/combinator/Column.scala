package com.novocode.squery.combinator

import com.novocode.squery.session.{PositionedResult, PositionedParameters}
import java.io.PrintWriter


/////////////////////////////////////////////////////////////////////////////
//  Columns
/////////////////////////////////////////////////////////////////////////////

sealed trait Column[T] extends Node with WithOp {
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
}

trait IntColumn extends SimpleColumn[java.lang.Integer] {
  def getResult(rs: PositionedResult): java.lang.Integer = rs.nextIntegerOrNull
  def setParameter(ps: PositionedParameters, value: java.lang.Integer): Unit = ps.setInt(value)
}

trait StringColumn extends SimpleColumn[String] {
  def getResult(rs: PositionedResult): String = rs.nextString
  def setParameter(ps: PositionedParameters, value: String): Unit = ps.setString(value)
}

abstract class WrappedColumn[T](val parent: SimpleColumn[T]) extends ConvertibleColumn[T] { self: SimpleColumn[T] =>
  def nodeChildren = parent :: Nil
}

abstract class NamedColumn[T](val table: Node, val name: String, val options: ColumnOption*) extends SimpleColumn[T] {
  def nodeChildren = table :: Nil
  override def toString = "NamedColumn " + name
}


/////////////////////////////////////////////////////////////////////////////
//  Tables
/////////////////////////////////////////////////////////////////////////////

sealed trait TableBase[T] extends Column[T] {
  def join[U <: TableBase.T_](other: U) = new Join[this.type, U](this, other)
  override def isNamedTable = true
}

object TableBase {
  type T_ = TableBase[_]
}

abstract class Table[T](val tableName: String) extends TableBase[T] with ConvertibleColumn[T] {

  def nodeChildren = Nil
  override def toString = "Table " + tableName

  def O = ColumnOption

  def stringColumn(n: String, options: ColumnOption*) = new NamedColumn[String](Node(this), n, options:_*) with StringColumn
  def intColumn(n: String, options: ColumnOption*) = new NamedColumn[java.lang.Integer](Node(this), n, options:_*) with IntColumn
  def booleanColumn(n: String, options: ColumnOption*) = new NamedColumn[java.lang.Boolean](Node(this), n, options:_*) with BooleanColumn

  def * : ConvertibleColumn[T]

  def getResult(rs: PositionedResult) = *.getResult(rs)
  def setParameter(ps: PositionedParameters, value: T) = *.setParameter(ps, value)
}

object Table {
  final case class Alias(child: Node) extends UnaryNode {
    override def toString = "Table.Alias"
    override def isNamedTable = true
  }
}

final class Join[+T1 <: TableBase.T_, +T2 <: TableBase.T_](_left: T1, _right: T2) extends TableBase[Nothing] {
  def left = _left.withOp(Join.JoinPart(Node(_left), Node(this)))
  def right = _right.withOp(Join.JoinPart(Node(_right), Node(this)))
  def nodeChildren = Node(_left) :: Node(_right) :: Nil
  override def toString = "Join(" + Node(_left) + "," + Node(_right) + ")"

  //def on(preds: BooleanColumn) = this
}

object Join {
  def unapply[T1 <: TableBase.T_, T2 <: TableBase.T_](j: Join[T1, T2]) = Some((j.left, j.right))

  final case class JoinPart(left: Node, right: Node) extends BinaryNode {
    override def toString = "JoinPart"
    override def nodeChildrenNames = Stream("table", "from")
  }
}

class Union[T <: Column.T_](val all: Boolean, query1: Query[T], query2: Query[T]) extends TableBase[Nothing] {
  val left = Node(query1)
  val right = Node(query2)
  def nodeChildren = left :: right :: Nil
}

object Union {
  //def unapply(u: Union[_]) = Some((u.all, u.left, u.right))

  final case class UnionPart(child: Node) extends UnaryNode {
    override def toString = "UnionPart"
  }
}


/////////////////////////////////////////////////////////////////////////////
//  Projections
/////////////////////////////////////////////////////////////////////////////

sealed trait Projection[T <: Product] extends ConvertibleColumn[T] with Product {
  def nodeChildren = 0 until productArity map { i => Node(productElement(i)) } toList

  def setParameter(ps: PositionedParameters, value: T): Unit = {
    for(i <- 0 until productArity) {
      val v = value.productElement(i)
      productElement(i).asInstanceOf[ConvertibleColumn[Any]].setParameter(ps, v)
    }
  }

  override def toString = "Projection" + productArity
}

final class Projection2[T1,T2](
  override val _1: SimpleColumn[T1],
  override val _2: SimpleColumn[T2])
extends Tuple2(_1,_2) with Projection[(T1,T2)] {
  def ~[U](c: SimpleColumn[U]) = new Projection3(_1,_2,c)
  def getResult(rs: PositionedResult) =
    (_1.getResult(rs),
     _2.getResult(rs))
}

final class Projection3[T1,T2,T3](
  override val _1: SimpleColumn[T1],
  override val _2: SimpleColumn[T2],
  override val _3: SimpleColumn[T3])
extends Tuple3(_1,_2,_3) with Projection[(T1,T2,T3)] {
  def ~[U](c: SimpleColumn[U]) = new Projection4(_1,_2,_3,c)
  def getResult(rs: PositionedResult) =
    (_1.getResult(rs),
     _2.getResult(rs),
     _3.getResult(rs))
}

final class Projection4[T1,T2,T3,T4](
  override val _1: SimpleColumn[T1],
  override val _2: SimpleColumn[T2],
  override val _3: SimpleColumn[T3],
  override val _4: SimpleColumn[T4])
extends Tuple4(_1,_2,_3,_4) with Projection[(T1,T2,T3,T4)] {
  def ~[U](c: SimpleColumn[U]) = new Projection5(_1,_2,_3,_4,c)
  def getResult(rs: PositionedResult) =
    (_1.getResult(rs),
     _2.getResult(rs),
     _3.getResult(rs),
     _4.getResult(rs))
}

final class Projection5[T1,T2,T3,T4,T5](
  override val _1: SimpleColumn[T1],
  override val _2: SimpleColumn[T2],
  override val _3: SimpleColumn[T3],
  override val _4: SimpleColumn[T4],
  override val _5: SimpleColumn[T5])
extends Tuple5(_1,_2,_3,_4,_5) with Projection[(T1,T2,T3,T4,T5)] {
  def ~[U](c: SimpleColumn[U]) = new Projection6(_1,_2,_3,_4,_5,c)
  def getResult(rs: PositionedResult) =
    (_1.getResult(rs),
     _2.getResult(rs),
     _3.getResult(rs),
     _4.getResult(rs),
     _5.getResult(rs))
}

final class Projection6[T1,T2,T3,T4,T5,T6](
  override val _1: SimpleColumn[T1],
  override val _2: SimpleColumn[T2],
  override val _3: SimpleColumn[T3],
  override val _4: SimpleColumn[T4],
  override val _5: SimpleColumn[T5],
  override val _6: SimpleColumn[T6])
extends Tuple6(_1,_2,_3,_4,_5,_6) with Projection[(T1,T2,T3,T4,T5,T6)] {
  def ~[U](c: SimpleColumn[U]) = new Projection7(_1,_2,_3,_4,_5,_6,c)
  def getResult(rs: PositionedResult) =
    (_1.getResult(rs),
     _2.getResult(rs),
     _3.getResult(rs),
     _4.getResult(rs),
     _5.getResult(rs),
     _6.getResult(rs))
}

final class Projection7[T1,T2,T3,T4,T5,T6,T7](
  override val _1: SimpleColumn[T1],
  override val _2: SimpleColumn[T2],
  override val _3: SimpleColumn[T3],
  override val _4: SimpleColumn[T4],
  override val _5: SimpleColumn[T5],
  override val _6: SimpleColumn[T6],
  override val _7: SimpleColumn[T7])
extends Tuple7(_1,_2,_3,_4,_5,_6,_7) with Projection[(T1,T2,T3,T4,T5,T6,T7)] {
  def ~[U](c: SimpleColumn[U]) = new Projection8(_1,_2,_3,_4,_5,_6,_7,c)
  def getResult(rs: PositionedResult) =
    (_1.getResult(rs),
     _2.getResult(rs),
     _3.getResult(rs),
     _4.getResult(rs),
     _5.getResult(rs),
     _6.getResult(rs),
     _7.getResult(rs))
}

final class Projection8[T1,T2,T3,T4,T5,T6,T7,T8](
  override val _1: SimpleColumn[T1],
  override val _2: SimpleColumn[T2],
  override val _3: SimpleColumn[T3],
  override val _4: SimpleColumn[T4],
  override val _5: SimpleColumn[T5],
  override val _6: SimpleColumn[T6],
  override val _7: SimpleColumn[T7],
  override val _8: SimpleColumn[T8])
extends Tuple8(_1,_2,_3,_4,_5,_6,_7,_8) with Projection[(T1,T2,T3,T4,T5,T6,T7,T8)] {
  def ~[U](c: SimpleColumn[U]) = new Projection9(_1,_2,_3,_4,_5,_6,_7,_8,c)
  def getResult(rs: PositionedResult) =
    (_1.getResult(rs),
     _2.getResult(rs),
     _3.getResult(rs),
     _4.getResult(rs),
     _5.getResult(rs),
     _6.getResult(rs),
     _7.getResult(rs),
     _8.getResult(rs))
}

final class Projection9[T1,T2,T3,T4,T5,T6,T7,T8,T9](
  override val _1: SimpleColumn[T1],
  override val _2: SimpleColumn[T2],
  override val _3: SimpleColumn[T3],
  override val _4: SimpleColumn[T4],
  override val _5: SimpleColumn[T5],
  override val _6: SimpleColumn[T6],
  override val _7: SimpleColumn[T7],
  override val _8: SimpleColumn[T8],
  override val _9: SimpleColumn[T9])
extends Tuple9(_1,_2,_3,_4,_5,_6,_7,_8,_9) with Projection[(T1,T2,T3,T4,T5,T6,T7,T8,T9)] {
  //def ~[U](c: SimpleColumn[U]) = new Projection10(_1,_2,_3,_4,_5,_6,_7,_8,_9,c)
  def getResult(rs: PositionedResult) =
    (_1.getResult(rs),
     _2.getResult(rs),
     _3.getResult(rs),
     _4.getResult(rs),
     _5.getResult(rs),
     _6.getResult(rs),
     _7.getResult(rs),
     _8.getResult(rs),
     _9.getResult(rs))
}
