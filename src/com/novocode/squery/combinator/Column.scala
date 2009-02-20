package com.novocode.squery.combinator

import com.novocode.squery.session.{PositionedResult, PositionedParameters}
import java.io.PrintWriter


/////////////////////////////////////////////////////////////////////////////
//  Columns
/////////////////////////////////////////////////////////////////////////////

sealed trait Column[+T] extends WithOp with Dump {
  def count = Operator.Count(this)
  def max = Operator.Max(this)
  def is[E](e: Column[E]) = Operator.Is(this, e)
  def in[E](e: Query[Column[E]]) = Operator.In(this, e.value.withOp(ColumnOp.SubQueryOp(e)))
  def notIn[E](e: Query[Column[E]]) = Operator.Not(Operator.In(this, e.value.withOp(ColumnOp.SubQueryOp(e))))
  def isNot[E](e: Column[E]) = Operator.Not(Operator.Is(this, e))
  def sortBy[CT](c: Column[CT]): this.type = withOp(ColumnOp.SortOp(this, c, false))
  def sortByDesc[CT](c: Column[CT]): this.type = withOp(ColumnOp.SortOp(this, c, true))
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
  def dumpThis(out: PrintWriter, prefix: String, name: String, dumper: Dump.Dumper) {
    val s = value match {
      case null => "null"
      case a: AnyRef => "["+a.getClass.getName+"] "+a
      case _ => "[Any] "+value
    }
    out.println(prefix+name+"ConstColumn "+s)
  }
}

trait BooleanColumn extends SimpleColumn[java.lang.Boolean] {
  def &&(b: BooleanColumn) = Operator.And(this, b)
  def ||(b: BooleanColumn) = Operator.Or(this, b)
  def not = Operator.Not(this)
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
  def dumpThis(out: PrintWriter, prefix: String, name: String, dumper: Dump.Dumper) {
    dumper(out, prefix, name+"<WrappedColumn> ", parent)
  }
}

abstract class NamedColumn[T](val table: Table[_], val name: String, val options: ColumnOption*) extends SimpleColumn[T] {
  def dumpThis(out: PrintWriter, prefix: String, name: String, dumper: Dump.Dumper) {
    out.println(prefix+name+"NamedColumn "+this.name)
    dumper(out, prefix+"  ", "table: ", table)
  }
}


/////////////////////////////////////////////////////////////////////////////
//  Tables
/////////////////////////////////////////////////////////////////////////////

sealed trait TableBase[T] extends Column[T] {
  def join[U <: TableBase.T_](other: U) = new Join[this.type, U](this, other)
}

object TableBase {
  type T_ = TableBase[_]
}

abstract class Table[T](val tableName: String) extends TableBase[T] with ConvertibleColumn[T] { self =>

  def O = ColumnOption

  def dumpThis(out: PrintWriter, prefix: String, name: String, dumper: Dump.Dumper) {
    out.println(prefix+name+"Table "+tableName)
  }

  def stringColumn(n: String, options: ColumnOption*) = new NamedColumn[String](this, n, options:_*) with StringColumn
  def intColumn(n: String, options: ColumnOption*) = new NamedColumn[java.lang.Integer](this, n, options:_*) with IntColumn
  def booleanColumn(n: String, options: ColumnOption*) = new NamedColumn[java.lang.Boolean](this, n, options:_*) with BooleanColumn

  def * : ConvertibleColumn[T]

  def getResult(rs: PositionedResult) = *.getResult(rs)
  def setParameter(ps: PositionedParameters, value: T) = *.setParameter(ps, value)
}

class Join[+T1 <: TableBase.T_, +T2 <: TableBase.T_](_left: T1, _right: T2) extends TableBase[Nothing] {
  val left = _left.withOp(ColumnOp.JoinOp(_left, this))
  val right = _right.withOp(ColumnOp.JoinOp(_right, this))

  //def on(preds: BooleanColumn) = this

  def dumpThis(out: PrintWriter, prefix: String, name: String, dumper: Dump.Dumper) {
    out.println(prefix+name+"Join")
    dumper(out, prefix+"  ", "left: ", left)
    dumper(out, prefix+"  ", "right: ", right)
  }
}

object Join {
  def unapply[T1 <: TableBase.T_, T2 <: TableBase.T_](j: Join[T1, T2]) = Some((j.left, j.right))
}


/////////////////////////////////////////////////////////////////////////////
//  Projections
/////////////////////////////////////////////////////////////////////////////

sealed trait Projection[T <: Product] extends ConvertibleColumn[T] with Product {
  def elements = 0 until productArity map { productElement(_).asInstanceOf[SimpleColumn[_]] }

  def dumpThis(out: PrintWriter, prefix: String, name: String, dumper: Dump.Dumper) {
    out.println(prefix+name+"Projection")
    for(i <- 0 until productArity)
      dumper(out, prefix+"  ", "_"+(i+1)+": ", productElement(i))
  }

  def setParameter(ps: PositionedParameters, value: T): Unit = {
    for(i <- 0 until productArity) {
      val v = value.productElement(i)
      productElement(i).asInstanceOf[ConvertibleColumn[Any]].setParameter(ps, v)
    }
  }
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
