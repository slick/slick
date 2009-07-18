package com.novocode.squery.combinator

import com.novocode.squery.session.{PositionedResult, PositionedParameters}


sealed trait Projection[T <: Product] extends ColumnBase[T] with Product {
  def nodeChildren = 0 until productArity map { i => Node(productElement(i)) } toList

  def setParameter(ps: PositionedParameters, value: Option[T]): Unit = {
    for(i <- 0 until productArity) {
      productElement(i).asInstanceOf[Column[Any]].setParameter(ps, value.map(_.productElement(i)))
    }
  }

  def getResultOption(rs: PositionedResult) = Some(getResult(rs))

  override def toString = "Projection" + productArity
}

final class Projection2[T1,T2](
  override val _1: Column[T1],
  override val _2: Column[T2])
extends Tuple2(_1,_2) with Projection[(T1,T2)] {
  def ~[U](c: Column[U]) = new Projection3(_1,_2,c)
  def getResult(rs: PositionedResult) =
    (_1.getResult(rs),
     _2.getResult(rs))
  override def mapOp(f: Node => Node): this.type = new Projection2(
    _1.mapOp(f),
    _2.mapOp(f)).asInstanceOf[this.type]
}

final class Projection3[T1,T2,T3](
  override val _1: Column[T1],
  override val _2: Column[T2],
  override val _3: Column[T3])
extends Tuple3(_1,_2,_3) with Projection[(T1,T2,T3)] {
  def ~[U](c: Column[U]) = new Projection4(_1,_2,_3,c)
  def getResult(rs: PositionedResult) =
    (_1.getResult(rs),
     _2.getResult(rs),
     _3.getResult(rs))
  override def mapOp(f: Node => Node): this.type = new Projection3(
    _1.mapOp(f),
    _2.mapOp(f),
    _3.mapOp(f)).asInstanceOf[this.type]
}

final class Projection4[T1,T2,T3,T4](
  override val _1: Column[T1],
  override val _2: Column[T2],
  override val _3: Column[T3],
  override val _4: Column[T4])
extends Tuple4(_1,_2,_3,_4) with Projection[(T1,T2,T3,T4)] {
  def ~[U](c: Column[U]) = new Projection5(_1,_2,_3,_4,c)
  def getResult(rs: PositionedResult) =
    (_1.getResult(rs),
     _2.getResult(rs),
     _3.getResult(rs),
     _4.getResult(rs))
  override def mapOp(f: Node => Node): this.type = new Projection4(
    _1.mapOp(f),
    _2.mapOp(f),
    _3.mapOp(f),
    _4.mapOp(f)).asInstanceOf[this.type]
}

final class Projection5[T1,T2,T3,T4,T5](
  override val _1: Column[T1],
  override val _2: Column[T2],
  override val _3: Column[T3],
  override val _4: Column[T4],
  override val _5: Column[T5])
extends Tuple5(_1,_2,_3,_4,_5) with Projection[(T1,T2,T3,T4,T5)] {
  def ~[U](c: Column[U]) = new Projection6(_1,_2,_3,_4,_5,c)
  def getResult(rs: PositionedResult) =
    (_1.getResult(rs),
     _2.getResult(rs),
     _3.getResult(rs),
     _4.getResult(rs),
     _5.getResult(rs))
  override def mapOp(f: Node => Node): this.type = new Projection5(
    _1.mapOp(f),
    _2.mapOp(f),
    _3.mapOp(f),
    _4.mapOp(f),
    _5.mapOp(f)).asInstanceOf[this.type]
}

final class Projection6[T1,T2,T3,T4,T5,T6](
  override val _1: Column[T1],
  override val _2: Column[T2],
  override val _3: Column[T3],
  override val _4: Column[T4],
  override val _5: Column[T5],
  override val _6: Column[T6])
extends Tuple6(_1,_2,_3,_4,_5,_6) with Projection[(T1,T2,T3,T4,T5,T6)] {
  def ~[U](c: Column[U]) = new Projection7(_1,_2,_3,_4,_5,_6,c)
  def getResult(rs: PositionedResult) =
    (_1.getResult(rs),
     _2.getResult(rs),
     _3.getResult(rs),
     _4.getResult(rs),
     _5.getResult(rs),
     _6.getResult(rs))
  override def mapOp(f: Node => Node): this.type = new Projection6(
    _1.mapOp(f),
    _2.mapOp(f),
    _3.mapOp(f),
    _4.mapOp(f),
    _5.mapOp(f),
    _6.mapOp(f)).asInstanceOf[this.type]
}

final class Projection7[T1,T2,T3,T4,T5,T6,T7](
  override val _1: Column[T1],
  override val _2: Column[T2],
  override val _3: Column[T3],
  override val _4: Column[T4],
  override val _5: Column[T5],
  override val _6: Column[T6],
  override val _7: Column[T7])
extends Tuple7(_1,_2,_3,_4,_5,_6,_7) with Projection[(T1,T2,T3,T4,T5,T6,T7)] {
  def ~[U](c: Column[U]) = new Projection8(_1,_2,_3,_4,_5,_6,_7,c)
  def getResult(rs: PositionedResult) =
    (_1.getResult(rs),
     _2.getResult(rs),
     _3.getResult(rs),
     _4.getResult(rs),
     _5.getResult(rs),
     _6.getResult(rs),
     _7.getResult(rs))
  override def mapOp(f: Node => Node): this.type = new Projection7(
    _1.mapOp(f),
    _2.mapOp(f),
    _3.mapOp(f),
    _4.mapOp(f),
    _5.mapOp(f),
    _6.mapOp(f),
    _7.mapOp(f)).asInstanceOf[this.type]
}

final class Projection8[T1,T2,T3,T4,T5,T6,T7,T8](
  override val _1: Column[T1],
  override val _2: Column[T2],
  override val _3: Column[T3],
  override val _4: Column[T4],
  override val _5: Column[T5],
  override val _6: Column[T6],
  override val _7: Column[T7],
  override val _8: Column[T8])
extends Tuple8(_1,_2,_3,_4,_5,_6,_7,_8) with Projection[(T1,T2,T3,T4,T5,T6,T7,T8)] {
  def ~[U](c: Column[U]) = new Projection9(_1,_2,_3,_4,_5,_6,_7,_8,c)
  def getResult(rs: PositionedResult) =
    (_1.getResult(rs),
     _2.getResult(rs),
     _3.getResult(rs),
     _4.getResult(rs),
     _5.getResult(rs),
     _6.getResult(rs),
     _7.getResult(rs),
     _8.getResult(rs))
  override def mapOp(f: Node => Node): this.type = new Projection8(
    _1.mapOp(f),
    _2.mapOp(f),
    _3.mapOp(f),
    _4.mapOp(f),
    _5.mapOp(f),
    _6.mapOp(f),
    _7.mapOp(f),
    _8.mapOp(f)).asInstanceOf[this.type]
}

final class Projection9[T1,T2,T3,T4,T5,T6,T7,T8,T9](
  override val _1: Column[T1],
  override val _2: Column[T2],
  override val _3: Column[T3],
  override val _4: Column[T4],
  override val _5: Column[T5],
  override val _6: Column[T6],
  override val _7: Column[T7],
  override val _8: Column[T8],
  override val _9: Column[T9])
extends Tuple9(_1,_2,_3,_4,_5,_6,_7,_8,_9) with Projection[(T1,T2,T3,T4,T5,T6,T7,T8,T9)] {
  //def ~[U](c: Column[U]) = new Projection10(_1,_2,_3,_4,_5,_6,_7,_8,_9,c)
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
  override def mapOp(f: Node => Node): this.type = new Projection9(
    _1.mapOp(f),
    _2.mapOp(f),
    _3.mapOp(f),
    _4.mapOp(f),
    _5.mapOp(f),
    _6.mapOp(f),
    _7.mapOp(f),
    _8.mapOp(f),
    _9.mapOp(f)).asInstanceOf[this.type]
}
