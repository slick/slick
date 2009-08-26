package com.novocode.squery.combinator

import com.novocode.squery.combinator.basic.BasicProfile
import com.novocode.squery.session.{PositionedResult, PositionedParameters}

sealed trait TableBase[T] extends Node with WithOp {
  def join[U <: TableBase.T_](other: U) = new Join[this.type, U](this, other)
  override def isNamedTable = true
}

object TableBase {
  type T_ = TableBase[_]
}

abstract class Table[T](val tableName: String) extends TableBase[T] with ColumnBase[T] {

  def nodeChildren = Nil
  override def toString = "Table " + tableName

  val O = ColumnOption

  def column[C](n: String, options: ColumnOption[C]*)(implicit tm: TypeMapper[C]) = new NamedColumn[C](Node(this), n, tm, options:_*)

  def * : ColumnBase[T]

  def getResult(profile: BasicProfile, rs: PositionedResult) = *.getResult(profile, rs)
  def setParameter(profile: BasicProfile, ps: PositionedParameters, value: Option[T]) = *.setParameter(profile, ps, value)
}

object Table {
  def unapply[T](t: Table[T]) = Some(t.tableName)

  final case class Alias(child: Node) extends UnaryNode {
    override def toString = "Table.Alias"
    override def isNamedTable = true
  }
}

final class Join[+T1 <: TableBase.T_, +T2 <: TableBase.T_](_left: T1, _right: T2) extends TableBase[Nothing] {
  def left = _left.mapOp(n => Join.JoinPart(Node(n), Node(this)))
  def right = _right.mapOp(n => Join.JoinPart(Node(n), Node(this)))
  def nodeChildren = Node(_left) :: Node(_right) :: Nil
  override def toString = "Join(" + Node(_left) + "," + Node(_right) + ")"

  //def on(preds: BooleanColumn) = this
}

object Join {
  def unapply[T1 <: TableBase.T_, T2 <: TableBase.T_](j: Join[T1, T2]) = Some((j.left, j.right))

  final case class JoinPart(left: Node, right: Node) extends BinaryNode {
    override def toString = "JoinPart"
    override def nodeNamedChildren = (left, "table") :: (right, "from") :: Nil
  }
}
