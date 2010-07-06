package com.novocode.squery.combinator.basic

import com.novocode.squery.combinator._
import com.novocode.squery.SQueryException
import com.novocode.squery.session.{PositionedResult, PositionedParameters}

abstract class BasicTable[T](_tableName: String) extends AbstractTable[T](_tableName) {

  val O: BasicColumnOptions = BasicColumnOptions

  def column[C : TypeMapper](n: String, options: ColumnOption[C]*) = new NamedColumn[C](Node(this), n, options:_*)

  def createFinderBy[P](f: (this.type => NamedColumn[P]))(implicit profile: BasicProfile, tm: TypeMapper[P]): BasicQueryTemplate[P,T] = {
    import profile.Implicit._
    Parameters[P](tm).flatMap(p => Query(this).where(t => AllColumnOps.Is(f(t.asInstanceOf[BasicTable.this.type]), p)))(profile)
  }

  def innerJoin[U <: TableBase.T_](other: U) = new JoinBase[this.type, U](this, other, Join.Inner)
  def leftJoin[U <: TableBase.T_](other: U) = new JoinBase[this.type, U](this, other, Join.Left)
  def rightJoin[U <: TableBase.T_](other: U) = new JoinBase[this.type, U](this, other, Join.Right)
  def outerJoin[U <: TableBase.T_](other: U) = new JoinBase[this.type, U](this, other, Join.Outer)
}

object BasicTable {
  type T_ = BasicTable[_]

  def unapply[T](t: BasicTable[T]) = Some(t.tableName)

  final case class Alias(child: Node) extends UnaryNode {
    override def toString = "BasicTable.Alias"
    override def isNamedTable = true
  }
}
