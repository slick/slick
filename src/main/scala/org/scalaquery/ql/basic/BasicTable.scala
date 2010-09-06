package org.scalaquery.ql.basic

import org.scalaquery.ql._
import org.scalaquery.SQueryException
import org.scalaquery.session.{PositionedResult, PositionedParameters}
import org.scalaquery.util.Node

abstract class AbstractBasicTable[T](_tableName: String) extends AbstractTable[T](_tableName) {

  type ProfileType <: BasicProfile

  val O: BasicColumnOptions = BasicColumnOptions

  def column[C : TypeMapper](n: String, options: ColumnOption[C, ProfileType]*) = new NamedColumn[C](Node(this), n, options:_*)

  def createFinderBy[P](f: (this.type => NamedColumn[P]))(implicit profile: BasicProfile, tm: TypeMapper[P]): BasicQueryTemplate[P,T] = {
    import profile.Implicit._
    Parameters[P](tm).flatMap(p => Query(this).where(t => ColumnOps.Is(f(t.asInstanceOf[AbstractBasicTable.this.type]), p)))(profile)
  }

  def innerJoin[U <: TableBase[_]](other: U) = new JoinBase[this.type, U](this, other, Join.Inner)
  def leftJoin[U <: TableBase[_]](other: U) = new JoinBase[this.type, U](this, other, Join.Left)
  def rightJoin[U <: TableBase[_]](other: U) = new JoinBase[this.type, U](this, other, Join.Right)
  def outerJoin[U <: TableBase[_]](other: U) = new JoinBase[this.type, U](this, other, Join.Outer)

  def ddl(implicit profile: ProfileType): DDL = profile.buildTableDDL(this)
}

abstract class BasicTable[T](_tableName: String) extends AbstractBasicTable[T](_tableName) {
  type ProfileType = BasicProfile
}
