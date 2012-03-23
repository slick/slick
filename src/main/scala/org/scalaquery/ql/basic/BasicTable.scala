package org.scalaquery.ql.basic

import org.scalaquery.ql._
import org.scalaquery.SQueryException
import org.scalaquery.session.{PositionedResult, PositionedParameters}
import org.scalaquery.ast.Node

abstract class AbstractBasicTable[T](_schemaName: Option[String], _tableName: String) extends AbstractTable[T](_schemaName, _tableName) {

  type ProfileType <: BasicProfile

  val O: BasicColumnOptions = BasicColumnOptions

  def column[C : TypeMapper](n: String, options: ColumnOption[C, ProfileType]*) = NamedColumn[C](Node(this), n, options)

  def createFinderBy[P](f: (this.type => NamedColumn[P]))(implicit driver: BasicProfile, tm: TypeMapper[P]): BasicQueryTemplate[P,T] = {
    import driver.Implicit.{scalaQueryDriver => _, _}
    for {
      param <- Parameters[P]
      table <- this if ColumnOps.Is(Node(f(this)), Node(param))
    } yield table
  }

  def innerJoin[U <: TableBase[_]](other: U) = new JoinBase[this.type, U](this, other, Join.Inner)
  def leftJoin[U <: TableBase[_]](other: U) = new JoinBase[this.type, U](this, other, Join.Left)
  def rightJoin[U <: TableBase[_]](other: U) = new JoinBase[this.type, U](this, other, Join.Right)
  def outerJoin[U <: TableBase[_]](other: U) = new JoinBase[this.type, U](this, other, Join.Outer)

  def ddl(implicit profile: ProfileType): DDL = profile.buildTableDDL(this)
}

abstract class BasicTable[T](_schemaName: Option[String], _tableName: String) extends AbstractBasicTable[T](_schemaName, _tableName) {
  def this(_tableName: String) = this(None, _tableName)
  type ProfileType = BasicProfile
}
