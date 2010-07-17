package com.novocode.squery.combinator

import TypeMapper._

case class ForeignKey[TT <: AbstractTable.T_](name: String, sourceTable: Node, targetTable: TT, originalTargetTable: TT,
    sourceColumns: Node, targetColumns: TT => ColumnBase[_], onUpdate: ForeignKeyAction, onDelete: ForeignKeyAction)
    extends OperatorColumn[Boolean] with BinaryNode {
  val left = Node(sourceColumns)
  val right = Node(targetColumns(targetTable))
  override def toString = "ForeignKey " + name
  def targetColumnsForOriginalTargetTable = targetColumns(originalTargetTable)
}

sealed abstract class ForeignKeyAction(val action: String)

object ForeignKeyAction {
  case object Cascade extends ForeignKeyAction("CASCADE")
  case object Restrict extends ForeignKeyAction("RESTRICT")
  case object NoAction extends ForeignKeyAction("NO ACTION")
  case object SetNull extends ForeignKeyAction("SET NULL")
  case object SetDefault extends ForeignKeyAction("SET DEFAULT")
}

case class ForeignKeyQuery[TT <: AbstractTable.T_](fk: ForeignKey[TT]) extends Query[TT](fk.targetTable, fk :: Nil, Nil, Nil) {
  override def toString = "ForeignKeyQuery"
}
