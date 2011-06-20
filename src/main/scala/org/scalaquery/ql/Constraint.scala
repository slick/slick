package org.scalaquery.ql

import TypeMapper._
import org.scalaquery.util.{Node, BinaryNode}

/**
 * Marker trait for foreign key and primary key constraints.
 */
trait Constraint

case class ForeignKey[TT <: AbstractTable[_], U](name: String, sourceTable: Node, targetTableUnpackable: Unpackable[TT, U], originalTargetTable: TT,
    sourceColumns: Node, targetColumns: TT => ColumnBase[_], onUpdate: ForeignKeyAction, onDelete: ForeignKeyAction)
    extends OperatorColumn[Boolean] with BinaryNode {
  val targetTable = targetTableUnpackable.value
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

case class ForeignKeyQuery[TT <: AbstractTable[_], U](fk: ForeignKey[TT, U]) extends Query[TT, U](fk.targetTableUnpackable, fk :: Nil, Nil, Nil) with Constraint {
  override def toString = "ForeignKeyQuery"
}

case class PrimaryKey(name: String, columns: Node) extends Constraint
