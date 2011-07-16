package org.scalaquery.ql

import org.scalaquery.util.{Node, BinaryNode}

/**
 * Marker trait for foreign key and primary key constraints.
 */
trait Constraint

case class ForeignKey[TT <: AbstractTable[_]](name: String, sourceTable: Node, targetTable: TT, originalTargetTable: TT,
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

class ForeignKeyQuery[TT <: AbstractTable[_]](val fks: List[ForeignKey[TT]]) extends Query[TT](fks.head.targetTable, fks, Nil, Nil) with Constraint {
  override def toString = "ForeignKeyQuery"

  /**
   * Combine the constraints of this ForeignKeyQuery with another one with the
   * same target table, leading to a single instance of the target table which
   * satisfies the constraints of both.
   */
  def & (other: ForeignKeyQuery[TT]) = {
    val tt = fks.head.targetTable
    new ForeignKeyQuery(fks ++ other.fks.map { fk => fk.copy(targetTable = tt) })
  }
}

case class PrimaryKey(name: String, columns: Node) extends Constraint
