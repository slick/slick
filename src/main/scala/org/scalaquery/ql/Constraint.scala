package org.scalaquery.ql

import org.scalaquery.util.{Node, BinaryNode}

/**
 * Marker trait for foreign key and primary key constraints.
 */
trait Constraint

case class ForeignKey[TT <: AbstractTable[_]](name: String, sourceTable: Node, targetTableUnpackable: Unpackable[TT, _], originalTargetTable: TT,
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

class ForeignKeyQuery[TT <: AbstractTable[_], U](val fks: List[ForeignKey[TT]], override val unpackable: Unpackable[TT, U]) extends Query[TT, U](unpackable, fks, Nil, Nil) with Constraint {
  override def toString = "ForeignKeyQuery"

  /**
   * Combine the constraints of this ForeignKeyQuery with another one with the
   * same target table, leading to a single instance of the target table which
   * satisfies the constraints of both.
   */
  def & (other: ForeignKeyQuery[TT, U]) = {
    val tt = fks.head.targetTableUnpackable
    new ForeignKeyQuery(fks ++ other.fks.map { fk => fk.copy(targetTableUnpackable = tt) }, unpackable)
  }
}

case class PrimaryKey(name: String, columns: Node) extends Constraint
