package org.scalaquery.ql

import org.scalaquery.util.{Node, BinaryNode}

/**
 * Marker trait for foreign key and primary key constraints.
 */
trait Constraint

class ForeignKey[TT <: AbstractTable[_], P](val name: String, val sourceTable: Node,
    val targetTableUnpackable: Unpackable[TT, _], originalTargetTable: TT,
    unpackp: Unpack[P, _],
    originalSourceColumns: P, originalTargetColumns: TT => P,
    val onUpdate: ForeignKeyAction, val onDelete: ForeignKeyAction)
    extends OperatorColumn[Boolean] with BinaryNode {
  val targetTable = targetTableUnpackable.value
  val left = Node(unpackp.reify(originalSourceColumns))
  val right = Node(unpackp.reify(originalTargetColumns(targetTable)))
  override def toString = "ForeignKey " + name
  def targetColumnsForOriginalTargetTable = Node(unpackp.reify(originalTargetColumns(originalTargetTable)))
  def linearizedSourceColumns = unpackp.linearizer(originalSourceColumns).getLinearizedNodes
  def linearizedTargetColumns = unpackp.linearizer(originalTargetColumns(targetTable)).getLinearizedNodes
  def linearizedTargetColumnsForOriginalTargetTable = unpackp.linearizer(originalTargetColumns(originalTargetTable)).getLinearizedNodes
  def withTargetTableUnpackable(targetTableUnpackable: Unpackable[TT, _]) =
    new ForeignKey[TT, P](name, sourceTable, targetTableUnpackable, originalTargetTable,
      unpackp, originalSourceColumns, originalTargetColumns, onUpdate, onDelete)
}

sealed abstract class ForeignKeyAction(val action: String)

object ForeignKeyAction {
  case object Cascade extends ForeignKeyAction("CASCADE")
  case object Restrict extends ForeignKeyAction("RESTRICT")
  case object NoAction extends ForeignKeyAction("NO ACTION")
  case object SetNull extends ForeignKeyAction("SET NULL")
  case object SetDefault extends ForeignKeyAction("SET DEFAULT")
}

class ForeignKeyQuery[TT <: AbstractTable[_], U](val fks: List[ForeignKey[TT, _]], override val unpackable: Unpackable[TT, U]) extends Query[TT, U](unpackable, fks, Nil, Nil) with Constraint {
  override def toString = "ForeignKeyQuery"

  /**
   * Combine the constraints of this ForeignKeyQuery with another one with the
   * same target table, leading to a single instance of the target table which
   * satisfies the constraints of both.
   */
  def & (other: ForeignKeyQuery[TT, U]) = {
    val tt = fks.head.targetTableUnpackable
    new ForeignKeyQuery(fks ++ other.fks.map { fk => fk.withTargetTableUnpackable(tt) }, unpackable)
  }
}

case class PrimaryKey(name: String, columns: IndexedSeq[Node]) extends Constraint
