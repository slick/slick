package scala.slick.lifted

import scala.slick.ast._
import scala.slick.ast.Filter


/**
 * Marker trait for foreign key and primary key constraints.
 */
trait Constraint

final class ForeignKey[TT <: TableNode, P]( //TODO Simplify this mess!
    val name: String,
    val sourceTable: Node,
    val onUpdate: ForeignKeyAction,
    val onDelete: ForeignKeyAction,
    val sourceColumns: P,
    val targetColumns: TT => P,
    val linearizedSourceColumns: IndexedSeq[Node],
    val linearizedTargetColumns: IndexedSeq[Node],
    val linearizedTargetColumnsForOriginalTargetTable: IndexedSeq[Node],
    val targetTable: TT)

object ForeignKey {
  def apply[TT <: TableNode, P](
      name: String,
      sourceTable: Node,
      targetTableShaped: ShapedValue[TT, _],
      originalTargetTable: TT,
      pShape: Shape[P, _, _],
      originalSourceColumns: P,
      originalTargetColumns: TT => P,
      onUpdate: ForeignKeyAction,
      onDelete: ForeignKeyAction
    ): ForeignKey[TT, P] = new ForeignKey[TT, P](
      name,
      sourceTable,
      onUpdate,
      onDelete,
      originalSourceColumns,
      originalTargetColumns,
      pShape.linearizer(originalSourceColumns).narrowedLinearizer.getLinearizedNodes,
      pShape.linearizer(originalTargetColumns(targetTableShaped.value)).narrowedLinearizer.getLinearizedNodes,
      pShape.linearizer(originalTargetColumns(originalTargetTable)).narrowedLinearizer.getLinearizedNodes,
      targetTableShaped.value
    )
}

sealed abstract class ForeignKeyAction(val action: String)

object ForeignKeyAction {
  case object Cascade extends ForeignKeyAction("CASCADE")
  case object Restrict extends ForeignKeyAction("RESTRICT")
  case object NoAction extends ForeignKeyAction("NO ACTION")
  case object SetNull extends ForeignKeyAction("SET NULL")
  case object SetDefault extends ForeignKeyAction("SET DEFAULT")
}

class ForeignKeyQuery[E <: TableNode, U](
    nodeDelegate: Node,
    base: ShapedValue[_ <: E, U],
    val fks: IndexedSeq[ForeignKey[E, _]],
    targetBaseQuery: Query[E, U],
    generator: AnonSymbol,
    aliasedValue: E
  ) extends WrappingQuery[E, U](nodeDelegate, base) with Constraint {

  /**
   * Combine the constraints of this ForeignKeyQuery with another one with the
   * same target table, leading to a single instance of the target table which
   * satisfies the constraints of both.
   */
  def & (other: ForeignKeyQuery[E, U]): ForeignKeyQuery[E, U] = {
    val newFKs = fks ++ other.fks
    val conditions =
      newFKs.map(fk => Library.==(Node(fk.targetColumns(aliasedValue)), Node(fk.sourceColumns))).
        reduceLeft[Node]((a, b) => Library.And(a, b))
    val newDelegate = Filter(generator, Node(targetBaseQuery), conditions)
    new ForeignKeyQuery[E, U](newDelegate, base, newFKs, targetBaseQuery, generator, aliasedValue)
  }
}

case class PrimaryKey(name: String, columns: IndexedSeq[Node]) extends Constraint

/**
 * An index (or foreign key constraint with an implicit index).
 */
class Index(val name: String, val table: TableNode, val on: IndexedSeq[Node], val unique: Boolean)
