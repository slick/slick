package scala.slick.lifted

import scala.slick.ast._
import scala.slick.ast.Filter
import scala.slick.model // workaround until deprecated lifted.ForeignKeyAction is removed

/**
 * Marker trait for foreign key and primary key constraints.
 */
trait Constraint

final class ForeignKey( //TODO Simplify this mess!
    val name: String,
    val sourceTable: Node,
    val onUpdate: model.ForeignKeyAction,
    val onDelete: model.ForeignKeyAction,
    val sourceColumns: Any,
    val targetColumns: Any => Any,
    val linearizedSourceColumns: IndexedSeq[Node],
    val linearizedTargetColumns: IndexedSeq[Node],
    val linearizedTargetColumnsForOriginalTargetTable: IndexedSeq[Node],
    val targetTable: TableNode,
    val columnsShape: Shape[_ <: FlatShapeLevel, _, _, _])

object ForeignKey {
  def apply[TT <: AbstractTable[_], P](
      name: String,
      sourceTable: Node,
      targetTableShaped: ShapedValue[TT, _],
      originalTargetTable: TT,
      pShape: Shape[_ <: FlatShapeLevel, P, _, _],
      originalSourceColumns: P,
      originalTargetColumns: TT => P,
      onUpdate: model.ForeignKeyAction,
      onDelete: model.ForeignKeyAction
    ): ForeignKey = new ForeignKey(
      name,
      sourceTable,
      onUpdate,
      onDelete,
      originalSourceColumns,
      originalTargetColumns.asInstanceOf[Any => Any],
      ExtraUtil.linearizeFieldRefs(pShape.toNode(originalSourceColumns)),
      ExtraUtil.linearizeFieldRefs(pShape.toNode(originalTargetColumns(targetTableShaped.value))),
      ExtraUtil.linearizeFieldRefs(pShape.toNode(originalTargetColumns(originalTargetTable))),
      targetTableShaped.value.tableNode,
      pShape
    )
}

class ForeignKeyQuery[E <: AbstractTable[_], U](
    nodeDelegate: Node,
    base: ShapedValue[_ <: E, U],
    val fks: IndexedSeq[ForeignKey],
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
    val conditions = newFKs.map { fk =>
      val sh = fk.columnsShape.asInstanceOf[Shape[FlatShapeLevel, Any, Any, Any]]
      Library.==.typed[Boolean](sh.toNode(fk.targetColumns(aliasedValue)), sh.toNode(fk.sourceColumns))
    }.reduceLeft[Node]((a, b) => Library.And.typed[Boolean](a, b))
    val newDelegate = Filter.ifRefutable(generator, targetBaseQuery.toNode, conditions)
    new ForeignKeyQuery[E, U](newDelegate, base, newFKs, targetBaseQuery, generator, aliasedValue)
  }
}

case class PrimaryKey(name: String, columns: IndexedSeq[Node]) extends Constraint

/**
 * An index (or foreign key constraint with an implicit index).
 */
class Index(val name: String, val table: AbstractTable[_], val on: IndexedSeq[Node], val unique: Boolean)
