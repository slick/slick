package slick.lifted

import slick.ast._
import slick.ast.Filter
import slick.model

import scala.collection.mutable.ArrayBuffer

// workaround until deprecated lifted.ForeignKeyAction is removed

/** Marker trait for foreign key and primary key constraints. */
trait Constraint

/** Represents a foreign key. Objects of this type are used internally by Slick.
  * At the user level you generally see `ForeignKeyQuery` objects instead. */
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
      linearizeFieldRefs(pShape.toNode(originalSourceColumns)),
      linearizeFieldRefs(pShape.toNode(originalTargetColumns(targetTableShaped.value))),
      linearizeFieldRefs(pShape.toNode(originalTargetColumns(originalTargetTable))),
      targetTableShaped.value.tableNode,
      pShape
    )

  def linearizeFieldRefs(n: Node): IndexedSeq[Node] = {
    val sels = new ArrayBuffer[Node]
    def f(n: Node): Unit = n match {
      case _: Select | _: Ref | _: TableNode => sels += n
      case _: ProductNode | _: OptionApply | _: GetOrElse | _: TypeMapping | _: ClientSideOp =>
        n.childrenForeach(f)
    }
    f(n)
    sels
  }
}

/** A query that selects data linked by a foreign key. */
class ForeignKeyQuery[E <: AbstractTable[_], U](
    nodeDelegate: Node,
    base: ShapedValue[_ <: E, U],
    val fks: IndexedSeq[ForeignKey],
    targetBaseQuery: Query[E, U, Seq],
    generator: AnonSymbol,
    aliasedValue: E
  ) extends WrappingQuery[E, U, Seq](nodeDelegate, base) with Constraint {

  /** Combine the constraints of this `ForeignKeyQuery` with another one with the
    * same target table, leading to a single instance of the target table which
    * satisfies the constraints of both. */
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

/** An explicit primary key. Simple primary keys can also be represented by `O.PrimaryKey`
  * column options instead. */
case class PrimaryKey(name: String, columns: IndexedSeq[Node]) extends Constraint

/** An index (or foreign key constraint with an implicit index). */
class Index(val name: String, val table: AbstractTable[_], val on: IndexedSeq[Node], val unique: Boolean)
