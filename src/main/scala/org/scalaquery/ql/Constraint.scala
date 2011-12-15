package org.scalaquery.ql

import org.scalaquery.ast.{SimpleNode, Node}

/**
 * Marker trait for foreign key and primary key constraints.
 */
trait Constraint

final class ForeignKeyData(
  val name: String,
  val sourceTable: Node,
  val linearizedTargetColumnsForOriginalTargetTable: IndexedSeq[Node],
  val onUpdate: ForeignKeyAction,
  val onDelete: ForeignKeyAction
)

final class ForeignKey[TT <: AbstractTable[_], P](unpackp: Unpack[P, _], originalTargetColumns: TT => P,
    val data: ForeignKeyData,
    val left: Node,
    val right: Node,
    val linearizedSourceColumns: IndexedSeq[Node],
    val linearizedTargetColumns: IndexedSeq[Node],
    val targetTable: TT) extends Node {
  override def toString = "ForeignKey " + data.name

  def withTargetTable(tt: TT) =
    new ForeignKey[TT, P](unpackp, originalTargetColumns,
      data,
      left,
      Node(unpackp.reify(originalTargetColumns(tt))),
      linearizedSourceColumns,
      unpackp.linearizer(originalTargetColumns(tt)).getLinearizedNodes,
      tt
    )

  protected[this] def nodeChildGenerators = IndexedSeq(left, right) ++ linearizedSourceColumns ++ linearizedTargetColumns
  protected[this] override def nodeChildNames = IndexedSeq("left", "right") ++
    linearizedSourceColumns.zipWithIndex.map{case (_,i) => "src "+i}  ++
    linearizedTargetColumns.zipWithIndex.map{case (_,i) => "target "+i}

  def nodeMapChildren(f: Node => Node): Node = {
    val l = f(left)
    val r = f(right)
    val linS = nodeMapNodes(linearizedSourceColumns, f)
    val linT = nodeMapNodes(linearizedTargetColumns, f)
    if(l.ne(left) || r.ne(right) || linS.isDefined || linT.isDefined)
      new ForeignKey(unpackp, originalTargetColumns, data, l, r,
        linS.getOrElse(linearizedSourceColumns), linT.getOrElse(linearizedTargetColumns), targetTable)
    else this
  }
}

object ForeignKey {
  def apply[TT <: AbstractTable[_], P](name: String, sourceTable: Node,
      targetTableUnpackable: Unpackable[TT, _], originalTargetTable: TT,
      unpackp: Unpack[P, _], originalSourceColumns: P, originalTargetColumns: TT => P,
      onUpdate: ForeignKeyAction, onDelete: ForeignKeyAction): ForeignKey[TT, P] =
    new ForeignKey[TT, P](unpackp, originalTargetColumns,
      new ForeignKeyData(name, sourceTable,
        unpackp.linearizer(originalTargetColumns(originalTargetTable)).getLinearizedNodes,
        onUpdate, onDelete),
      Node(unpackp.reify(originalSourceColumns)),
      Node(unpackp.reify(originalTargetColumns(targetTableUnpackable.value))),
      unpackp.linearizer(originalSourceColumns).getLinearizedNodes,
      unpackp.linearizer(originalTargetColumns(targetTableUnpackable.value)).getLinearizedNodes,
      targetTableUnpackable.value
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

final case class ForeignKeyQuery[TT <: AbstractTable[_], U](value: Node, fkNodes: IndexedSeq[Node])(val fks: IndexedSeq[ForeignKey[TT, _]], val unpackable: Unpackable[TT, U]) extends Query[TT, U] with Constraint with SimpleNode {
  override def cond = fks
  override def toString = "ForeignKeyQuery"
  protected[this] def nodeChildGenerators = value +: fkNodes
  protected[this] override def nodeChildNames: Iterable[String] = "value" #:: Stream.from(0).map("fk" + _)
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Node = copy[TT, U](value = ch.head, fkNodes = ch.tail)()

  /**
   * Combine the constraints of this ForeignKeyQuery with another one with the
   * same target table, leading to a single instance of the target table which
   * satisfies the constraints of both.
   */
  def & (other: ForeignKeyQuery[TT, U]) = {
    val tt = fks.head.targetTable
    val newFKs = other.fks.map { fk => fk.withTargetTable(tt) }
    copy(fkNodes = fkNodes ++ newFKs.map(Node.apply _))(fks ++ newFKs, unpackable)
  }
}

case class PrimaryKey(name: String, columns: IndexedSeq[Node]) extends Constraint
