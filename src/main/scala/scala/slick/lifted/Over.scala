package scala.slick.lifted

import scala.slick.ast._
import scala.collection.mutable.ArrayBuffer

/** `Over` provides a DSL for `OVER clause` of window function in the query language.
  * An optional series of `partitionBy`/`OrderBy`/`rowsBetween` expressions can be
  * chained, e.g.:
  * {{{
  *   salary.avg :: Over.partitionBy(dept).orderBy(dept,salary)
  *                   .rowsBetween(RowCursor.Preceding, RowCursor.Current)
  * }}}
  * NOTE: to cooperate with it, you maybe need some aggregate related column extension
  * methods, like [[scala.slick.lifted.ExtensionMethods]] did.
  * */
object Over {
  sealed case class RowCursor(name: String)

  object RowCursor {
    object Current extends RowCursor("current row")
    object Preceding extends RowCursor("unbounded preceding")
    object Following extends RowCursor("unbounded following")
  }

  ///
  def apply(): OverClause = new OverClause(None)

  def partitionBy(expr: Column[_]*): OverWithPartitionBy = {
    val partitionByNode = LiteralNode(" partition by ") +: sepNodes(expr.map(_.toNode), ",")
    new OverWithPartitionBy(ProductNode(partitionByNode))
  }

  def orderBy(expr: Column[_]*)(prevNode: Option[Node] = None): OverWithOrderBy = {
    val orderByNode = LiteralNode(" order by ") +: sepNodes(expr.map(_.toNode), ",")
    prevNode.map(n => new OverWithOrderBy(ProductNode(n +: orderByNode)))
      .getOrElse(new OverWithOrderBy(ProductNode(orderByNode)))
  }

  def rowsBetween(start: RowCursor, end: RowCursor)(prevNode: Option[Node] = None): OverWithRowsBetween = {
    val rowsBetweenNode = LiteralNode(s" rows between ${start.name} and ${end.name} ")
    prevNode.map(n => new OverWithRowsBetween(ProductNode(n +: rowsBetweenNode +: Nil)))
      .getOrElse(new OverWithRowsBetween(rowsBetweenNode))
  }

  private def sepNodes(nodes: Seq[Node], sep: String): Seq[Node] = {
    val result = ArrayBuffer[Node]()
    var first = true
    for(node <- nodes) {
      if(first) first = false else result += LiteralNode(sep)
      result += node
    }
    result.toSeq
  }

  //
  sealed class OverClause(clause: Option[Node]) {
    def ::[T: TypedType](agg: Column[T]): Column[T] = {
      clause.map(n => Column.forNode[T](ProductNode(agg.toNode +: LiteralNode(" over(") +: n +: LiteralNode(") ") +: Nil)))
        .getOrElse(Column.forNode[T](ProductNode(agg.toNode +: LiteralNode(" over() ") +: Nil)))
    }
  }
  final class OverWithPartitionBy(clause: Node) extends OverClause(Some(clause)) {
    def orderBy(expr: Column[_]*): OverWithOrderBy = Over.orderBy(expr: _*)(Some(clause))
    def rowsBetween(start: RowCursor, end: RowCursor): OverWithRowsBetween = Over.rowsBetween(start, end)(Some(clause))
  }
  final class OverWithOrderBy(clause: Node) extends OverClause(Some(clause)) {
    def rowsBetween(start: RowCursor, end: RowCursor): OverWithRowsBetween = Over.rowsBetween(start, end)(Some(clause))
  }
  final class OverWithRowsBetween(clause: Node) extends OverClause(Some(clause))
}