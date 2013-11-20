package scala.slick.lifted

import scala.slick.ast._

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
  def apply(): OverClause = new OverClause(IndexedSeq())

  def partitionBy(columns: Column[_]*): OverWithPartitionBy = new OverWithPartitionBy(columns.map(_.toNode))

  def orderBy(ordered: Ordered): OverWithOrderBy = new OverWithOrderBy(ordered.columns)

  def rowsBetween(start: RowCursor, end: RowCursor): OverWithRowsBetween = new OverWithRowsBetween((start.name, end.name))

  //
  sealed class OverClause(partitionBy: Seq[Node] = Nil, orderBy: Seq[(Node, Ordering)] = Nil, rowsBetween: Option[(String, String)] = None) {
    def ::[T: TypedType](agg: Column[T]): Column[T] = Column.forNode[T](WindowFunc(agg.toNode, partitionBy, orderBy, rowsBetween))
  }

  final class OverWithPartitionBy(partitionBy: Seq[Node]) extends OverClause(partitionBy) {
    def orderBy(ordered: Ordered): OverWithOrderBy = new OverWithOrderBy(ordered.columns, partitionBy)
    def rowsBetween(start: RowCursor, end: RowCursor): OverWithRowsBetween =
      new OverWithRowsBetween((start.name, end.name), partitionBy = partitionBy)
  }
  final class OverWithOrderBy(orderBy: Seq[(Node, Ordering)],
                              partitionBy: Seq[Node] = Nil) extends OverClause(partitionBy, orderBy) {
    def rowsBetween(start: RowCursor, end: RowCursor): OverWithRowsBetween =
      new OverWithRowsBetween((start.name, end.name), orderBy, partitionBy)
  }
  final class OverWithRowsBetween(rowsBetween: (String, String),
                                  orderBy: Seq[(Node, Ordering)] = Nil,
                                  partitionBy: Seq[Node] = Nil) extends OverClause(partitionBy, orderBy, Some(rowsBetween))
}