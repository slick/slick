package scala.slick.lifted

import scala.slick.ast._

/** `Over` provides a DSL for `OVER clause` of window function in the query language.
  * An optional series of `partitionBy`/`OrderBy`/`rowsBetween` expressions can be
  * chained, e.g.:
  * {{{
  *   salary.avg :: Over.partitionBy(dept).orderBy(dept,salary)
  *                   .rowsFrame(RowCursor.UnboundPreceding, RowCursor.CurrentRow)
  * }}}
  * NOTE: to cooperate with it, you maybe need some aggregate related column extension
  * methods, like [[scala.slick.lifted.ExtensionMethods]] did.
  * */
object Over {
  sealed class RowCursor(val desc: String)

  object RowCursor {
    case object CurrentRow extends RowCursor("current row")
    case class BoundPreceding[T <: AnyVal](value: T) extends RowCursor(s"$value preceding")
    case object UnboundPreceding extends RowCursor("unbounded preceding")
    case class BoundFollowing[T <: AnyVal](value: T) extends RowCursor(s"$value following")
    case object UnboundFollowing extends RowCursor("unbounded following")
  }

  private val ROWS_MODE  = "rows"
  private val RANGE_MODE = "range"

  ///
  def apply(): OverClause = new OverClause(IndexedSeq())

  def partitionBy(columns: Column[_]*): OverWithPartitionBy = new OverWithPartitionBy(columns.map(_.toNode))

  def orderBy(ordered: Ordered): OverWithOrderBy = new OverWithOrderBy(ordered.columns)

  def rowsFrame(start: RowCursor, end: Option[RowCursor] = None): OverWithFrameDef = new OverWithFrameDef((ROWS_MODE, start.desc, end.map(_.desc)))
  
  def rangeFrame(start: RowCursor, end: Option[RowCursor] = None): OverWithFrameDef = new OverWithFrameDef((RANGE_MODE, start.desc, end.map(_.desc)))

  //
  sealed class OverClause(partitionBy: Seq[Node] = Nil, orderBy: Seq[(Node, Ordering)] = Nil,
                          frameDef: Option[(String, String, Option[String])] = None) {
    def ::[T: TypedType](agg: Column[T]): Column[T] = Column.forNode[T](WindowFunc(agg.toNode, partitionBy, orderBy, frameDef))
  }

  final class OverWithPartitionBy(partitionBy: Seq[Node]) extends OverClause(partitionBy) {
    
    def orderBy(ordered: Ordered): OverWithOrderBy = new OverWithOrderBy(ordered.columns, partitionBy)
    
    def rowsFrame(start: RowCursor, end: Option[RowCursor] = None): OverWithFrameDef =
      new OverWithFrameDef((ROWS_MODE, start.desc, end.map(_.desc)), partitionBy = partitionBy)
    
    def rangeFrame(start: RowCursor, end: Option[RowCursor] = None): OverWithFrameDef =
      new OverWithFrameDef((RANGE_MODE, start.desc, end.map(_.desc)), partitionBy = partitionBy)
  }
  
  final class OverWithOrderBy(orderBy: Seq[(Node, Ordering)],
                              partitionBy: Seq[Node] = Nil) extends OverClause(partitionBy, orderBy) {
    def rowsFrame(start: RowCursor, end: Option[RowCursor] = None): OverWithFrameDef =
      new OverWithFrameDef((ROWS_MODE, start.desc, end.map(_.desc)), orderBy, partitionBy)
    
    def rangeFrame(start: RowCursor, end: Option[RowCursor] = None): OverWithFrameDef =
      new OverWithFrameDef((RANGE_MODE, start.desc, end.map(_.desc)), orderBy, partitionBy)
  }
  
  final class OverWithFrameDef(frameDef: (String, String, Option[String]),
                               orderBy: Seq[(Node, Ordering)] = Nil,
                               partitionBy: Seq[Node] = Nil) extends OverClause(partitionBy, orderBy, Some(frameDef))
}