package scala.slick.ast

/** A SQL comprehension */
case class Comprehension(from: Seq[(Symbol, Node)] = Seq.empty, where: Seq[Node] = Seq.empty, groupBy: Option[Node] = None, orderBy: Seq[(Node, Ordering)] = Seq.empty, select: Option[Node] = None, fetch: Option[Long] = None, offset: Option[Long] = None) extends DefNode {
  val nodeChildren = from.map(_._2) ++ where ++ groupBy ++ orderBy.map(_._1) ++ select
  override def nodeChildNames =
    from.map("from " + _._1) ++
    where.zipWithIndex.map("where" + _._2) ++
    groupBy.map(_ => "groupBy") ++
    orderBy.map("orderBy " + _._2) ++
    select.map(_ => "select")
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]) = {
    val newFrom = ch.slice(0, from.length)
    val whereOffset = newFrom.length
    val newWhere = ch.slice(whereOffset, whereOffset + where.length)
    val groupByOffset = whereOffset + newWhere.length
    val newGroupBy = ch.slice(groupByOffset, groupByOffset + (if(groupBy.isDefined) 1 else 0))
    val orderByOffset = groupByOffset + newGroupBy.length
    val newOrderBy = ch.slice(orderByOffset, orderByOffset + orderBy.length)
    val selectOffset = orderByOffset + newOrderBy.length
    val newSelect = ch.slice(selectOffset, selectOffset + (if(select.isDefined) 1 else 0))
    copy(
      from = (from, newFrom).zipped.map { case ((s, _), n) => (s, n) },
      where = newWhere,
      groupBy = if(newGroupBy.isEmpty) None else Some(newGroupBy.head),
      orderBy = (orderBy, newOrderBy).zipped.map { case ((_, o), n) => (n, o) },
      select = if(newSelect.isEmpty) None else Some(newSelect.head)
    )
  }
  def nodeGenerators = from
  override def toString = "Comprehension(fetch = "+fetch+", offset = "+offset+")"

  protected[this] def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) =
    copy(from = (from, gen).zipped.map { case ((_, n), s) => (s, n) })
}

/** The row_number window function */
final case class RowNumber(by: Seq[(Node, Ordering)] = Seq.empty) extends Node {
  lazy val nodeChildren = by.map(_._1)
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]) =
    copy(by = by.zip(ch).map{ case ((_, o), n) => (n, o) })
  override def nodeChildNames = by.zipWithIndex.map("by" + _._2)
  override def toString = "RowNumber"
}
