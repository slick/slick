package slick.ast

import TypeUtil.typeToTypeUtil
import Util._

/** A SQL comprehension */
final case class Comprehension(sym: Symbol, from: Node, select: Option[Node], where: Seq[Node] = Seq.empty, groupBy: Option[Node] = None, orderBy: Seq[(Node, Ordering)] = Seq.empty, having: Seq[Node] = Seq.empty, fetch: Option[Node] = None, offset: Option[Node] = None) extends DefNode {
  type Self = Comprehension
  val nodeChildren = Seq(from) ++ select ++ where ++ groupBy ++ orderBy.map(_._1) ++ having ++ fetch ++ offset
  override def nodeChildNames =
    Seq("from "+sym) ++
    select.map(_ => "select") ++
    where.zipWithIndex.map("where" + _._2) ++
    groupBy.map(_ => "groupBy") ++
    orderBy.map("orderBy " + _._2) ++
    having.zipWithIndex.map("having" + _._2) ++
    fetch.map(_ => "fetch") ++
    offset.map(_ => "offset")
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]) = {
    val newFrom = ch(0)
    val newSelect = ch.slice(1, 1 + (if(select.isDefined) 1 else 0))
    val whereOffset = 1 + newSelect.length
    val newWhere = ch.slice(whereOffset, whereOffset + where.length)
    val groupByOffset = whereOffset + newWhere.length
    val newGroupBy = ch.slice(groupByOffset, groupByOffset + (if(groupBy.isDefined) 1 else 0))
    val orderByOffset = groupByOffset + newGroupBy.length
    val newOrderBy = ch.slice(orderByOffset, orderByOffset + orderBy.length)
    val havingOffset = orderByOffset + newOrderBy.length
    val newHaving = ch.slice(havingOffset, havingOffset + having.length)
    val fetchOffset = havingOffset + newHaving.length
    val newFetch = ch.slice(fetchOffset, fetchOffset + (if(fetch.isDefined) 1 else 0))
    val offsetOffset = fetchOffset + newFetch.length
    val newOffset = ch.slice(offsetOffset, offsetOffset + (if(offset.isDefined) 1 else 0))
    copy(
      from = newFrom,
      select = if(newSelect.isEmpty) None else Some(newSelect.head),
      where = newWhere,
      groupBy = if(newGroupBy.isEmpty) None else Some(newGroupBy.head),
      orderBy = (orderBy, newOrderBy).zipped.map { case ((_, o), n) => (n, o) },
      having = newHaving,
      fetch = if(newFetch.isEmpty) None else Some(newFetch.head),
      offset = if(newOffset.isEmpty) None else Some(newOffset.head)
    )
  }
  def nodeGenerators = Seq((sym, from))
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = "")
  protected[this] def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(sym = gen.head)
  def nodeWithComputedType2(scope: SymbolScope, typeChildren: Boolean, retype: Boolean): Self = {
    // Assign type to "from" Node and compute the resulting scope
    val f2 = from.nodeWithComputedType(scope, typeChildren, retype)
    val genScope = scope + (sym -> f2.nodeType.asCollectionType.elementType)
    // Assign types to "select", "where", "groupBy", "orderBy", "having", "fetch" and "offset" Nodes
    val s2 = mapOrNone(select)(_.nodeWithComputedType(genScope, typeChildren, retype))
    val w2 = mapOrNone(where)(_.nodeWithComputedType(genScope, typeChildren, retype))
    val g2 = mapOrNone(groupBy)(_.nodeWithComputedType(genScope, typeChildren, retype))
    val o = orderBy.map(_._1)
    val o2 = mapOrNone(o)(_.nodeWithComputedType(genScope, typeChildren, retype))
    val h2 = mapOrNone(having)(_.nodeWithComputedType(genScope, typeChildren, retype))
    val fetch2 = mapOrNone(fetch)(_.nodeWithComputedType(genScope, typeChildren, retype))
    val offset2 = mapOrNone(offset)(_.nodeWithComputedType(genScope, typeChildren, retype))
    // Check if the nodes changed
    val same = (f2 eq from) && s2.isEmpty && w2.isEmpty && g2.isEmpty && o2.isEmpty && h2.isEmpty && fetch2.isEmpty && offset2.isEmpty
    val newSel = s2.map(_.headOption).getOrElse(select)
    val newType =
      if(!nodeHasType || retype)
        CollectionType(f2.nodeType.asCollectionType.cons, newSel.get.nodeType.asCollectionType.elementType)
      else nodeType
    if(same && newType == nodeType) this else {
      // Compute result type
      copy(
        from = f2,
        select = newSel,
        where = w2.getOrElse(where),
        groupBy = g2.map(_.headOption).getOrElse(groupBy),
        orderBy = o2.map(o2 => (orderBy, o2).zipped.map { case ((_, o), n) => (n, o) }).getOrElse(orderBy),
        having = h2.getOrElse(having),
        fetch = fetch2.map(_.headOption).getOrElse(fetch),
        offset = offset2.map(_.headOption).getOrElse(offset)
      ).nodeTyped(newType)
    }
  }
}

/** The row_number window function */
final case class RowNumber(by: Seq[(Node, Ordering)] = Seq.empty) extends TypedNode {
  type Self = RowNumber
  def tpe = ScalaBaseType.longType
  lazy val nodeChildren = by.map(_._1)
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]) =
    copy(by = by.zip(ch).map{ case ((_, o), n) => (n, o) })
  override def nodeChildNames = by.zipWithIndex.map("by" + _._2)
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = "")
}
