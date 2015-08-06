package slick.ast

import TypeUtil.typeToTypeUtil
import Util._

/** A SQL comprehension */
final case class Comprehension(sym: TermSymbol, from: Node, select: Node, where: Option[Node] = None,
                               groupBy: Option[Node] = None, orderBy: Seq[(Node, Ordering)] = Seq.empty,
                               having: Option[Node] = None,
                               fetch: Option[Node] = None, offset: Option[Node] = None) extends DefNode {
  type Self = Comprehension
  val children = Seq(from, select) ++ where ++ groupBy ++ orderBy.map(_._1) ++ having ++ fetch ++ offset
  override def childNames =
    Seq("from "+sym, "select") ++
    where.map(_ => "where") ++
    groupBy.map(_ => "groupBy") ++
    orderBy.map("orderBy " + _._2) ++
    having.map(_ => "having") ++
    fetch.map(_ => "fetch") ++
    offset.map(_ => "offset")
  protected[this] def rebuild(ch: IndexedSeq[Node]) = {
    val newFrom = ch(0)
    val newSelect = ch(1)
    val whereOffset = 2
    val newWhere = ch.slice(whereOffset, whereOffset + where.productArity)
    val groupByOffset = whereOffset + newWhere.length
    val newGroupBy = ch.slice(groupByOffset, groupByOffset + groupBy.productArity)
    val orderByOffset = groupByOffset + newGroupBy.length
    val newOrderBy = ch.slice(orderByOffset, orderByOffset + orderBy.length)
    val havingOffset = orderByOffset + newOrderBy.length
    val newHaving = ch.slice(havingOffset, havingOffset + having.productArity)
    val fetchOffset = havingOffset + newHaving.length
    val newFetch = ch.slice(fetchOffset, fetchOffset + fetch.productArity)
    val offsetOffset = fetchOffset + newFetch.length
    val newOffset = ch.slice(offsetOffset, offsetOffset + offset.productArity)
    copy(
      from = newFrom,
      select = newSelect,
      where = newWhere.headOption,
      groupBy = newGroupBy.headOption,
      orderBy = (orderBy, newOrderBy).zipped.map { case ((_, o), n) => (n, o) },
      having = newHaving.headOption,
      fetch = newFetch.headOption,
      offset = newOffset.headOption
    )
  }
  def generators = Seq((sym, from))
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = "")
  protected[this] def rebuildWithSymbols(gen: IndexedSeq[TermSymbol]) = copy(sym = gen.head)
  def withInferredType(scope: Type.Scope, typeChildren: Boolean): Self = {
    // Assign type to "from" Node and compute the resulting scope
    val f2 = from.infer(scope, typeChildren)
    val genScope = scope + (sym -> f2.nodeType.asCollectionType.elementType)
    // Assign types to "select", "where", "groupBy", "orderBy", "having", "fetch" and "offset" Nodes
    val s2 = select.infer(genScope, typeChildren)
    val w2 = mapOrNone(where)(_.infer(genScope, typeChildren))
    val g2 = mapOrNone(groupBy)(_.infer(genScope, typeChildren))
    val o = orderBy.map(_._1)
    val o2 = mapOrNone(o)(_.infer(genScope, typeChildren))
    val h2 = mapOrNone(having)(_.infer(genScope, typeChildren))
    val fetch2 = mapOrNone(fetch)(_.infer(genScope, typeChildren))
    val offset2 = mapOrNone(offset)(_.infer(genScope, typeChildren))
    // Check if the nodes changed
    val same = (f2 eq from) && (s2 eq select) && w2.isEmpty && g2.isEmpty && o2.isEmpty && h2.isEmpty && fetch2.isEmpty && offset2.isEmpty
    val newType =
      if(!hasType) CollectionType(f2.nodeType.asCollectionType.cons, s2.nodeType.asCollectionType.elementType)
      else nodeType
    if(same && newType == nodeType) this else {
      copy(
        from = f2,
        select = s2,
        where = w2.map(_.headOption).getOrElse(where),
        groupBy = g2.map(_.headOption).getOrElse(groupBy),
        orderBy = o2.map(o2 => (orderBy, o2).zipped.map { case ((_, o), n) => (n, o) }).getOrElse(orderBy),
        having = h2.map(_.headOption).getOrElse(having),
        fetch = fetch2.map(_.headOption).getOrElse(fetch),
        offset = offset2.map(_.headOption).getOrElse(offset)
      ) :@ newType
    }
  }
}

/** The row_number window function */
final case class RowNumber(by: Seq[(Node, Ordering)] = Seq.empty) extends SimplyTypedNode {
  type Self = RowNumber
  def buildType = ScalaBaseType.longType
  lazy val children = by.map(_._1)
  protected[this] def rebuild(ch: IndexedSeq[Node]) =
    copy(by = by.zip(ch).map{ case ((_, o), n) => (n, o) })
  override def childNames = by.zipWithIndex.map("by" + _._2)
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = "")
}
