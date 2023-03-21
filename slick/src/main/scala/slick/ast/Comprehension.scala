package slick.ast

import slick.ast.TypeUtil.typeToTypeUtil
import slick.ast.Util.*
import slick.util.ConstArray

/** A SQL comprehension */
final case class Comprehension[+Fetch <: Option[Node]](sym: TermSymbol,
                                                       from: Node,
                                                       select: Node,
                                                       where: Option[Node] = None,
                                                       groupBy: Option[Node] = None,
                                                       orderBy: ConstArray[(Node, Ordering)] = ConstArray.empty,
                                                       having: Option[Node] = None,
                                                       distinct: Option[Node] = None,
                                                       fetch: Fetch = None,
                                                       offset: Option[Node] = None,
                                                       forUpdate: Boolean = false) extends DefNode {
  type Self = Comprehension[Option[Node]]
  override def self = this
  lazy val children =
    (ConstArray.newBuilder() +
      from +
      select ++
      where ++
      groupBy ++
      orderBy.map(_._1) ++
      having ++
      distinct ++
      fetch ++
      offset)
      .result
  override def childNames: Seq[String] =
    Seq("from " + sym, "select") ++
      where.map(_ => "where") ++
      groupBy.map(_ => "groupBy") ++
      orderBy.map("orderBy " + _._2).toSeq ++
      having.map(_ => "having") ++
      distinct.map(_ => "distinct") ++
      fetch.map(_ => "fetch") ++
      offset.map(_ => "offset")
  protected[this] def rebuild(ch: ConstArray[Node]) = {
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
    val distinctOffset = havingOffset + newHaving.length
    val newDistinct = ch.slice(distinctOffset, distinctOffset + distinct.productArity)
    val fetchOffset = distinctOffset + newDistinct.length
    val newFetch = ch.slice(fetchOffset, fetchOffset + fetch.productArity)
    val offsetOffset = fetchOffset + newFetch.length
    val newOffset = ch.slice(offsetOffset, offsetOffset + offset.productArity)
    copy(
      from = newFrom,
      select = newSelect,
      where = newWhere.headOption,
      groupBy = newGroupBy.headOption,
      orderBy = orderBy.zip(newOrderBy).map { case ((_, o), n) => (n, o) },
      having = newHaving.headOption,
      distinct = newDistinct.headOption,
      fetch = newFetch.headOption,
      offset = newOffset.headOption
    )
  }
  def generators = ConstArray((sym, from))
  protected[this] def rebuildWithSymbols(gen: ConstArray[TermSymbol]): Comprehension[Fetch] = copy(sym = gen.head)
  def withInferredType(scope: Type.Scope, typeChildren: Boolean): Self = {
    // Assign type to "from" Node and compute the resulting scope
    val f2 = from.infer(scope, typeChildren)
    val genScope = scope + (sym -> f2.nodeType.asCollectionType.elementType)
    // Assign types to "select", "where", "groupBy", "orderBy", "having", "distinct", "fetch" and "offset" Nodes
    val s2 = select.infer(genScope, typeChildren)
    val w2 = mapOrNone(where)(_.infer(genScope, typeChildren))
    val g2 = mapOrNone(groupBy)(_.infer(genScope, typeChildren))
    val o = orderBy.map(_._1)
    val o2 = o.endoMap(_.infer(genScope, typeChildren))
    val h2 = mapOrNone(having)(_.infer(genScope, typeChildren))
    val distinct2 = mapOrNone(distinct)(_.infer(genScope, typeChildren))
    val fetch2 = mapOrNone(fetch)(_.infer(genScope, typeChildren))
    val offset2 = mapOrNone(offset)(_.infer(genScope, typeChildren))
    // Check if the nodes changed
    val same = (f2 eq from) && (s2 eq select) && w2.isEmpty && g2.isEmpty && (o2 eq o) && h2.isEmpty &&
      distinct2.isEmpty && fetch2.isEmpty && offset2.isEmpty
    val newType =
      if(!hasType) CollectionType(f2.nodeType.asCollectionType.cons, s2.nodeType.asCollectionType.elementType)
      else nodeType
    if(same && newType == nodeType) this else {
      copy(
        from = f2,
        select = s2,
        where = w2.orElse(where),
        groupBy = g2.orElse(groupBy),
        orderBy = if(o2 eq o) orderBy else orderBy.zip(o2).map { case ((_, o), n) => (n, o) },
        having = h2.orElse(having),
        distinct = distinct2.orElse(distinct),
        fetch = fetch2.orElse(fetch),
        offset = offset2.orElse(offset)
      ) :@ newType
    }
  }
}
object Comprehension {
  type Base = Comprehension[Option[Node]]
}

/** The row_number window function */
final case class RowNumber(by: ConstArray[(Node, Ordering)] = ConstArray.empty) extends SimplyTypedNode {
  type Self = RowNumber
  override def self = this
  override def buildType: ScalaNumericType[Long] = ScalaBaseType.longType
  lazy val children = by.map(_._1)
  protected[this] def rebuild(ch: ConstArray[Node]) =
    copy(by = by.zip(ch).map{ case ((_, o), n) => (n, o) })
  override def childNames: IndexedSeq[String] = by.zipWithIndex.map("by" + _._2).toSeq
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = "")
}
