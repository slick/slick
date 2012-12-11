package scala.slick.ast

import TypeUtil.typeToTypeUtil
import Util._

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
  def nodeWithComputedType(scope: SymbolScope): Node = {
    // Assign types to all "from" Nodes and compute the resulting scope
    val (genScope, f2) = from.foldLeft((scope, Vector.empty[Node])) { case ((sc, n2s), (s, n)) =>
      val n2 = n.nodeWithComputedType(sc)
      val sc2 = sc + (s -> n2.nodeType.asCollectionType.elementType)
      (sc2, n2s :+ n2)
    }
    // Assign types to "where", "groupBy", "orderBy" and "select" Nodes
    val w2 = mapOrNone(where)(_.nodeWithComputedType(genScope))
    val g2 = mapOrNone(groupBy)(_.nodeWithComputedType(genScope))
    val o = orderBy.map(_._1)
    val o2 = mapOrNone(o)(_.nodeWithComputedType(genScope))
    val s2 = mapOrNone(select)(_.nodeWithComputedType(genScope))
    // Check if the nodes changed
    val same = (from, f2).zipped.map(_._2 eq _).forall(identity) &&
      w2.isEmpty && g2.isEmpty && o2.isEmpty && s2.isEmpty
    val newSel = s2.map(_.headOption).getOrElse(select)
    val newType = (newSel match {
      case Some(sel) => sel.nodeType
      case None =>
        val el = f2.last.nodeType.asCollectionType.elementType
        val tc = f2.head.nodeType.asCollectionType.cons
        CollectionType(tc, el)
    })
    if(same && newType == nodeType) this else {
      // Compute result type
      copy(
        from = (from, f2).zipped.map { case ((s, _), n) => (s, n) },
        where = w2.getOrElse(where),
        groupBy = g2.map(_.headOption).getOrElse(groupBy),
        orderBy = o2.map(o2 => (orderBy, o2).zipped.map { case ((_, o), n) => (n, o) }).getOrElse(orderBy),
        select = s2.map(_.headOption).getOrElse(select)
      ).nodeTyped(newType)
    }
  }
}

/** The row_number window function */
final case class RowNumber(by: Seq[(Node, Ordering)] = Seq.empty) extends TypedNode {
  def tpe = StaticType.Long
  lazy val nodeChildren = by.map(_._1)
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]) =
    copy(by = by.zip(ch).map{ case ((_, o), n) => (n, o) })
  override def nodeChildNames = by.zipWithIndex.map("by" + _._2)
  override def toString = "RowNumber"
}

/** A client-side projection of type
  * (CollectionType(c, t), u) => CollectionType(c, u). */
final case class ResultSetMapping(generator: Symbol, from: Node, map: Node) extends BinaryNode with DefNode {
  def left = from
  def right = map
  override def nodeChildNames = Seq("from "+generator, "map")
  protected[this] def nodeRebuild(left: Node, right: Node) = copy(from = left, map = right)
  def nodeGenerators = Seq((generator, from))
  override def toString = "ResultSetMapping"
  protected[this] def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(generator = gen(0))
  def nodeWithComputedType(scope: SymbolScope): Node = {
    val f2 = from.nodeWithComputedType(scope)
    val fromType = f2.nodeType.asCollectionType
    val s2 = map.nodeWithComputedType(scope + (generator -> fromType.elementType))
    val newType = CollectionType(fromType.cons, s2.nodeType)
    if((f2 eq from) && (s2 eq map) && newType == nodeType) this
    else copy(from = f2, map = s2).nodeTyped(newType)
  }
}
