package org.scalaquery.ast

import OptimizerUtil._
import collection.mutable.ArrayBuffer
import org.scalaquery.util.RefId

/**
 * Expand columns and merge comprehensions in queries
 */
object Columnizer {

  def run(tree: Node): Node = {
    val t2 = rewriteSelects(tree)
    //val t3 = toComprehensions.andThen(mergeComprehensions).apply(t2)
    //val cs = findColumns(withCs)
    //println("*** cs: "+cs)
    //findDefs(withCs)
    //introduceRefs(withCs)
    t2
  }

  val toComprehensions = new Transformer {
    def replace = {
      case Bind(gen, from, select) => Comprehension(Seq((gen, from)), Nil, Some(select))
      case Filter(gen, from, where) => Comprehension(Seq((gen, from)), Seq(where), None)
    }
  }

  val mergeComprehensions = new Transformer {
    def replace = {
      case c1 @ Comprehension(from1, where1, Some(c2 @ Comprehension(from2, where2, select))) =>
        c2.copy(from = from1 ++ from2, where = where1 ++ where2)
    }
  }

  def unwrap(wrappers: Set[Symbol], n: Node): Node = n.replace {
    case InRef(sym, what) if wrappers contains sym => what
  }

  def replaceSelect(in: Node, select: Node, wrappers: List[Symbol]): Node = in match {
    case f: FilteredQuery => f.nodeMapFrom(n => replaceSelect(n, select, f.generator :: wrappers))
    case b @ Bind(_, _, Pure(_)) => b.copy(select = unwrap(wrappers.toSet, select))
    case b @ Bind(gen, _, nonPure) => b.copy(select = replaceSelect(nonPure, select, gen :: wrappers))
    //case FilteredJoin(_, _, )
  }

  def newGenerators(n: Node) = {
    val gens = n.collectNodeGenerators.map(_._1).toSet
    memoized[Symbol, Symbol](_ => { case s => if(gens contains s) new AnonSymbol else s })
  }

  def findUnfilteredGenerator(n: Node): Node = n match {
    case FilteredQuery(_, from) => findUnfilteredGenerator(from)
    case n => n
  }

  def rewriteSelects(tree: Node): Node = memoized[Node, Node](r => {
    case b @ Bind(gen, from, select) if !(from.isInstanceOf[BaseJoin] || from.isInstanceOf[FilteredJoin]) =>
      val selRefs = select.collectInRefTargets(gen)
      if(selRefs.isEmpty || !(findUnfilteredGenerator(from).isInstanceOf[Bind]) ) b.nodeMapChildren(r)
      else { //TODO what if selRefs.isEmpty && !filterRefs.isEmpty?
        val (filterRefsSyms, filterRefs) = findFilterRefs(from)
        val selRefsToUnwrapped = selRefs.toSeq.map(r => (r, unwrap(filterRefsSyms, r))).toMap
        val filterRefsToUnwrapped = filterRefs.toSeq.map(r => (r, unwrap(filterRefsSyms, r))).toMap
        val allUnwrappedRefsToSyms = (selRefsToUnwrapped.values ++ filterRefsToUnwrapped.values).toSet.iterator.map((u: Node) => (u, new AnonSymbol)).toMap
        val struct = StructNode(allUnwrappedRefsToSyms.iterator.map{ case (u,s) => (s,u) }.toIndexedSeq)
        val fromRep = replaceSelect(from, Pure(struct), Nil)
        val newGens = newGenerators(fromRep)
        val newFilterRefsSyms = filterRefsSyms.map(newGens)
        val rFrom = r(fromRep.replaceSymbols(newGens))
        val rSel = r(select)
        val fromReplMap = filterRefsToUnwrapped.map{ case (w,u) => (u.replaceSymbols(newGens), FieldRef(gen, allUnwrappedRefsToSyms(u)).replaceSymbols(newGens)) }
        //println("*** fromReplMap: "+fromReplMap)
        //fromReplMap.foreach(t => t._1.dump(t._2+" <- "))
        val selReplMap = selRefsToUnwrapped.mapValues(u => FieldRef(gen, allUnwrappedRefsToSyms(u)).replaceSymbols(newGens))
        //println("*** selReplMap: "+selReplMap)
        //selReplMap.foreach(t => t._1.dump(t._2+" <- "))
        val b2 = b.copy(
          from = replaceReferences(gen, newFilterRefsSyms, fromReplMap, rFrom),
          select = replaceReferences(gen, newFilterRefsSyms, selReplMap, rSel))
        b2
      }
    case n => n.nodeMapChildren(r)
  })(tree)

  def findFilterRefs(n: Node): (Set[Symbol], Seq[Node]) = {
    def findSyms(n: Node): Seq[Symbol] = n match {
      case FilteredQuery(gen, from) => gen +: findSyms(from)
      case _ => IndexedSeq.empty
    }
    val syms = findSyms(n)
    val refs = syms.flatMap(n.collectInRefTargets)
    (syms.toSet, refs)
  }

  def replaceReferences(Sym: Symbol, filterGens: Set[Symbol], m: Map[Node, Node], n: Node): Node = {
    object TransitiveRef {
      def unapply(n: Node): Option[(Symbol, Node)] = {
        n match {
          case InRef(sym, what) if filterGens contains sym =>
            unapply(what) match {
              case Some((_, what)) => Some((sym, what))
              case None => Some((sym, what))
            }
          case _ => None
        }
      }
    }
    n.replace {
      case t @ TransitiveRef(sym, value) =>
        //value.dump("*** matched in "+sym+": "+m.get(value)+": ")
        m.get(value).map{ case FieldRef(gen, n) => FieldRef(sym, n) }.getOrElse(t)
      case i @ InRef(Sym, value) => m.get(value).getOrElse(i)
    }
  }

  def findDefs(n: Node) = n.collectAll[(Symbol, (Node, Node))] {
    case n: DefNode => n.nodeGenerators.map { case (sym, what) => (sym, (what, n)) }
  }.toMap
}

case class Comprehension(from: Seq[(Symbol, Node)], where: Seq[Node], select: Option[Node]) extends Node with DefNode {
  protected[this] def nodeChildGenerators = from.map(_._2) ++ where ++ select
  override protected[this] def nodeChildNames = from.map("from " + _._1) ++ where.zipWithIndex.map("where" + _._2) ++ select.zipWithIndex.map("select" + _._2)
  def nodeMapChildren(f: Node => Node) = mapChildren(f, f)
  def mapChildren(fromMap: Node => Node, otherMap: Node => Node): Node = {
    val fromO = nodeMapNodes(from.view.map(_._2), fromMap)
    val whereO = nodeMapNodes(where, otherMap)
    val selectO = select.map(otherMap)
    if(fromO.isDefined || whereO.isDefined || selectO != select)
      copy(from = fromO.map(f => from.view.map(_._1).zip(f)).getOrElse(from), where = whereO.getOrElse(where), select = selectO)
    else this
  }
  def nodeGenerators = from
  override def toString = "Comprehension"
  def nodeMapGenerators(f: Symbol => Symbol) = {
    val gens = from.map(_._1)
    mapOrNone(gens, f) match {
      case Some(s) => copy(from = from.zip(s).map { case ((_, n), s) => (s, n) })
      case None => this
    }
  }
}
