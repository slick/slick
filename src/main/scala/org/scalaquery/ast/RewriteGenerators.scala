package org.scalaquery.ast

import OptimizerUtil._
import collection.mutable.HashMap
import org.scalaquery.ql.AbstractTable

/**
 * Rewrite all generators to return exactly the required fields.
 */
object RewriteGenerators {

  def apply(tree: Node): Node = memoized[Node, Node](r => {
    case b @ Bind(gen, from, select) if !(from.isInstanceOf[BaseJoin] || from.isInstanceOf[FilteredJoin]) =>
      val selRefs = select.collectInRefTargets(gen)
      if(selRefs.isEmpty || !(findFilterSource(from).isInstanceOf[Bind]) ) b.nodeMapChildren(r)
      else { //TODO what if selRefs.isEmpty && !filterRefs.isEmpty?
        val (filterRefsSyms, filterRefs) = findFilterRefs(from)
        val selRefsToUnwrapped = selRefs.toSeq.map(r => (r, unwrap(filterRefsSyms, r))).toMap
        val filterRefsToUnwrapped = filterRefs.toSeq.map(r => (r, unwrap(filterRefsSyms, r))).toMap
        val allUnwrappedRefsToSyms = (selRefsToUnwrapped.values ++ filterRefsToUnwrapped.values).toSet.iterator.map((u: Node) => (u, new AnonSymbol)).toMap
        val struct = StructNode(allUnwrappedRefsToSyms.iterator.map{ case (u,s) => (s,u) }.toIndexedSeq)
        val fromRep = replaceSelect(from, Pure(struct), Nil)
        val newGens = fromRep.generatorsReplacer
        val newFilterRefsSyms = filterRefsSyms.map(newGens)
        val rFrom = r(fromRep.replaceSymbols(newGens))
        val rSel = r(select)
        val fromReplMap = filterRefsToUnwrapped.map{ case (w,u) => (u.replaceSymbols(newGens), Path(gen, allUnwrappedRefsToSyms(u)).replaceSymbols(newGens)) }
        //println("*** fromReplMap: "+fromReplMap)
        //fromReplMap.foreach(t => t._1.dump(t._2+" <- "))
        val selReplMap = selRefsToUnwrapped.mapValues(u => Path(gen, allUnwrappedRefsToSyms(u)).replaceSymbols(newGens))
        //println("*** selReplMap: "+selReplMap)
        //selReplMap.foreach(t => t._1.dump(t._2+" <- "))
        val b2 = b.copy(
          from = replaceReferences(gen, newFilterRefsSyms, fromReplMap, rFrom),
          select = replaceReferences(gen, newFilterRefsSyms, selReplMap, rSel))
        b2
      }
    case n => n.nodeMapChildren(r)
  })(tree)

  def unwrap(wrappers: Set[Symbol], n: Node): Node = n.replace {
    case InRef(sym, what) if wrappers contains sym => what
  }

  def replaceSelect(in: Node, select: Node, wrappers: List[Symbol]): Node = in match {
    case f: FilteredQuery => f.nodeMapFrom(n => replaceSelect(n, select, f.generator :: wrappers))
    case b @ Bind(_, _, Pure(_)) => b.copy(select = unwrap(wrappers.toSet, select))
    case b @ Bind(gen, _, nonPure) => b.copy(select = replaceSelect(nonPure, select, gen :: wrappers))
    //case FilteredJoin(_, _, )
  }

  def findFilterSource(n: Node): Node = n match {
    case FilteredQuery(_, from) => findFilterSource(from)
    case n => n
  }

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
        m.get(value).map{ case Path(gen, n) => Path(sym, n) }.getOrElse(t)
      case i @ InRef(Sym, value) => m.get(value).getOrElse(i)
    }
  }
}
