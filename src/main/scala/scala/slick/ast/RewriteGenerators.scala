package scala.slick.ast

import OptimizerUtil._
import scala.slick.ql.RawNamedColumn
import scala.slick.SLICKException
import scala.slick.util.Logging

/**
 * Rewrite all generators to return exactly the required fields.
 */
object RewriteGenerators extends Logging {

  class SyntheticBindSymbol extends AnonSymbol

  def apply(tree: Node): Node = {
    val t2 = rewriteGenerators(tree)
    // Replace InRef(RawNamedColumn) occurrences left over after rewriting them in all generators.
    // This can happen in queries without projections.
    memoized[Node, Node](r => {
      case InRef(t, r: RawNamedColumn) => FieldRef(t, r.symbol)
      case InRef(t, n) =>
        findPureTarget(n) match {
          case StructNode(IndexedSeq((sym, _))) => FieldRef(t, sym)
          case ProductNode(FieldRef(_, sym)) => FieldRef(t, sym)
        }
      case n => n.nodeMapChildren(r)
    })(t2)
  }

  def findPureTarget(in: Node): Node = in match {
    case f: FilteredQuery => findPureTarget(f.from)
    case Bind(_, _, Pure(n)) => n
    case Bind(_, _, nonPure) => findPureTarget(nonPure)
    case n => n
  }

  def rewriteGenerators(tree: Node): Node = memoized[Node, Node](r => {
    //case fs @ RealFilterChain(filterSyms, Bind(_, _, _)) =>
    //TODO push additional columns needed by the filters into the Bind (without rewriting the existing ones)
    case b @ Bind(gen, from, what) if(!(gen.isInstanceOf[SyntheticBindSymbol])) =>
      rewriteGenerator(b, gen, from).nodeMapChildren(r)
    case j @ FilteredJoin(leftGen, rightGen, left, right, _, _) =>
      rewriteGenerator(rewriteGenerator(j, rightGen, right).nodeMapChildren(r), leftGen, left).nodeMapChildren(r)
    case j @ BaseJoin(leftGen, rightGen, left, right, _) =>
      rewriteGenerator(rewriteGenerator(j, rightGen, right).nodeMapChildren(r), leftGen, left).nodeMapChildren(r)
    case n => n.nodeMapChildren(r)
  })(tree)

  def rewriteGenerator(b: Node, gen: Symbol, from: Node): Node = {
    logger.debug("rewriteGenerator: ",b)
    val rewrite = collectRewriteSymbols(gen, from)
    logger.debug("rewrite: "+rewrite)
    val refsMap = collectReferences(b, rewrite).iterator.map(n => (n, new AnonSymbol)).toMap
    val struct = refsMap.iterator.map{ case (n,s) => (s,n) }.toIndexedSeq

    val (withNewSelect, replacementMap) = {
      val isTableBased = findSelect(from) match {
        case _: TableNode => true
        case Pure(TableRef(_)) => true
        case x =>
          logger.debug("not table-based:", x)
          false
      }
      logger.debug("isTableBased: "+isTableBased+", keys: "+refsMap.keys)
      if(isTableBased && refsMap.keys.forall(_.isInstanceOf[RawNamedColumn])) {
        // only column refs -> rewrite directly
        logger.debug("Rewriting directly as table")
        (b, refsMap.map { case (r: RawNamedColumn, _) => (r: Node, r.symbol) })
      } else {
        logger.debug("struct:", StructNode(struct))
        (b.nodeMapChildren(n => if(n eq from) replaceSelect(from, struct, rewrite, new ReplaceSelectContext(b)) else n), refsMap)
      }
    }
    logger.debug("withNewSelect:", withNewSelect)
    logger.debug("replacementMap: "+replacementMap)
    val rr = replaceReferences(withNewSelect, rewrite, replacementMap, Set.empty)
    logger.debug("rewritten:", rr)
    rr
  }

  def collectRewriteSymbols(s: Symbol, n: Node): Set[Symbol] = n match {
    case FilteredQuery(gen, from) => collectRewriteSymbols(gen, from) + s
    case _ => Set(s)
  }

  def collectReferences(n: Node, syms: Set[Symbol]): Set[Node] = n match {
    case InRef(sym, what) if syms contains sym => Set(what)
    case n => n.nodeChildren.map(ch => collectReferences(ch, syms)).flatten.toSet
  }

  def replaceReferences(n: Node, syms: Set[Symbol], repl: Map[Node, Symbol], parents: Set[Symbol]): Node = n match {
    case ir @ InRef(sym, what) if syms contains sym =>
      //logger.debug("matched InRef("+sym+", "+what+") with matching sym")
      repl.get(what) match {
        case Some(sym2) =>
          if(parents contains sym) {
            //TODO aliasing to another column in the same struct -> we should prevent this earlier
            //logger.debug("NOT replacing with FieldRef("+sym+", "+sym2+") at parents "+parents)
            replaceReferences(what, syms, repl, parents)
          } else {
            //logger.debug("replacing with FieldRef("+sym+", "+sym2+") at parents "+parents)
            FieldRef(sym, sym2)
          }
        case None =>
          replaceReferences(what, syms, repl, parents)
      }
    case d: DefNode =>
      d.nodeMapScopedChildren{ case (symO, ch) => replaceReferences(ch, syms, repl, parents ++ symO.toSet) }
    case n => n.nodeMapChildren(ch => replaceReferences(ch, syms, repl, parents))
  }

  def findSelect(in: Node): Node = in match {
    case f: FilteredQuery => findSelect(f.from)
    case b @ Bind(_, _, Pure(_)) => b.from
    case b @ Bind(_, _, nonPure) => findSelect(nonPure)
    case n => n
  }

  class ReplaceSelectContext(baseNode: Node, val keepExisting: Boolean = false, var indices: Option[Seq[Int]] = None) {
    def forUnion = new ReplaceSelectContext(baseNode, true)

    lazy val filterSyms = {
      logger.debug("computing filter syms for:", baseNode)
      baseNode.collect[Symbol]{ case FilteredQuery(gen, _) => gen }.toSet
    }

    def unwrap(n: Node): Node = n match {
      case InRef(sym1, InRef(sym2, what)) if filterSyms contains sym2 => InRef(sym1, unwrap(what))
      case n => n.nodeMapChildren(unwrap)
    }
  }

  def replaceSelect(in: Node, struct: IndexedSeq[(Symbol, Node)], genChain: Set[Symbol], ctx: ReplaceSelectContext): Node = {
    logger.debug("replaceSelect("+in+", "+struct+", "+genChain+", "+ctx+")")
    in match {
      case f: FilteredQuery => f.nodeMapFrom(n => replaceSelect(n, struct, genChain, ctx))
      case b @ Bind(_, _, p @ Pure(_)) => b.copy(select = replaceSelect(p, struct, genChain, ctx))
      case b @ Bind(gen, _, nonPure) => b.copy(select = replaceSelect(nonPure, struct, genChain, ctx))
      case t: TableNode => //TODO support keepExisting and useIndices
        val gen = new SyntheticBindSymbol
        val rewrapped = StructNode(struct.map { case (s,n) => (s, rewrap(n, genChain.iterator.map(s => (s, gen)).toMap, gen)) })
        logger.debug("actual replacement:", rewrapped)
        Bind(gen, t, Pure(rewrapped))
      case f @ FilteredJoin(leftGen, rightGen, _, _, jt, on) => //TODO support keepExisting and useIndices
        val gen = new SyntheticBindSymbol
        logger.debug("struct:", StructNode(struct))
        val rewrapMap = genChain.iterator.map(s => (s, gen)).toMap + (leftGen -> leftGen) + (rightGen -> rightGen)
        val rewrapped = StructNode(struct.map { case (s,n) => (s, rewrap(n, rewrapMap, gen)) })
        logger.debug("genChain: "+genChain)
        logger.debug("replacement for FilteredJoin:", rewrapped)
        Bind(gen, f, Pure(rewrapped))
      case Pure(what) =>
        if(ctx.keepExisting) {
          val cols = what.flattenProduct
          if(ctx.indices == None) {
            val idxMap = cols.zipWithIndex.map{ case (n, idx) => (ctx.unwrap(n), idx) }.toMap
            logger.debug("idxMap: "+idxMap)
            logger.debug("keeping existing in:", ProductNode(cols.map(ctx.unwrap): _*))
            logger.debug("columns to find:", StructNode(struct.map{ case (s, n) => (s, ctx.unwrap(n)) }))
            val structIdxs = struct.map { case (_, n) => idxMap.get(ctx.unwrap(n)) match {
              case Some(idx) => idx
              case None => throw new SLICKException("Unknown expression "+n+" in UNION result - cannot rewrite")
            }}
            logger.debug("found indexes: "+structIdxs)
            ctx.indices = Some(structIdxs)
          }
          val mappedStruct = struct.zipWithIndex.map { case ((sym, _), idx) => (sym, cols(ctx.indices.get(idx))) }
          //TODO support keepExisting and useIndices
          //sys.error("not implemented")
          Pure(StructNode(mappedStruct))
        } else Pure(StructNode(struct))
      case u @ Union(left, right, _, leftGen, rightGen) =>
        logger.debug("replacing Union of "+leftGen+", "+rightGen)
        logger.debug("genChain: "+genChain)
        val unionCtx = ctx.forUnion
        val lr = replaceSelect(left, struct.map{ case (s,n) => (s, n.unwrap(Set(leftGen))) }, genChain, unionCtx)
        val rr = replaceSelect(right, struct, genChain, unionCtx)
        u.copy(left = lr, right = rr)
    }
  }

  def rewrap(n: Node, wrappers: Map[Symbol, Symbol], newWrapper: Symbol): Node = n match {
    case c @ RawNamedColumn(_) => FieldRef(newWrapper, c.symbol)
    case InRef(sym, what) =>
      if(wrappers.keySet contains sym) rewrap(what, wrappers, wrappers(sym))
      else rewrap(what, wrappers, newWrapper)
    case n => n.nodeMapChildren(ch => rewrap(ch, wrappers, newWrapper))
  }
}
