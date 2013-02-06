package scala.slick.compiler

import scala.slick.ast._
import Util._

/**
 * Since Nodes cannot be encoded into Scala Tuples, they have to be encoded
 * into the Tuples' children instead, so we end up with trees like
 * ProductNode(Select(r, ElementSymbol(1)), Select(r, ElementSymbol(2)))
 * where r is a Ref to an expression that yields a Query of
 * ProductNode(_, _). This optimizer rewrites those forms to the raw Ref r.
 */
class ReconstructProducts extends Phase {
  val name = "reconstructProducts"

  /** ADT for representing shapes of ProductNodes */
  private sealed abstract class PShape
  private case object PLeaf extends PShape
  private case class PProduct(children: Seq[PShape]) extends PShape

  /** Find the common reference in a candidate node */
  private def findCommonRef(n: Node): Option[AnonSymbol] = n match {
    case Ref(sym: AnonSymbol) => Some(sym)
    case Select(in, _: ElementSymbol) => findCommonRef(in)
    case ProductNode(ch) =>
      val chref = ch.map(findCommonRef)
      if(chref.contains(None)) None
      else {
        val refs = chref.map(_.get).toSet
        if(refs.size == 1) Some(refs.head)
        else None
      }
    case _ => None
  }

  /** Extractor for Select(_, ElementSymbol) chains */
  private object ProductElements {
    def unapply(n: Node): Option[(List[Int], Symbol)] = n match {
      case Ref(sym) => Some((Nil, sym))
      case Select(in, ElementSymbol(idx)) => unapply(in) match {
        case None => None
        case Some((l, sym)) => Some((idx :: l, sym))
      }
    }
  }

  /** Find the common prefix in a ProductNode of nested paths */
  def commonPrefix(n: Node, base: Symbol): Option[List[Int]] = n match {
    case ProductNode(ch) if !ch.isEmpty =>
      val chpr = ch.map(n => commonPrefix(n, base)).iterator.collect { case Some(x) => x }
      if(chpr.isEmpty) None
      else Some(chpr.reduceLeft((a, b) => a.zip(b).takeWhile{case (a,b) => a == b}.map(_._1)))
    case ProductElements(idxs, sym) if sym == base => Some(idxs.reverse)
    case _ => None
  }

  /** Find the shape and common ref of a product if it exists */
  private def shapeFor(n: Node, expect: List[Int] = Nil): Option[PShape] = n match {
    case ProductNode(ch) =>
      logger.debug("    shapeFor: ProductNode("+ch+"), expecting "+expect)
      val chsh = ch.zipWithIndex.map { case (c, i) => shapeFor(c, (i+1) :: expect) }
      if(chsh.contains(None)) None else Some(PProduct(chsh.map(_.get)))
    case ProductElements(idxs, _) =>
      logger.debug("    shapeFor: ProductElements("+idxs+"), expecting "+expect)
      if(idxs == expect) Some(PLeaf) else None
  }

  /** Check if the shape matches the ref target */
  private def shapeMatches(s: PShape, t: Node): Boolean = (s, t) match {
    case (PLeaf, _) => true
    case (PProduct(pch), ProductNode(nch)) if pch.length == nch.length =>
      (pch, nch).zipped.map(shapeMatches).forall(identity)
    case (PProduct(pch), Join(_, _, left, right, _, _)) if pch.length == 2 =>
      (pch, Seq(left, right)).zipped.map(shapeMatches).forall(identity)
    case (PProduct(pch), GroupBy(_, _, from, by)) if pch.length == 2 =>
      (pch, Seq(from, by)).zipped.map(shapeMatches).forall(identity)
    case _ => false
  }

  /** Replace matching products */
  private def transformer = new Transformer.Defs {
    // Narrow the target of a ref to the actual Pure value produced
    def narrow(n: Node): Option[Pure] = n match {
      case n: Pure => Some(n)
      case n: Join =>
        Some(Pure(ProductNode(Seq(n, n)))) // dummy value with correct shape
      case n: GroupBy =>
        Some(Pure(ProductNode(Seq(n, n)))) // dummy value with correct shape
      case Union(left, _, _, _, _) => narrow(left)
      case FilteredQuery(_, from) => narrow(from)
      case Bind(_, _, select) => narrow(select)
      case Ref(Def(n)) => narrow(n)
      case p: ProductNode =>
        Some(Pure(p)) // probably a deconstructed tuple, so try it
      case _ => None
    }
    def replace = {
      case p: ProductNode =>
        (for {
          sym <- findCommonRef(p)
          _ = logger.debug("Found ProductNode with common ref "+sym)
          target <- defs.get(sym)
          _ = logger.debug("  Ref target: "+target)
          ntarget <- narrow(target)
          _ = logger.debug("  Narrowed target: "+ntarget)
          prefix = commonPrefix(p, sym).getOrElse(Nil)
          _ = logger.debug("  Common prefix: "+prefix)
          shape <- shapeFor(p, prefix)
          _ = logger.debug("  Shape: "+shape)
        } yield if(shapeMatches(shape, ntarget.value)) {
            val repl = (sym :: prefix.map(ElementSymbol.apply)).reverse
            logger.debug("  Shape matches -> replacing with "+Path.toString(repl))
            Path(repl)
          } else p
        ).getOrElse(p)
    }
  }

  def apply(state: CompilerState) = state.map(transformer.repeat)
}
