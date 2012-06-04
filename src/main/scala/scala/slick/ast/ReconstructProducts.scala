package scala.slick.ast

import OptimizerUtil._
import scala.slick.util.Logging

/**
 * Since Nodes cannot be encoded into Scala Tuples, they have to be encoded
 * into the Tuples' children instead, so we end up with trees like
 * ProductNode(Select(r, ElementSymbol(1)), Select(r, ElementSymbol(2)))
 * where r is a Ref to an expression that yields a Query of
 * ProductNode(_, _). This optimizer rewrites those forms to the raw Ref r.
 */
object ReconstructProducts extends (Node => Node) with Logging {

  /** ADT for representing shapes of ProductNodes */
  private sealed abstract class PShape
  private case object PLeaf extends PShape
  private case class PProduct(children: Seq[PShape]) extends PShape

  /** Find the common reference in a candidate node */
  private def findCommonRef(n: Node): Option[AnonSymbol] = n match {
    case Ref(sym: AnonSymbol) => Some(sym)
    case Select(in, _: ElementSymbol) => findCommonRef(in)
    case ProductNode(ch @ _*) =>
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

  /** Find the shape and common ref of a product if it exists */
  private def shapeFor(n: Node, expect: List[Int] = Nil): Option[PShape] = n match {
    case ProductNode(ch @ _*) =>
      val chsh = ch.zipWithIndex.map { case (c, i) => shapeFor(c, (i+1) :: expect) }
      if(chsh.contains(None)) None else Some(PProduct(chsh.map(_.get)))
    case ProductElements(idxs, _) =>
      if(idxs == expect) Some(PLeaf) else None
  }

  /** Check if the shape matches the ref target */
  private def shapeMatches(s: PShape, t: Node): Boolean = (s, t) match {
    case (PLeaf, _) => true
    case (PProduct(pch), ProductNode(nch @ _*)) if pch.length == nch.length =>
      (pch, nch).zipped.map(shapeMatches).forall(identity)
    case (PProduct(pch), j: JoinNode) if pch.length == 2 =>
      (pch, Seq(j.left, j.right)).zipped.map(shapeMatches).forall(identity)
    case _ => false
  }

  /** Replace matching products */
  private object transformer extends Transformer.Defs {
    // Narrow the target of a ref to the actual Pure value produced
    def narrow(n: Node): Option[Pure] = n match {
      case n: Pure => Some(n)
      case n: JoinNode =>
        Some(Pure(ProductNode(n, n))) // dummy value with correct shape
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
          shape <- shapeFor(p)
          _ = logger.debug("  Shape: "+shape)
        } yield if(shapeMatches(shape, ntarget.value)) {
            logger.debug("  Shape matches")
            Ref(sym)
          } else p
          ).getOrElse(p)
    }
  }

  def apply(tree: Node) = transformer.repeat(tree)
}
