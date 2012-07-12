package scala.slick.compiler

import scala.slick.ast._

/** Rewrite OrderBy to SortBy */
class RewriteOrderBy extends Phase {
  val name = "rewriteOrderBy"

  def apply(tree: Node, state: CompilationState): Node = tree match {
    case BoundOrderBy(gens, bgen, OrderBy(ogen, _, by), Pure(sel)) =>
      val map = gens.iterator.map(_._1).zipWithIndex.toMap
      def substRef(n: Node): Node = n match {
        case r @ Ref(g) => map.get(g) match {
          case Some(idx) => Select(Ref(ogen), ElementSymbol(idx + 2))
          case None => r
        }
        case n => n.nodeMapChildren(substRef)
      }
      val innerSel = Pure(ProductNode((sel +: gens.map(g => Ref(g._1)))))
      val innerBind = gens.foldRight[Node](innerSel) { case ((gen, from), z) => Bind(gen, from, z) }
      val sort = SortBy(ogen, innerBind, by.map { case (n, o) => (substRef(n), o) })
      val outerBind = Bind(bgen, sort, Pure(Select(Ref(bgen), ElementSymbol(1))))
      outerBind
    case n => n.nodeMapChildren(ch => apply(ch, state))
  }

  /** Extractor for OrderBy nested in one or more Binds */
  object BoundOrderBy {
    def unapply(n: Node): Option[(List[(Symbol, Node)], Symbol, OrderBy, Node)] = n match {
      case Bind(gen, o: OrderBy, sel) => Some(Nil, gen, o, sel)
      case Bind(gen, from, BoundOrderBy(l, s, o, n)) => Some(((gen, from) :: l, s, o, n))
      case _ => None
    }
  }
}
