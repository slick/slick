package scala.slick.ast
package opt

import Util._
import scala.collection.mutable.{HashSet, HashMap}
import scala.slick.SlickException

/** Replace IntrinsicSymbols by AnonSymbols and collect them in a LetDynamic */
class LocalizeRefs extends Phase {
  val name = "localizeRefs"
  def apply(tree: Node, state: CompilationState): Node = {
    val map = new HashMap[AnonSymbol, Node]
    val newNodes = new HashMap[AnonSymbol, Node]
    val tr = new Transformer {
      def replace = {
        case r: RefNode => r.nodeMapReferences {
          case IntrinsicSymbol(target) =>
            val s = new AnonSymbol
            map += s -> target
            newNodes += s -> target
            s
          case s => s
        }
      }
    }
    val tree2 = tr.once(tree)
    while(!newNodes.isEmpty) {
      val m = newNodes.toMap
      newNodes.clear()
      m.foreach { case (sym, n) => map += sym -> tr.once(n) }
    }
    if(map.isEmpty) tree2 else LetDynamic(map.toSeq, tree2)
  }
}

/** Ensure that all symbol definitions in a tree are unique */
class AssignUniqueSymbols extends Phase {
  val name = "assignUniqueSymbols"

  def apply(tree: Node, state: CompilationState): Node = {
    val seen = new HashSet[AnonSymbol]
    def tr(n: Node, replace: Map[AnonSymbol, AnonSymbol]): Node = n match {
      case r @ Ref(a: AnonSymbol) => replace.get(a) match {
        case Some(s) => if(s eq a) r else Ref(s)
        case None => r
      }
      case l: LetDynamic =>
        throw new SlickException("Dynamic scopes should be eliminated before assigning unique symbols")
      case d: DefNode =>
        var defs = replace
        d.nodeMapScopedChildren { (symO, ch) =>
          val r = tr(ch, defs)
          symO match {
            case Some(a: AnonSymbol) =>
              if(seen.contains(a)) defs += a -> new AnonSymbol
              else seen += a
            case _ =>
          }
          r
        }.nodeMapGenerators {
          case a: AnonSymbol => defs.getOrElse(a, a)
          case s => s
        }
      case n => n.nodeMapChildren(tr(_, replace))
    }
    tr(tree, Map())
  }
}

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

/** Ensure that LetDynamic has been eliminated */
class LetDynamicEliminated extends Phase {
  val name = "letDynamicEliminated"
  def apply(tree: Node, state: CompilationState): Node = {
    if(tree.isInstanceOf[LetDynamic])
      throw new SlickException("Unexpected LetDynamic after Inliner")
    else tree
  }
}
