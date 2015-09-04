package slick.compiler

import scala.collection.mutable.{HashSet, HashMap}
import slick.SlickException
import slick.ast._
import TypeUtil._

/** Ensure that all symbol definitions in a tree are unique. The same symbol
  * can initially occur in multiple sub-trees when some part of a query is
  * reused multiple times. This phase assigns new, uniqe symbols, so that
  * later phases do not have to take scopes into account for identifying
  * the source of a symbol. The rewriting is performed for both, term symbols
  * and type symbols. */
class AssignUniqueSymbols extends Phase {
  val name = "assignUniqueSymbols"

  def apply(state: CompilerState) = state.map { tree =>
    val replace = new HashMap[TermSymbol, AnonSymbol]
    def tr(n: Node): Node = {
      val n3 = n match {
        case Select(in, s) => Select(tr(in), s) :@ n.nodeType
        case r @ Ref(a: AnonSymbol) =>
          val s = replace.getOrElse(a, a)
          if(s eq a) r else Ref(s)
        case t: TableNode => t.copy(identity = new AnonTableIdentitySymbol)(t.driverTable)
        case Pure(value, _) => Pure(tr(value))
        case g: GroupBy =>
          val d = g.copy(identity = new AnonTypeSymbol)
          val a = new AnonSymbol
          replace += g.fromGen -> a
          g.copy(fromGen = a, tr(g.from), tr(g.by), identity = new AnonTypeSymbol)
        case n: StructNode => n.mapChildren(tr)
        case d: DefNode =>
          replace ++= d.generators.iterator.map(_._1 -> new AnonSymbol)
          d.mapSymbols(s => replace.getOrElse(s, s)).mapChildren(tr)
        case n => n.mapChildren(tr)
      }
      // Remove all NominalTypes (which might have changed)
      if(n3.hasType && hasNominalType(n3.nodeType)) n3.untyped else n3
    }
    tr(tree)
  }

  def hasNominalType(t: Type): Boolean = t match {
    case _: NominalType => true
    case _: AtomicType => false
    case _ => t.children.exists(hasNominalType)
  }
}
