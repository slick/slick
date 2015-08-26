package slick.compiler

import scala.collection.mutable.HashSet
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
    val seen = new HashSet[AnonSymbol]
    def tr(n: Node, replace: Map[AnonSymbol, AnonSymbol]): Node = {
      val n2 = n match { // Give TableNode and Pure nodes a unique TypeSymbol
        case t: TableNode => t.copy(identity = new AnonTableIdentitySymbol)
        case Pure(value, _) => Pure(value)
        case n => n
      }
      val n3 = n2 match {
        case r @ Ref(a: AnonSymbol) => replace.get(a) match {
          case Some(s) => if(s eq a) r else Ref(s)
          case None => r
        }
        case d: DefNode =>
          var defs = replace
          d.mapScopedChildren { (symO, ch) =>
            val r = tr(ch, defs)
            symO match {
              case Some(a: AnonSymbol) =>
                if(seen.contains(a)) defs += a -> new AnonSymbol
                else seen += a
              case _ =>
            }
            r
          }.mapSymbols {
            case a: AnonSymbol => defs.getOrElse(a, a)
            case s => s
          }
        case n: Select => n.mapChildren(tr(_, replace)) :@ n.nodeType
        case n => n.mapChildren(tr(_, replace))
      }
      // Remove all NominalTypes (which might have changed)
      if(n3.hasType && hasNominalType(n3.nodeType)) n3.untyped else n3
    }
    tr(tree, Map())
  }

  def hasNominalType(t: Type): Boolean = t match {
    case _: NominalType => true
    case _: AtomicType => false
    case _ => t.children.exists(hasNominalType)
  }
}
