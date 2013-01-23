package scala.slick.compiler

import scala.collection.mutable.HashSet
import scala.slick.SlickException
import scala.slick.ast._

/** Ensure that all symbol definitions in a tree are unique */
class AssignUniqueSymbols extends Phase {
  val name = "assignUniqueSymbols"

  def apply(state: CompilerState) = state.map { tree =>
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
