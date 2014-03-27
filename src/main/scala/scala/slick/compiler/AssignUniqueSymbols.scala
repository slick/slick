package scala.slick.compiler

import scala.collection.mutable.HashSet
import scala.slick.SlickException
import scala.slick.ast._
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
    val seenType = new HashSet[TypeSymbol]
    def tr(n: Node, replace: Map[AnonSymbol, AnonSymbol]): Node = {
      val n2 = n match { // Give TableNode and Pure nodes a unique TypeSymbol
        case t: TableNode =>
          t.copy(identity = new AnonTableIdentitySymbol)
        case p @ Pure(value, ts) =>
          if(seenType contains ts) Pure(value)
          else {
            seenType += ts
            p
          }
        case n => n
      }
      val n3 = // Remove all NominalTypes (which might have changed)
        if(n2.nodeHasType && !(n2.nodeType.collect { case _: NominalType => () }).isEmpty)
          n2.nodeUntypedOrCopy
        else n2
      n3 match {
        case r @ Ref(a: AnonSymbol) => replace.get(a) match {
          case Some(s) => if(s eq a) r else Ref(s)
          case None => r
        }
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
    }
    tr(tree, Map())
  }
}
