package slick.compiler

import slick.ast._
import Util._
import TypeUtil._

/** Infer types and compute missing structural views for all nominal table types. */
class InferTypes extends Phase {
  val name = "inferTypes"

  def apply(state: CompilerState) = state.map { tree =>
    val tree2 = tree.nodeWithComputedType(new DefaultSymbolScope(Map.empty), true, false)
    val structs = tree2.collect[(TypeSymbol, (Symbol, Type))] {
      case s @ Select(_ :@ (n: NominalType), sym) => n.sourceNominalType.sym -> (sym -> s.nodeType)
    }.groupBy(_._1).mapValues(v => StructType(v.map(_._2).toMap.toIndexedSeq))
    logger.debug("Found Selects for NominalTypes: "+structs.keySet.mkString(", "))
    def tr(n: Node): Node = n.nodeMapChildren(tr, keepType = true).nodeTypedOrCopy(n.nodeType.replace {
      case UnassignedStructuralType(tsym) if structs.contains(tsym) => structs(tsym)
    })
    tr(tree2)
  }
}
