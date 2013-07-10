package scala.slick.compiler

import scala.slick.ast._
import Util.nodeToNodeOps

/** Compute type information for all nodes in the AST */
class AssignTypes extends Phase {
  val name = "assignTypes"

  def apply(state: CompilerState) = state.map(_.nodeWithComputedType(
    new DefaultSymbolScope(Map.empty), typeChildren = true, retype = true))
}
