package scala.slick.compiler

import scala.slick.ast._
import Util.nodeToNodeOps

/** Compute type information for all nodes in the AST */
class AssignTypes extends Phase {
  val name = "assignTypes"

  def apply(tree: Node, state: CompilationState): Node =
    tree.nodeWithComputedType(new DefaultSymbolScope(Map.empty))
}
