package scala.slick.compiler

import scala.slick.SlickException
import scala.slick.ast._

/** Ensure that LetDynamic has been eliminated */
class LetDynamicEliminated extends Phase {
  val name = "letDynamicEliminated"
  def apply(tree: Node, state: CompilationState): Node = {
    if(tree.isInstanceOf[LetDynamic])
      throw new SlickException("Unexpected LetDynamic after Inliner")
    else tree
  }
}
