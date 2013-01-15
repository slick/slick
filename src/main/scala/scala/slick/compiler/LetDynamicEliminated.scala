package scala.slick.compiler

import scala.slick.SlickException
import scala.slick.ast._

/** Ensure that LetDynamic has been eliminated */
class LetDynamicEliminated extends Phase {
  val name = "letDynamicEliminated"

  def apply(state: CompilerState) =
    if(state.tree.isInstanceOf[LetDynamic])
      throw new SlickException("Unexpected LetDynamic after Inliner")
    else state
}
