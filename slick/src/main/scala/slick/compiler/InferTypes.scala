package slick.compiler

import slick.ast._
import Util._
import TypeUtil._

/** Infer all missing types. */
class InferTypes extends Phase {
  val name = "inferTypes"

  def apply(state: CompilerState) =
    state.map(_.nodeWithComputedType(new DefaultSymbolScope(Map.empty), typeChildren = true))
}
