package scala.slick.compiler

import scala.slick.ast._
import Util._

/** Inline references to IntrinsicSymbols. */
class Inline extends Phase {
  val name = "inline"

  def apply(state: CompilerState) = state.map { tree =>
    tree.replace { case Ref(i: IntrinsicSymbol) => i.target }
  }
}
