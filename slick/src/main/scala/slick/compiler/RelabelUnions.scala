package slick.compiler

import slick.ast._
import Util._

/** Assign the AnonSymbols of fields from the left side of a Union to the
  * right side. This ensures that both sides are protected when we prune
  * unused references pointing to left-side Symbols. */
class RelabelUnions extends Phase {
  val name = "relabelUnions"

  def apply(state: CompilerState) = state.map(_.replace({
    case u @ Union(Bind(_, _, Pure(StructNode(ls), lts)), rb @ Bind(_, _, Pure(StructNode(rs), _)), _) =>
      val rs2 = (ls, rs).zipped.map { case ((s, _), (_, n)) => (s, n) }
      u.copy(right = rb.copy(select = Pure(StructNode(rs2), lts))).infer()
  }, keepType = true, bottomUp = true))
}
