package slick.compiler

import slick.ast._
import Util._

/** Expand paths of record types to reference all fields individually and
  * recreate the record structure at the call site. */
class ExpandRecords extends Phase {
  val name = "expandRecords"

  def apply(state: CompilerState) = state.map(_.replace({ case n: PathElement => expandPath(n) }).infer())

  def expandPath(n: Node): Node = n.nodeType.structural match {
    case StructType(ch) =>
      StructNode(ch.map { case (s, t) => (s, expandPath(n.select(s) :@ t)) }(collection.breakOut))
    case p: ProductType =>
      ProductNode(p.numberedElements.map { case (s, t) => expandPath(n.select(s) :@ t) }.toVector)
    case t => n.asInstanceOf[PathElement].untypedPath
  }
}
