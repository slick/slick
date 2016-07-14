package slick.compiler

import slick.ast._
import Util._
import slick.SlickException

/** Expand paths of record types to reference all fields individually and
  * recreate the record structure at the call site. */
class ExpandRecords extends Phase {
  val name = "expandRecords"

  private var tree: Node = null

  def apply(state: CompilerState) = state.map{n => tree = n; n.replace({ case n: PathElement => expandPath(n) }).infer()}

  def expandPath(n: Node): Node = n.nodeType.structural match {
    case StructType(ch) =>
      StructNode(ch.map { case (s, t) => (s, expandPath(n.select(s) :@ t)) })
    case p: ProductType =>
      ProductNode(p.elements.zipWithIndex.map { case (t, i) => expandPath(n.select(new ElementSymbol(i+1)) :@ t) })
    case t => try {
      n.asInstanceOf[PathElement].untypedPath
    } catch {
      case ex: ClassCastException => logger.debug("Tree", tree, mark = _ eq n); throw ex
    }
  }
}
