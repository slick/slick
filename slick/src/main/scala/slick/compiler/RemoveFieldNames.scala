package slick.compiler

import slick.ast._
import Util._
import TypeUtil._

/** Convert unreferenced StructNodes to single columns or ProductNodes (which is needed for
  * aggregation functions and at the top level). */
class RemoveFieldNames extends Phase {
  val name = "removeFieldNames"

  def apply(state: CompilerState) = state.map { n => ClientSideOp.mapResultSetMapping(n, true) { rsm =>
    val CollectionType(_, NominalType(top, StructType(fdefs))) = rsm.from.nodeType
    val indexes = fdefs.zipWithIndex.map { case ((s, _), i) => (s, ElementSymbol(i+1)) }.toMap
    val rsm2 = rsm.nodeMapServerSide(false, { n =>
      val refTSyms = n.collect[TypeSymbol] { case Select(_ :@ NominalType(s, _), _) => s }.toSet
      val allTSyms = n.collect[TypeSymbol] { case p: Pure => p.identity }.toSet
      val unrefTSyms = allTSyms -- refTSyms
      n.replaceInvalidate {
        case Pure(StructNode(ch), pts) if unrefTSyms contains pts =>
          (Pure(if(ch.length == 1 && pts != top) ch(0)._2 else ProductNode(ch.map(_._2)), pts), pts)
      }.infer()
    })
    logger.debug("Transformed RSM: ", rsm2)
    val CollectionType(_, fType) = rsm2.from.nodeType
    val baseRef = Ref(rsm.generator) :@ fType
    rsm2.copy(map = rsm2.map.replace({
      case Select(Ref(s), f) if s == rsm.generator =>
        Select(baseRef, indexes(f)).infer()
    }, keepType = true)) :@ rsm.nodeType
  }}
}
