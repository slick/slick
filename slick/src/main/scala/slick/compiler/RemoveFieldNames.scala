package slick.compiler

import slick.ast._
import Util._
import TypeUtil._
import slick.util.ConstArray

/** Convert unreferenced StructNodes to single columns or ProductNodes (which is needed for
  * aggregation functions and at the top level). */
class RemoveFieldNames(val alwaysKeepSubqueryNames: Boolean = false) extends Phase {
  val name = "removeFieldNames"

  def apply(state: CompilerState) = state.map { n => ClientSideOp.mapResultSetMapping(n, true) { rsm =>
    val CollectionType(_, NominalType(top, StructType(fdefs))) = rsm.from.nodeType
    val requiredSyms = rsm.map.collect[TermSymbol]({
      case Select(Ref(s), f) if s == rsm.generator => f
    }, stopOnMatch = true).toSeq.distinct.zipWithIndex.toMap
    logger.debug("Required symbols: " + requiredSyms.mkString(", "))
    val rsm2 = rsm.nodeMapServerSide(false, { n =>
      val refTSyms = n.collect[TypeSymbol] {
        case Select(_ :@ NominalType(s, _), _) => s
        case Union(_, _ :@ CollectionType(_, NominalType(s, _)), _) => s
        case Comprehension(_, _ :@ CollectionType(_, NominalType(s, _)), _, _, _, _, _, _, _, _, _) if alwaysKeepSubqueryNames => s
      }.toSet
      val allTSyms = n.collect[TypeSymbol] { case p: Pure => p.identity }.toSet
      val unrefTSyms = allTSyms -- refTSyms
      n.replaceInvalidate {
        case Pure(StructNode(ConstArray.empty), pts)  =>
          // Always convert an empty StructNode because there is nothing to reference
          (Pure(ProductNode(ConstArray.empty), pts), pts)
        case Pure(StructNode(ch), pts) if unrefTSyms contains pts =>
          val sel =
            if(ch.length == 1 && pts != top) ch(0)._2
            else if(pts != top) ProductNode(ch.map(_._2))
            else ProductNode(ConstArray.from(ch.map { case (s, n) => (requiredSyms.getOrElse(s, Int.MaxValue), n) }.toSeq.sortBy(_._1)).map(_._2))
          (Pure(sel, pts), pts)
        case Pure(StructNode(ch), pts) if pts == top =>
          val sel =
            StructNode(ConstArray.from(ch.map { case (s, n) => (requiredSyms.getOrElse(s, Int.MaxValue), (s, n)) }.toSeq.sortBy(_._1)).map(_._2))
          (Pure(sel, pts), pts)
      }.infer()
    })
    logger.debug("Transformed RSM: ", rsm2)
    val CollectionType(_, fType) = rsm2.from.nodeType
    val baseRef = Ref(rsm.generator) :@ fType
    rsm2.copy(map = rsm2.map.replace({
      case Select(Ref(s), f) if s == rsm.generator =>
        Select(baseRef, ElementSymbol(requiredSyms(f) + 1)).infer()
    }, keepType = true)) :@ rsm.nodeType
  }}
}
