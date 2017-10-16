package slick.compiler

import slick.ast.TypeUtil._
import slick.ast.Util._
import slick.ast._
import slick.util.{Ellipsis, ConstArray}

/** Rewrite "distinct on" to "distinct" or "group by" */
class RewriteDistinct extends Phase {
  val name = "rewriteDistinct"

  def apply(state: CompilerState) = if(state.get(Phase.assignUniqueSymbols).map(_.distinct).getOrElse(true)) state.map(_.replace({

    case n @ Bind(s1, dist1: Distinct, Pure(sel1, ts1))  =>
      logger.debug("Rewriting Distinct in Bind:", Ellipsis(n, List(0, 0)))
      val (inner, sel2) = rewrite(s1, dist1, sel1)
      Bind(s1, inner, Pure(sel2, ts1)).infer()

    case n @ Aggregate(s1, dist1: Distinct, sel1)  =>
      logger.debug("Rewriting Distinct in Aggregate:", Ellipsis(n, List(0, 0)))
      val (inner, sel2) = rewrite(s1, dist1, sel1)
      Aggregate(s1, inner, sel2).infer()

  }, keepType = true, bottomUp = true)) else {
    logger.debug("No DISTINCT used as determined by assignUniqueSymbols - skipping phase")
    state
  }

  def rewrite(s1: TermSymbol, dist1: Distinct, sel1: Node): (Node, Node) = {
    val refFields = sel1.collect[TermSymbol] {
      case Select(Ref(s), f) if s == s1 => f
    }.toSet
    logger.debug("Referenced fields: " + refFields.mkString(", "))
    val onFlat = ProductNode(ConstArray(dist1.on)).flatten
    val onNodes = onFlat.children.toSet
    val onFieldPos = onNodes.iterator.zipWithIndex.collect[(TermSymbol, Int)] {
      case (Select(Ref(s), f), idx) if s == dist1.generator => (f, idx)
    }.toMap
    logger.debug("Fields used directly in 'on' clause: " + onFieldPos.keySet.mkString(", "))
    if((refFields -- onFieldPos.keys).isEmpty) {
      // Only distinct fields referenced -> Create subquery and remove 'on' clause
      val onDefs = ConstArray.from(onNodes).map((new AnonSymbol, _))
      val onLookup = onDefs.iterator.collect[(TermSymbol, AnonSymbol)] {
        case (a, Select(Ref(s), f)) if s == dist1.generator => (f, a)
      }.toMap
      val inner = Bind(dist1.generator, Distinct(new AnonSymbol, dist1.from, ProductNode(ConstArray.empty)), Pure(StructNode(onDefs)))
      val sel2 = sel1.replace {
        case Select(Ref(s), f) if s == s1 => Select(Ref(s), onLookup(f))
      }
      val ret = Subquery(inner, Subquery.AboveDistinct)
      logger.debug("Removed 'on' clause from Distinct:", Ellipsis(ret, List(0, 0, 0)))
      (ret, sel2)
    } else {
      val sel2 = sel1.replace {
        case Select(Ref(s), f) :@ tpe if s == s1 =>
          onFieldPos.get(f) match {
            case Some(idx) =>
              Select(Select(Ref(s), ElementSymbol(1)), ElementSymbol(idx+1))
            case None =>
              val as = new AnonSymbol
              Aggregate(as, Select(Ref(s), ElementSymbol(2)),
                Library.Min.typed(tpe, Select(Ref(as), f)))
          }
      }
      val ret = GroupBy(dist1.generator, dist1.from, onFlat)
      logger.debug("Transformed Distinct to GroupBy:", Ellipsis(ret, List(0)))
      (ret, sel2)
    }
  }
}
