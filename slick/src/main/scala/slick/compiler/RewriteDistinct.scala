package slick.compiler

import slick.ast.Library.AggregateFunctionSymbol
import slick.ast.TypeUtil._
import slick.ast.Util._
import slick.ast._
import slick.util.{Ellipsis, ConstArray}

/** Rewrite "distinct on" to "distinct" or "group by" */
class RewriteDistinct extends Phase {
  val name = "rewriteDistinct"

  def apply(state: CompilerState) = if(state.get(Phase.assignUniqueSymbols).map(_.distinct).getOrElse(true)) state.map(_.replace({

    case n @ Bind(s1, Distinct(s2, from1, on1), Pure(sel1, ts1))  =>
      logger.debug("Rewriting Distinct:", Ellipsis(n, List(0, 0)))
      val refFields = sel1.collect[TermSymbol] {
        case Select(Ref(s), f) if s == s1 => f
      }.toSet
      logger.debug("Referenced fields: " + refFields.mkString(", "))
      val onFlat = ProductNode(ConstArray(on1)).flatten
      val onNodes = onFlat.children.toSet
      val onFieldPos = onNodes.iterator.zipWithIndex.collect[(TermSymbol, Int)] {
        case (Select(Ref(s), f), idx) if s == s2 => (f, idx)
      }.toMap
      logger.debug("Fields used directly in 'on' clause: " + onFieldPos.keySet.mkString(", "))
      if((refFields -- onFieldPos.keys).isEmpty) {
        // Only distinct fields referenced -> Create subquery and remove 'on' clause
        val onDefs = ConstArray.from(onNodes).map((new AnonSymbol, _))
        val onLookup = onDefs.iterator.collect[(TermSymbol, AnonSymbol)] {
          case (a, Select(Ref(s), f)) if s == s2 => (f, a)
        }.toMap
        val inner = Bind(s2, Distinct(new AnonSymbol, from1, ProductNode(ConstArray.empty)), Pure(StructNode(onDefs)))
        val sel2 = sel1.replace {
          case Select(Ref(s), f) if s == s1 => Select(Ref(s), onLookup(f))
        }
        val ret = Bind(s1, Subquery(inner, Subquery.AboveDistinct), Pure(sel2, ts1)).infer()
        logger.debug("Removed 'on' clause from Distinct:", Ellipsis(ret, List(0, 0, 0, 0)))
        ret
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
        val ret = Bind(s1, GroupBy(s2, from1, onFlat), Pure(sel2, ts1)).infer()
        logger.debug("Transformed Distinct to GroupBy:", Ellipsis(ret, List(0, 0)))
        ret
      }

  }, keepType = true, bottomUp = true)) else state
}
