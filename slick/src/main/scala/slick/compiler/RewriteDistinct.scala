package slick.compiler

import slick.ast.TypeUtil._
import slick.ast.Util._
import slick.ast._
import slick.util.{Ellipsis, ConstArray}

/** Rewrite "distinct on" to "distinct" or "group by" */
class RewriteDistinct extends Phase {
  val name = "rewriteDistinct"

  def apply(state: CompilerState) = {
    logger.debug("RewriteDistinct phase starting")
    if(state.get(Phase.assignUniqueSymbols).map(_.distinct).getOrElse(true)) {
      logger.debug("DISTINCT detected, proceeding with rewrite")
      state.map(_.replace({

        case n @ Bind(s1, dist1: Distinct, Pure(sel1, ts1))  =>
          logger.debug("Rewriting Distinct in Bind:", Ellipsis(n, List(0, 0)))
          val (inner, sel2) = rewrite(s1, dist1, sel1)
          Bind(s1, inner, Pure(sel2, ts1)).infer()

        case n @ Aggregate(s1, dist1: Distinct, sel1)  =>
          logger.debug("Rewriting Distinct in Aggregate:", Ellipsis(n, List(0, 0)))
          val (inner, sel2) = rewrite(s1, dist1, sel1)
          Aggregate(s1, inner, sel2).infer()

      }, keepType = true, bottomUp = true))
    } else {
      logger.debug("No DISTINCT used as determined by assignUniqueSymbols - skipping phase")
      state
    }
  }

  def rewrite(s1: TermSymbol, dist1: Distinct, sel1: Node): (Node, Node) = {
    val refFields = sel1.collect[TermSymbol] {
      case Select(Ref(s), f) if s == s1 => f
    }.toSet
    logger.debug("Referenced fields: " + refFields.mkString(", "))
    val onFlat = ProductNode(ConstArray(dist1.on)).flatten
    val onNodes = onFlat.children
    val onFieldPos = onNodes.iterator.zipWithIndex.collect[(TermSymbol, Int)] {
      case (Select(Ref(s), f), idx) if s == dist1.generator => (f, idx)
    }.toMap
    logger.debug("Fields used directly in 'on' clause: " + onFieldPos.keySet.mkString(", "))
    
    // Check if there's a SortBy in dist1.from that would conflict with DISTINCT ON
    val hasConflictingSortBy = checkForConflictingSortBy(dist1.from, dist1.generator, onFieldPos.keySet)
    logger.debug(s"Has conflicting SortBy: $hasConflictingSortBy")
    
    if((refFields -- onFieldPos.keys).isEmpty && !hasConflictingSortBy) {
      // Only distinct fields referenced AND no conflicting ORDER BY -> Create subquery and remove 'on' clause
      logger.debug("Creating subquery because only distinct fields referenced and no conflicts")
      val onDefs = onNodes.map((new AnonSymbol, _))
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
      // Either non-distinct fields referenced OR conflicting ORDER BY -> Use GroupBy approach
      logger.debug("Using GroupBy approach due to: " + (if (hasConflictingSortBy) "conflicting ORDER BY" else "non-distinct fields referenced"))
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

  def checkForConflictingSortBy(node: Node, distinctGenerator: TermSymbol, distinctFields: Set[TermSymbol]): Boolean = {
    logger.debug(s"Checking node for conflicting sortBy: ${node.getClass.getSimpleName}")
    
    // Collect all SortBy nodes in the tree
    val sortByNodes = node.collect {
      case sortBy: SortBy => sortBy
    }
    
    logger.debug(s"Found ${sortByNodes.length} SortBy nodes")
    
    for (sortBy <- sortByNodes) {
      logger.debug(s"Analyzing SortBy with generator: ${sortBy.generator}")
      
      // Extract all field references from ORDER BY expressions
      val orderByFields = sortBy.by.flatMap { case (orderExpr, _) =>
        logger.debug(s"Processing orderBy expression: $orderExpr")
        orderExpr.collect[TermSymbol] {
          case Select(Ref(s), f) => 
            logger.debug(s"Found ORDER BY field: $f from ref $s")
            f
        }
      }.toSet
      
      logger.debug(s"ORDER BY fields: $orderByFields, DISTINCT fields: $distinctFields")
      val extraOrderByFields = orderByFields -- distinctFields
      
      if (extraOrderByFields.nonEmpty) {
        logger.debug(s"Found conflicting ORDER BY fields: $extraOrderByFields")
        return true
      }
    }
    
    logger.debug("No conflicting ORDER BY fields found")
    false
  }
}
