package org.scalaquery.ast

import OptimizerUtil._
import org.scalaquery.ql.{Query, AbstractTable}
import org.scalaquery.util.Logging

/**
 * Expand columns in queries
 */
object Columnizer extends (Node => Node) with Logging {

  val expandColumns = new Transformer.Defs {
    def replace = pftransitive {
      // Remove unnecessary wrapping of generated TableRef reference
      case ResolvedInRef(sym, Pure(TableRef(sym1)), InRef(sym2, what)) if sym1 == sym2 => InRef(sym, what)
      // Rewrite a table reference that has already been rewritten to a Ref
      case ResolvedRef(sym, f @ FilterChain(syms, t: AbstractTable[_])) => InRef(sym, Node(t.*))
      // Push InRef down into ProductNode
      case InRef(sym, ProductNode(ns @ _*)) => ProductNode(ns.map(n => InRef(sym, n)): _*)
      // Merge products
      case NestedProductNode(ch @ _*) => ProductNode(ch: _*)
      // Rewrite a table reference returned in a Bind
      case b @ Bind(_, _, t: AbstractTable[_]) => b.copy(select = Bind(new AnonSymbol, t, Pure(Node(t.*))))
      //case Pure(ResolvedRef(sym1, f @ FilterChain(syms, t: AbstractTable[_]))) => Pure(TableRef(sym1))
    }
  }

  def apply(tree: Node): Node = fixpoint(tree)((expandAndOptimize _).andThen(expandPureSelects))

  def expandPureSelects(n: Node): Node = {
    val n2 = memoized[Node, Node](r => {
      case b @ Bind(gen, _, Pure(x)) if (x match {
          case TableRef(_) => false
          case ProductNode(_*) => false
          case StructNode(_) => false
          case _ => true
        }) => b.copy(select = Pure(ProductNode(x)))
      case n => n.nodeMapChildren(r)
    })(n)
    val defs = n2.collectAll[(Symbol, Node)]{ case d: DefNode => d.nodeGenerators }.toMap
    logger.debug("Generated defs from single-column selects: "+defs)
    def findField(n: Node): Node = n match {
      case Bind(_, _, Pure(ProductNode(x))) => x
      case FilteredQuery(_, from) => findField(from)
    }
    memoized[Node, Node](r => {
      case Ref(sym) if defs.contains(sym) => InRef(sym, findField(defs(sym)))
      case n => n.nodeMapChildren(r)
    })(n2)
  }

  def expandAndOptimize(tree: Node): Node = {
    def isFilterOrUnionOfTable(n: Node): Boolean = n match {
      case FilteredQuery(_, from) => isFilterOrUnionOfTable(from)
      case _: AbstractTable[_] => true
      case Union(left, right, _, _, _) => isFilterOrUnionOfTable(left) || isFilterOrUnionOfTable(right)
      case _ => false
    }
    def mapSourceTable(n: Node, f: AbstractTable[_] => Node): Node = n match {
      case q @ FilteredQuery(_, _) =>
        q.nodeMapFrom(mapSourceTable(_, f))
      case u @ Union(left, right, _, _, _) =>
        u.copy(left = mapSourceTable(left, f), right = mapSourceTable(right, f))
      case source: AbstractTable[_] => f(source)
      case n => n
    }
    val t2 = if(isFilterOrUnionOfTable(tree)) {
      mapSourceTable(tree, source => Node(Query(source).map(_.*)))
    } else tree
    val t3 = expandColumns(t2)
    //TODO This hack unwraps the expanded references within their scope
    // There may be other situations where unwrapping finds the wrong symbols,
    // so this should be done in a completely different way (by introducing
    // StructNodes much earlier and avoiding wrapping altogether)
    def optimizeUnions(n: Node): Node = n match {
      case u @ Union(left, right, _, _, _) =>
        val l = Optimizer.standard(optimizeUnions(left))
        val r = Optimizer.standard(optimizeUnions(right))
        u.copy(left = l, right = r)
      case n => n.nodeMapChildren(optimizeUnions)
    }
    val t4 = optimizeUnions(t3)
    Optimizer.standard(t4)
  }
}
