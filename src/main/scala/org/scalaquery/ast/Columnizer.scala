package org.scalaquery.ast

import org.scalaquery.ql.AbstractTable
import OptimizerUtil._

/**
 * Expand columns in queries
 */
object Columnizer extends (Node => Node) {

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
      case Pure(ResolvedRef(sym1, f @ FilterChain(syms, t: AbstractTable[_]))) => Pure(TableRef(sym1))
    }
  }

  def apply(tree: Node): Node = {
    val n = expandAndOptimize(tree)
    if(n == tree) n else apply(n)
  }

  def expandAndOptimize(tree: Node): Node = {
    val t2 = expandColumns(tree)
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
    val t3 = optimizeUnions(t2)
    Optimizer.standard(t3)
  }
}
