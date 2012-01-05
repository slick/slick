package org.scalaquery.ast

import OptimizerUtil._

/**
 * A tree transformer which replaces nodes transitively while updating
 * some information about the tree.
 */
abstract class Transformer extends (Node => Node) { self =>

  def replace: PartialFunction[Node, Node]

  def initTree(n: Node) = ()

  def apply(tree: Node): Node = {
    val repl = replace.orElse(pfidentity[Node])
    def scanAndTr(n: Node): Node = {
      initTree(n)
      val n2 = memoized[Node, Node](r => { n => repl(n).nodeMapChildren(r) })(n)
      if(n2 eq n) n else scanAndTr(n2)
    }
    scanAndTr(tree)
  }

  def compose(g: Transformer): Transformer = new Transformer {
    def replace = self.replace.orElse(g.replace)
    override def initTree(n: Node) {
      self.initTree(n)
      g.initTree(n)
    }
  }

  def andThen(g: Transformer): Transformer = g.compose(this)
}
