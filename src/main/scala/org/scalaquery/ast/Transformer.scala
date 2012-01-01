package org.scalaquery.ast

import collection.mutable.HashMap
import org.scalaquery.util.RefId
import OptimizerUtil._

/**
 * A tree transformer which replaces nodes transitively while updating
 * some information about the tree.
 */
abstract class Transformer extends (Node => Node) { self =>
  private var counts = new HashMap[RefId[Node], Int]
  private var composed: Vector[Transformer] = Vector.empty

  /** Get the number of exact identities of a Node in a collect() call */
  def count(n: Node) = counts.getOrElse(RefId(n), 0)

  /** Check if a node exists exactly once in the current tree */
  def unique(n: Node) = count(n) == 1

  def replace: PartialFunction[Node, Node]

  def initTree(n: Node) = ()

  final def scan(n: Node) {
    val r = RefId(n)
    val c = counts.getOrElse(r, 0)
    counts(r) = c + 1
    if(c == 0) n.nodeChildren.foreach(scan _)
  }

  def apply(tree: Node): Node = {
    composed.foreach { _.counts = self.counts }
    val repl = replace.orElse(pfidentity[Node])
    def scanAndTr(n: Node): Node = {
      counts.clear()
      scan(n)
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
    composed = Vector(self, g)
  }

  def andThen(g: Transformer): Transformer = g.compose(this)
}
