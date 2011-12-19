package org.scalaquery.ast

import collection.mutable.{ArrayBuffer, HashMap}
import org.scalaquery.util.RefId

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

  final def scan(n: Node) {
    val r = RefId(n)
    val c = counts.getOrElse(r, 0)
    counts(r) = c + 1
    if(c == 0) n.nodeChildren.foreach(scan _)
  }

  def apply(tree: Node): Node = {
    composed.foreach { _.counts = self.counts }
    val memo = new HashMap[Node, Node]
    val repl = replace.orElse(Optimizer.pfidentity[Node])
    def tr(n: Node): Node = memo.getOrElseUpdate(n, repl(n).nodeMapChildren(tr))
    def scanAndTr(n: Node): Node = {
      counts.clear()
      memo.clear()
      scan(n)
      val n2 = tr(n)
      if(n2 eq n) n else scanAndTr(n2)
    }
    scanAndTr(tree)
  }

  def compose(g: Transformer): Transformer = new Transformer {
    def replace = self.replace.orElse(g.replace)
    composed = Vector(self, g)
  }

  def andThen(g: Transformer): Transformer = g.compose(this)
}
