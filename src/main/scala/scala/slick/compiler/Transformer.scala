package scala.slick.compiler

import scala.slick.ast._
import scala.collection.mutable.HashMap
import Util._

/**
 * A tree transformer which replaces nodes transitively while updating
 * some information about the tree.
 */
abstract class Transformer { self =>

  val keepType = false
  def replace: PartialFunction[Node, Node]

  def initTree(n: Node) = ()

  private[this] def run(tree: Node, once: Boolean): Node = {
    val repl = replace.orElse[Node, Node] { case n => n }
    def scanAndTr(n: Node): Node = {
      initTree(n)
      val n2 = memoized[Node, Node](r => { n =>
        val rn = repl(n)
        rn.nodeMapChildren(r, keepType)
      })(n)
      if(once || (n2 eq n)) n2 else scanAndTr(n2)
    }
    scanAndTr(tree)
  }

  def once(tree: Node) = run(tree, true)

  def repeat(tree: Node) = run(tree, false)
}

object Transformer {
  trait Defs extends Transformer {
    val defs = new HashMap[Symbol, Node]
    abstract override def initTree(tree: Node) {
      super.initTree(tree)
      defs.clear()
      defs ++= tree.collectAll[(Symbol, Node)] { case d: DefNode => d.nodeGenerators }
    }
    object Def {
      def unapply(sym: Symbol): Option[Node] = defs.get(sym)
    }
  }
}
