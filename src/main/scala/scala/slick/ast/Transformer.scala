package scala.slick.ast

import OptimizerUtil._
import collection.mutable.HashMap
import scala.slick.util.RefId

/**
 * A tree transformer which replaces nodes transitively while updating
 * some information about the tree.
 */
abstract class Transformer extends (Node => Node) { self =>

  def replace: PartialFunction[Node, Node]

  def initTree(n: Node) = ()

  private def applyInternal(tree: Node, once: Boolean): Node = {
    val repl = replace.orElse(pfidentity[Node])
    def scanAndTr(n: Node): Node = {
      initTree(n)
      val n2 = memoized[Node, Node](r => { n => repl(n).nodeMapChildren(r) })(n)
      if(once || (n2 eq n)) n2 else scanAndTr(n2)
    }
    scanAndTr(tree)
  }

  def apply(tree: Node): Node = applyInternal(tree, false)

  def applyOnce(tree: Node): Node = applyInternal(tree, true)

  def compose(g: Transformer): Transformer = new Transformer {
    def replace = self.replace.orElse(g.replace)
    override def initTree(n: Node) {
      self.initTree(n)
      g.initTree(n)
    }
  }

  def andThen(g: Transformer): Transformer = g.compose(this)
}

object Transformer {
  trait Defs extends Transformer {
    val defs = new HashMap[Symbol, Node]
    abstract override def initTree(tree: Node) {
      super.initTree(tree)
      defs.clear()
      defs ++= tree.collectAll[(Symbol, Node)] { case d: DefNode => d.nodeGenerators }
    }
    //@deprecated("Use Def instead", "")
    object ResolvedRef {
      def unapply(n: Node): Option[(Symbol, Node)] = n match {
        case Ref(sym) => defs.get(sym).map(v => (sym, v))
        case _ => None
      }
    }
    //@deprecated("Use Def instead", "")
    object ResolvedInRef {
      def unapply(n: Node): Option[(Symbol, Node, Node)] = n match {
        case InRef(sym, what) => defs.get(sym).map(v => (sym, v, what))
        case _ => None
      }
    }
    object Def {
      def unapply(sym: Symbol): Option[Node] = defs.get(sym)
    }
  }

  trait DefRefsBidirectional extends Transformer {
    val defs = new HashMap[Symbol, RefId[Node]]
    val reverse = new HashMap[RefId[Node], Symbol]
    override def initTree(tree: Node) {
      super.initTree(tree)
      defs.clear()
      defs ++= tree.collectAll[(Symbol, RefId[Node])] {
        case d: DefNode => d.nodeGenerators.map { case (s,n) => (s, RefId(n)) }
      }
      reverse.clear()
      reverse ++= defs.map { case (k,v) => (v,k) }
    }
  }
}

abstract class RecursiveTransformer extends (Node => Node) {
  private[this] var _chain: List[Node] = Nil
  private[this] var _scope: Map[Symbol, (Node, Node)] = Map.empty
  def chain: List[Node] = _chain
  def scope: Map[Symbol, (Node, Node)]
  def apply(tree: Node): Node = {
    val repl = replace.orElse(pfidentity[Node])
    def tr(n: Node): Node = {
      val n2 = repl(n)
      var defsHere: Seq[(Symbol, Node)] = n2 match {
        case d: DefNode => d.nodeGenerators
        case _ => Seq.empty
      }
      def updateDefs(from: Node, to: Node) {
        defsHere = defsHere.map { case (s, n) => (s, if(n eq from) to else n) }
      }
      val defChildren = defsHere.map(t => RefId(t._2)).toSet
      val n3 = n2.nodeMapChildren { ch: Node =>
        if(defChildren.isEmpty || defChildren.contains(RefId(ch))) {
          val prevChain = _chain
          _chain = n2 :: _chain
          val ch2 = tr(ch)
          updateDefs(ch, ch2)
          _chain = prevChain
          ch2
        } else {
          val prevChain = _chain
          _chain = n2 :: _chain
          val prevScope = _scope
          _scope = _scope ++ defsHere.map { case (s, what) => (s, (what, n2)) }
          val ch2 = tr(ch)
          updateDefs(ch, ch2)
          _chain = prevChain
          _scope = prevScope
          ch2
        }
      }
      if(n3 eq n) n else tr(n3)
    }
    tr(tree)
  }
  def replace: PartialFunction[Node, Node]
}
