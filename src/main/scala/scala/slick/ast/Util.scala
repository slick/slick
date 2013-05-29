package scala.slick.ast

import scala.language.implicitConversions
import scala.collection.mutable.{HashMap, HashSet, ArrayBuffer}

/**
 * Utility methods for AST manipulation.
 */
object Util {

  abstract class Memoizer[A, B] extends (A => B) {
    def state: scala.collection.Map[A, B]
  }

  def memoized[A, B](f: (A => B) => A => B): Memoizer[A, B] = new Memoizer[A, B] {
    private[this] lazy val g = f(this)
    def apply(a: A) = state.getOrElseUpdate(a, g(a))
    val state = new HashMap[A, B]
  }

  def mapOrNone[A <: AnyRef](c: Traversable[A])(f: A => A): Option[IndexedSeq[A]] = {
    val b = new ArrayBuffer[A]
    var changed = false
    c.foreach { x =>
      val n = f(x)
      b += n
      if(n ne x) changed = true
    }
    if(changed) Some(b.result()) else None
  }

  @inline implicit def nodeToNodeOps(n: Node): NodeOps = new NodeOps(n)
}

/** A scope for scoped traversal */
case class Scope(m: Map[Symbol, (Node, Scope)]) {
  def get(s: Symbol) = m.get(s)
  def + (s: Symbol, n: Node) = Scope(m + (s -> (n, this)))
}

object Scope {
  val empty = Scope(Map())
}

/** Extra methods for Nodes. */
final class NodeOps(val tree: Node) extends AnyVal {
  import Util._

  @inline def collect[T](pf: PartialFunction[Node, T]): Iterable[T] = NodeOps.collect(tree, pf)

  def collectAll[T](pf: PartialFunction[Node, Seq[T]]): Iterable[T] = collect[Seq[T]](pf).flatten

  def replace(f: PartialFunction[Node, Node], keepType: Boolean = false): Node = NodeOps.replace(tree, f, keepType)

  def foreach[U](f: (Node => U)) {
    def g(n: Node) {
      f(n)
      n.nodeChildren.foreach(g)
    }
    g(tree)
  }

  def flattenProduct = {
    def f(n: Node): IndexedSeq[Node] = n match {
      case ProductNode(ns) => ns.flatMap(f).toIndexedSeq
      case n => IndexedSeq(n)
    }
    f(tree)
  }

  def mapChildrenWithScope(f: (Option[Symbol], Node, Scope) => Node, scope: Scope): Node = tree match {
    case d: DefNode =>
      var local = scope
      d.nodeMapScopedChildren { (symO, ch) =>
        val r = f(symO, ch, local)
        symO.foreach(sym => local = local + (sym, r))
        r
      }
    case n => n.nodeMapChildren(ch => f(None, ch, scope))
  }

  def findNode(p: Node => Boolean): Option[Node] = {
    if(p(tree)) Some(tree)
    else {
      val it = tree.nodeChildren.iterator.map(_.findNode(p)).dropWhile(_.isEmpty)
      if(it.hasNext) it.next() else None
    }
  }
}

object NodeOps {
  import Util._

  // These methods should be in the class by 2.10.0-RC1 took away the ability
  // to use closures in value classes

  def collect[T](tree: Node, pf: PartialFunction[Node, T]): Iterable[T] = {
    val b = new ArrayBuffer[T]
    tree.foreach(pf.andThen[Unit]{ case t => b += t }.orElse[Node, Unit]{ case _ => () })
    b
  }

  def replace(tree: Node, f: PartialFunction[Node, Node], keepType: Boolean): Node =
    f.applyOrElse(tree, ({ case n: Node => n.nodeMapChildren(_.replace(f), keepType) }): PartialFunction[Node, Node])
}

/** Some less general but still useful methods for the code generators. */
object ExtraUtil {

  def findPaths(startingAt: Set[Symbol], n: Node) = {
    val b = new HashSet[Node]
    def f(n: Node): Unit = n match {
      case p @ Path(syms) if startingAt contains syms.last => b += p
      case n => n.nodeChildren.foreach(f)
    }
    f(n)
    b.toSet
  }

  def hasRowNumber(n: Node): Boolean = n match {
    case c: Comprehension => false
    case r: RowNumber => true
    case n => n.nodeChildren.exists(hasRowNumber)
  }

  def replaceRowNumber(n: Node)(f: RowNumber => Node): Node = n match {
    case c: Comprehension => c
    case r: RowNumber => f(r)
    case n => n.nodeMapChildren(ch => replaceRowNumber(ch)(f))
  }

  def hasRefToOneOf(n: Node, s: scala.collection.Set[Symbol]): Boolean = n match {
    case r: RefNode => s.contains(r.nodeReference) || n.nodeChildren.exists(ch => hasRefToOneOf(ch, s))
    case n => n.nodeChildren.exists(ch => hasRefToOneOf(ch, s))
  }

  def linearizeFieldRefs(n: Node): IndexedSeq[Node] = {
    val sels = new ArrayBuffer[Node]
    def f(n: Node): Unit = n match {
      case Path(_) => sels += n
      case _: ProductNode | _: OptionApply | _: GetOrElse | _: TypeMapping | _: ClientSideOp =>
        n.nodeChildren.foreach(f)
    }
    f(n)
    sels
  }
}
