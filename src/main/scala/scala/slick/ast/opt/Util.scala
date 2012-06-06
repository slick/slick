package scala.slick.ast
package opt

import scala.language.implicitConversions
import collection.TraversableLike
import collection.generic.CanBuildFrom
import collection.mutable.ArrayBuffer

/**
 * Utility methods for the optimizers.
 */
object Util extends UtilLowPriority {

  def pfidentity[T]: PartialFunction[T, T] = { case x => x }

  def pftransitive[T <: AnyRef](pf: PartialFunction[T, T]): PartialFunction[T, T] = new PartialFunction[T, T] {
    def isDefinedAt(x: T): Boolean = pf.isDefinedAt(x)
    def apply(x: T): T = {
      val y = pf(x)
      if(y.eq(x) || !pf.isDefinedAt(y)) y
      else apply(y)
    }
  }

  def memoized[A, B](f: (A => B) => A => B): (A => B) = {
    val memo = new collection.mutable.HashMap[A, B]
    lazy val g = f(r)
    lazy val r: (A => B) = { a => memo.getOrElseUpdate(a, g(a)) }
    r
  }

  def mapOrSame[Coll <: TraversableLike[A, Coll], A <: AnyRef, To >: Coll](c: Coll, f: A => A)(implicit bf: CanBuildFrom[Coll, A, To]): To = {
    val b = bf.apply(c)
    var changed = false
    c.foreach { x =>
      val n = f(x)
      b += n
      if(n ne x) changed = true
    }
    if(changed) b.result() else c
  }

  def mapOrNone[Coll <: TraversableLike[A, Coll] with AnyRef, A <: AnyRef, To >: Coll <: AnyRef](c: Coll, f: A => A)(implicit bf: CanBuildFrom[Coll, A, To]): Option[To] = {
    val n = mapOrSame[Coll, A, To](c, f)(bf)
    if(c eq n) None else Some(n)
  }

  @inline implicit def nodeToNodeOps(n: Node): NodeOps = new NodeOps(n)
}

class UtilLowPriority {
  @inline implicit def nodeToTraversable(n: Node): Traversable[Node] = new NodeTraversable(n)
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
class NodeOps(val tree: Node) extends AnyVal {
  import Util._

  def collect[T](pf: PartialFunction[Node, T]): Iterable[T] = {
    val b = new ArrayBuffer[T]
    foreach(pf.andThen[Unit]{ case t => b += t }.orElse[Node, Unit]{ case _ => () })
    b
  }

  def collectAll[T](pf: PartialFunction[Node, Seq[T]]): Iterable[T] = collect[Seq[T]](pf).flatten

  def replace(f: PartialFunction[Node, Node]): Node = {
    val g = f.orElse(pfidentity[Node])
    memoized[Node, Node](r => { n => g(g(n).nodeMapChildren(r)) })(tree)
  }

  def replaceSymbols(f: Symbol => Symbol): Node = {
    replace {
      case d: DefNode => d.nodeMapGenerators(f)
      case r: RefNode => r.nodeMapReferences(f)
    }
  }

  def foreach[U](f: (Node => U)) {
    def g(n: Node) {
      f(n)
      n.nodeChildren.foreach(g)
    }
    g(tree)
  }

  def flattenProduct = {
    def f(n: Node): IndexedSeq[Node] = n match {
      case ProductNode(ns @ _*) => ns.flatMap(f).toIndexedSeq
      case n => IndexedSeq(n)
    }
    f(tree)
  }

  def mapChildrenWithScope(f: (Node, Scope) => Node, scope: Scope): Node = tree match {
    case d: DefNode =>
      var local = scope
      d.nodeMapScopedChildren { (symO, ch) =>
        val r = f(ch, local)
        symO.foreach(sym => local = local + (sym, r))
        r
      }
    case n => n.nodeMapChildren(ch => f(ch, scope))
  }
}

/** A Traversable instance for nodes. */
class NodeTraversable(tree: Node) extends Traversable[Node] {
  def foreach[U](f: (Node => U)) = new NodeOps(tree).foreach(f)
}

/** An extractor for the transitive source of a chain of FilteredQuery nodes */
object FilterChain {
  def unapply(n: Node): Some[(List[Symbol], Node)] = n match {
    case FilteredQuery(sym, from) =>
      val (ss, n) = unapply(from).get
      Some((sym :: ss, n))
    case n => Some(Nil, n)
  }

  def mapSource(n: Node, f: Node => Node): Node = n match {
    case q @ FilteredQuery(_, _) =>
      q.nodeMapFrom(mapSource(_, f))
    case source => f(source)
  }
}

/** An extractor for nested ProductNodes (does not match non-nested ones) */
object NestedProductNode {
  def unapplySeq(p: ProductNode): Option[Seq[Node]] = {
    var nested = false
    val b = new ArrayBuffer[Node]
    def scan(n: Node): Unit = n match {
      case ProductNode(ch @ _*) =>
        nested = true
        ch.foreach(scan)
      case n => b += n
    }
    p.nodeChildren.foreach(scan)
    if(nested) Some(b)
    else None
  }
}
