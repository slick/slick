package org.scalaquery.ast

import org.scalaquery.util.RefId
import collection.TraversableLike
import collection.generic.CanBuildFrom
import collection.mutable.ArrayBuffer

/**
 * Utility methods for the optimizers.
 */
object OptimizerUtil {

  def pfidentity[T]: PartialFunction[T, T] = { case x => x }

  def memoized[A, B](f: (A => B) => A => B): (A => B) = {
    val memo = new collection.mutable.HashMap[A, B]
    lazy val g = f(r)
    lazy val r: (A => B) = { a => memo.getOrElseUpdate(a, g(a)) }
    r
  }

  def refMemoized[A <: AnyRef, B](f: (A => B) => A => B): (A => B) = {
    val memo = new collection.mutable.HashMap[RefId[A], B]
    lazy val g = f(r)
    lazy val r: (A => B) = { a =>
      val ra = RefId(a)
      memo.getOrElseUpdate(ra, g(a))
    }
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

  def mapOrNone[Coll <: TraversableLike[A, Coll], A <: AnyRef, To >: Coll <: AnyRef](c: Coll, f: A => A)(implicit bf: CanBuildFrom[Coll, A, To]): Option[To] = {
    val n = mapOrSame[Coll, A, To](c, f)(bf)
    if(c eq n) None else Some(n)
  }

  implicit def nodeToNodeOps(n: Node): NodeOps = new NodeOps(n)
}

/**
 * Extra methods for Nodes.
 */
class NodeOps(tree: Node) extends Traversable[Node] {
  import OptimizerUtil._

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

  def foreach[U](f: (Node => U)) {
    def g(n: Node) {
      f(n)
      n.nodeChildren.foreach(g)
    }
    g(tree)
  }
}
