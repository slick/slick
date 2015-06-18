package slick.ast

import scala.language.implicitConversions
import scala.collection.mutable.ArrayBuffer

/**
 * Utility methods for AST manipulation.
 */
object Util {

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

/** Extra methods for Nodes. */
final class NodeOps(val tree: Node) extends AnyVal {
  import Util._

  @inline def collect[T](pf: PartialFunction[Node, T], stopOnMatch: Boolean = false): Seq[T] =
    NodeOps.collect(tree, pf, stopOnMatch)

  def collectAll[T](pf: PartialFunction[Node, Seq[T]]): Seq[T] = collect[Seq[T]](pf).flatten

  def replace(f: PartialFunction[Node, Node], keepType: Boolean = false, bottomUp: Boolean = false, retype: Boolean = false): Node =
    NodeOps.replace(tree, f, keepType, bottomUp, retype)

  def foreach[U](f: (Node => U)): Unit = {
    def g(n: Node) {
      f(n)
      n.children.foreach(g)
    }
    g(tree)
  }

  def findNode(p: Node => Boolean): Option[Node] = {
    if(p(tree)) Some(tree)
    else {
      val it = tree.children.iterator.map(_.findNode(p)).dropWhile(_.isEmpty)
      if(it.hasNext) it.next() else None
    }
  }

  def select(field: TermSymbol): Node = (field, tree) match {
    case (s: AnonSymbol, StructNode(ch)) => ch.find{ case (s2,_) => s == s2 }.get._2
    case (s: FieldSymbol, StructNode(ch)) => ch.find{ case (s2,_) => s == s2 }.get._2
    case (s: ElementSymbol, ProductNode(ch)) => ch(s.idx-1)
    case (s, n) => Select(n, s)
  }
}

object NodeOps {
  import Util._

  // These methods should be in the class but 2.10.0-RC1 took away the ability
  // to use closures in value classes

  def collect[T](tree: Node, pf: PartialFunction[Node, T], stopOnMatch: Boolean): Seq[T] = {
    val b = new ArrayBuffer[T]
    def f(n: Node): Unit = pf.andThen[Unit] { case t =>
      b += t
      if(!stopOnMatch) n.children.foreach(f)
    }.orElse[Node, Unit]{ case _ =>
      n.children.foreach(f)
    }.apply(n)
    f(tree)
    b
  }

  def replace(tree: Node, f: PartialFunction[Node, Node], keepType: Boolean, bottomUp: Boolean, retype: Boolean): Node =
    if(bottomUp) {
      val t2 = tree.mapChildren(n => replace(n, f, keepType, bottomUp, retype), keepType && !retype)
      val t3 = if(retype) t2.infer() else t2
      f.applyOrElse(t2, identity[Node])
    } else {
      def g(n: Node) = {
        val n2 = n.mapChildren(n => replace(n, f, keepType, bottomUp, retype), keepType && !retype)
        if(retype) n2.infer() else n2
      }
      f.applyOrElse(tree, g)
    }
}
