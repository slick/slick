package slick.ast

import slick.ast.TypeUtil.:@

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
  import NodeOps._

  @inline def collect[T](pf: PartialFunction[Node, T], stopOnMatch: Boolean = false): Seq[T] = {
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

  def collectAll[T](pf: PartialFunction[Node, Seq[T]]): Seq[T] = collect[Seq[T]](pf).flatten

  def replace(f: PartialFunction[Node, Node], keepType: Boolean = false, bottomUp: Boolean = false): Node = {
    def g(n: Node): Node = n.mapChildren(_.replace(f, keepType, bottomUp), keepType)
    if(bottomUp) f.applyOrElse(g(tree), identity[Node]) else f.applyOrElse(tree, g)
  }

  /** Replace nodes in a bottom-up traversal with an extra state value that gets passed through the
    * traversal. Types are never kept or rebuilt when a node changes.
    *
    * @param f The replacement function that takes the current Node (whose children have already
    *          been transformed), the current state, and the original (untransformed) version of
    *          the Node. */
  def replaceFold[T](z: T)(f: PartialFunction[(Node, T, Node), (Node, T)]): (Node, T) = {
    var v: T = z
    val ch: IndexedSeq[Node] = tree.children.map { n =>
      val (n2, v2) = n.replaceFold(v)(f)
      v = v2
      n2
    }(collection.breakOut)
    val t2 = tree.withChildren(ch)
    f.applyOrElse((t2, v, tree), (t: (Node, T, Node)) => (t._1, t._2))
  }

  /** Replace nodes in a bottom-up traversal while invalidating TypeSymbols. Any later references
    * to the invalidated TypeSymbols have their types unassigned, so that the whole tree can be
    * retyped afterwards to get the correct new TypeSymbols in. */
  def replaceInvalidate(f: PartialFunction[(Node, Set[TypeSymbol], Node), (Node, Set[TypeSymbol])]): Node = {
    replaceFold(Set.empty[TypeSymbol])(f.orElse {
      case ((n: Ref), invalid, _) if containsTS(n.nodeType, invalid) => (n.untyped, invalid)
      case ((n: Select), invalid, _) if containsTS(n.nodeType, invalid) => (n.untyped, invalid)
    })._1
  }

  def untypeReferences(invalid: Set[TypeSymbol]): Node = {
    if(invalid.isEmpty) tree else replace({
      case n: Ref if containsTS(n.nodeType, invalid) => n.untyped
      case n: Select if containsTS(n.nodeType, invalid) => n.untyped
    }, bottomUp = true)
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

private object NodeOps {
  private def containsTS(t: Type, invalid: Set[TypeSymbol]): Boolean = {
    if(invalid.isEmpty) false else t match {
      case NominalType(ts, exp) => invalid.contains(ts) || containsTS(exp, invalid)
      case t => t.children.exists(ch => containsTS(ch, invalid))
    }
  }
}
