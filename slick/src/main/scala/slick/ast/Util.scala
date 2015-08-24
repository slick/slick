package slick.ast

import slick.ast.TypeUtil.:@
import slick.util.{ConstArray, ConstArrayBuilder}

import scala.collection
import scala.collection.mutable
import scala.language.implicitConversions
import scala.collection.mutable.ArrayBuffer

/**
 * Utility methods for AST manipulation.
 */
object Util {

  def mapOrNone[A <: AnyRef](c: Option[A])(f: A => A): Option[A] = {
    if(c.isEmpty) None else {
      val x = c.get
      val n = f(x)
      if(n ne x) Some(n) else None
    }
  }

  @inline implicit def nodeToNodeOps(n: Node): NodeOps = new NodeOps(n)
}

/** Extra methods for Nodes. */
final class NodeOps(val tree: Node) extends AnyVal {
  import Util._
  import NodeOps._

  @inline def collect[T](pf: PartialFunction[Node, T], stopOnMatch: Boolean = false): ConstArray[T] = {
    val b = new ConstArrayBuilder[T]
    def f(n: Node): Unit = pf.andThen[Unit] { case t =>
      b += t
      if(!stopOnMatch) n.children.foreach(f)
    }.orElse[Node, Unit]{ case _ =>
      n.children.foreach(f)
    }.apply(n)
    f(tree)
    b.result
  }

  def collectAll[T](pf: PartialFunction[Node, ConstArray[T]]): ConstArray[T] = collect[ConstArray[T]](pf).flatten

  def replace(f: PartialFunction[Node, Node], keepType: Boolean = false, bottomUp: Boolean = false): Node = {
    def g(n: Node): Node = n.mapChildren(_.replace(f, keepType, bottomUp), keepType)
    if(bottomUp) f.applyOrElse(g(tree), identity[Node]) else f.applyOrElse(tree, g)
  }

  /** Replace nodes in a bottom-up traversal while invalidating TypeSymbols. Any later references
    * to the invalidated TypeSymbols have their types unassigned, so that the whole tree can be
    * retyped afterwards to get the correct new TypeSymbols in. The PartialFunction may return
    * `null`, which is considered the same as not matching. */
  def replaceInvalidate(f: PartialFunction[Node, (Node, TypeSymbol)]): Node = {
    val invalid = mutable.HashSet.empty[TypeSymbol]
    val default = (_: Node) => null
    def tr(n: Node): Node = {
      val n2 = n.mapChildren(tr)
      val res = f.applyOrElse(n2, default)
      if(res ne null) {
        invalid += res._2
        res._1
      } else n2 match {
        case n2: PathElement if containsTS(n2.nodeType, invalid) => n2.untyped
        case _ => n2
      }
    }
    tr(tree)
  }

  def untypeReferences(invalid: Set[TypeSymbol]): Node = {
    if(invalid.isEmpty) tree else replace({
      case n: PathElement if containsTS(n.nodeType, invalid) => n.untyped
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
  private def containsTS(t: Type, invalid: collection.Set[TypeSymbol]): Boolean = {
    if(invalid.isEmpty) false else t match {
      case NominalType(ts, exp) => invalid.contains(ts) || containsTS(exp, invalid)
      case t: AtomicType => false
      case t => t.children.exists(ch => containsTS(ch, invalid))
    }
  }
}
