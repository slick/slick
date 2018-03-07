package slick.ast

import slick.util.ConstArray

import scala.collection.mutable
import scala.language.implicitConversions

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

  def collect[T](pf: PartialFunction[Node, T], stopOnMatch: Boolean = false): ConstArray[T] = {
    val retNull: (Node => T) = (_ => null.asInstanceOf[T])
    val b = ConstArray.newBuilder[T]()
    def f(n: Node): Unit = {
      val r = pf.applyOrElse(n, retNull)
      if(r.asInstanceOf[AnyRef] ne null) {
        b += r
        if(!stopOnMatch) n.childrenForeach(f)
      }
      else n.childrenForeach(f)
    }
    f(tree)
    b.result
  }

  def collectAll[T](pf: PartialFunction[Node, ConstArray[T]]): ConstArray[T] = collect[ConstArray[T]](pf).flatten

  def replace(f: PartialFunction[Node, Node], keepType: Boolean = false, bottomUp: Boolean = false): Node = {
    if(bottomUp) {
      def r(n: Node): Node = f.applyOrElse(g(n), identity[Node])
      def g(n: Node): Node = n.mapChildren(r, keepType)
      r(tree)
    } else {
      def r(n: Node): Node = f.applyOrElse(n, g)
      def g(n: Node): Node = n.mapChildren(r, keepType)
      r(tree)
    }
  }

  /** Replace nodes in a bottom-up traversal while invalidating TypeSymbols. Any later references
    * to the invalidated TypeSymbols have their types unassigned, so that the whole tree can be
    * retyped afterwards to get the correct new TypeSymbols in. The PartialFunction may return
    * `null`, which is considered the same as not matching. */
  def replaceInvalidate(f: PartialFunction[Node, (Node, TypeSymbol)]): Node = {
    import TypeUtil.typeToTypeUtil
    val invalid = mutable.HashSet.empty[TypeSymbol]
    val default = (_: Node) => null
    def tr(n: Node): Node = {
      val n2 = n.mapChildren(tr)
      val res = f.applyOrElse(n2, default)
      if(res ne null) {
        invalid += res._2
        res._1
      } else n2 match {
        case n2: PathElement if n2.nodeType.containsSymbol(invalid) => n2.untyped
        case _ => n2
      }
    }
    tr(tree)
  }

  def untypeReferences(invalid: Set[TypeSymbol]): Node = {
    import TypeUtil.typeToTypeUtil
    if(invalid.isEmpty) tree else replace({
      case n: PathElement if n.nodeType.containsSymbol(invalid) => n.untyped
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
