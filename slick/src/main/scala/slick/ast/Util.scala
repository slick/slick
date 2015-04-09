package slick.ast

import scala.language.implicitConversions
import scala.collection.mutable.{HashSet, ArrayBuffer}

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

  @inline def collect[T](pf: PartialFunction[Node, T], stopOnMatch: Boolean = false): Seq[T] =
    NodeOps.collect(tree, pf, stopOnMatch)

  def collectAll[T](pf: PartialFunction[Node, Seq[T]]): Seq[T] = collect[Seq[T]](pf).flatten

  def replace(f: PartialFunction[Node, Node], keepType: Boolean = false, bottomUp: Boolean = false): Node = NodeOps.replace(tree, f, keepType, bottomUp)

  def foreach[U](f: (Node => U)) {
    def g(n: Node) {
      f(n)
      n.nodeChildren.foreach(g)
    }
    g(tree)
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

  def select(field: Symbol): Node = (field, tree) match {
    case (s: AnonSymbol, StructNode(ch)) => ch.find{ case (s2,_) => s == s2 }.get._2
    case (s: FieldSymbol, StructNode(ch)) => ch.find{ case (s2,_) => s == s2 }.get._2
    case (s: ElementSymbol, ProductNode(ch)) => ch(s.idx-1)
    case (s, n) => Select(n, s)
  }

  def hasRefTo(s: Symbol): Boolean = findNode {
    case Ref(s2) if s2 == s => true
    case _ => false
  }.isDefined

  def hasRefToOneOf(s: Set[Symbol]): Boolean = findNode {
    case Ref(s2) if s contains s2 => true
    case _ => false
  }.isDefined
}

object NodeOps {
  import Util._

  // These methods should be in the class but 2.10.0-RC1 took away the ability
  // to use closures in value classes

  def collect[T](tree: Node, pf: PartialFunction[Node, T], stopOnMatch: Boolean): Seq[T] = {
    val b = new ArrayBuffer[T]
    def f(n: Node): Unit = pf.andThen[Unit] { case t =>
      b += t
      if(!stopOnMatch) n.nodeChildren.foreach(f)
    }.orElse[Node, Unit]{ case _ =>
      n.nodeChildren.foreach(f)
    }.apply(n)
    f(tree)
    b
  }

  def replace(tree: Node, f: PartialFunction[Node, Node], keepType: Boolean, bottomUp: Boolean): Node =
    if(bottomUp) f.applyOrElse(tree.nodeMapChildren(_.replace(f, keepType, bottomUp), keepType), identity[Node])
    else f.applyOrElse(tree, ({ case n: Node => n.nodeMapChildren(_.replace(f, keepType, bottomUp), keepType) }): PartialFunction[Node, Node])
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

  def linearizeFieldRefs(n: Node): IndexedSeq[Node] = {
    val sels = new ArrayBuffer[Node]
    def f(n: Node): Unit = n match {
      case _: Select | _: Ref | _: TableNode => sels += n
      case _: ProductNode | _: OptionApply | _: GetOrElse | _: TypeMapping | _: ClientSideOp =>
        n.nodeChildren.foreach(f)
    }
    f(n)
    sels
  }
}

object ProductOfCommonPaths {
  def unapply(n: ProductNode): Option[(Symbol, Vector[List[Symbol]])] = if(n.nodeChildren.isEmpty) None else
    n.nodeChildren.foldLeft(null: Option[(Symbol, Vector[List[Symbol]])]) {
      case (None, _) => None
      case (null, FwdPath(sym :: rest)) => Some((sym, Vector(rest)))
      case (Some((sym0, v)), FwdPath(sym :: rest)) if sym == sym0 => Some((sym, v :+ rest))
      case _ => None
    }
}
