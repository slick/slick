package scala.slick.ast

import scala.slick.ast.TypeUtil._
import scala.slick.SlickException

/**
 * An operation which is expected to be run on the client side.
 */
trait ClientSideOp {
  def nodeMapServerSide(keepType: Boolean, r: Node => Node): Node
}

object ClientSideOp {
  /** Perform a computation only on the server side of a tree that may be
    * wrapped in client-side operations. Types are preserved unless
    * ``keepType`` is set to false. */
  def mapServerSide(n: Node, keepType: Boolean = true)(f: Node => Node): Node = n match {
    case n: ClientSideOp => n.nodeMapServerSide(keepType, (ch => mapServerSide(ch, keepType)(f)))
    case n => f(n)
  }
  def mapResultSetMapping(n: Node, keepType: Boolean = true)(f: ResultSetMapping => Node): Node = n match {
    case r: ResultSetMapping => f(r)
    case n: ClientSideOp => n.nodeMapServerSide(keepType, (ch => mapResultSetMapping(ch, keepType)(f)))
    case n => throw new SlickException("No ResultSetMapping found in tree")
  }
}

/** Get the first element of a collection. For client-side operations only. */
final case class First(val child: Node) extends UnaryNode with ClientSideOp {
  type Self = First
  protected[this] def nodeRebuild(ch: Node) = copy(child = ch)
  def nodeWithComputedType(scope: SymbolScope, typeChildren: Boolean, retype: Boolean): Self =
    if(nodeHasType && !typeChildren) this else {
      val this2 = nodeMapChildren(_.nodeWithComputedType(scope, typeChildren, retype), !retype)
      if(!nodeHasType || retype) {
        val tp = this2.nodeChildren.head.nodeType.asCollectionType.elementType
        nodeBuildTypedNode(this2, tp)
      } else this2
    }
  def nodeMapServerSide(keepType: Boolean, r: Node => Node) = nodeMapChildren(r, keepType)
}

/** A client-side projection of type
  * ``(CollectionType(c, t), u) => CollectionType(c, u)``. Unlike other nodes
  * which only operate on real collections, a ResultSetMapping may use an
  * Identity functor as its collection type constructor ``c``, thus giving it
  * a type of ``(t, u) => u`` where ``t`` and ``u`` are primitive or Option
  * types. */
final case class ResultSetMapping(generator: Symbol, from: Node, map: Node) extends BinaryNode with DefNode with ClientSideOp {
  type Self = ResultSetMapping
  def left = from
  def right = map
  override def nodeChildNames = Seq("from "+generator, "map")
  protected[this] def nodeRebuild(left: Node, right: Node) = copy(from = left, map = right)
  def nodeGenerators = Seq((generator, from))
  override def toString = "ResultSetMapping"
  protected[this] def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(generator = gen(0))
  def nodeWithComputedType(scope: SymbolScope, typeChildren: Boolean, retype: Boolean): Self =
    if(nodeHasType && !typeChildren) this else {
      val f2 = from.nodeWithComputedType(scope, typeChildren, retype)
      val (s2, newType) = f2.nodeType match {
        case CollectionType(cons, elem) =>
          val s2 = map.nodeWithComputedType(scope + (generator -> elem), typeChildren, retype)
          (s2, CollectionType(cons, s2.nodeType))
        case t =>
          val s2 = map.nodeWithComputedType(scope + (generator -> t), typeChildren, retype)
          (s2, s2.nodeType)
      }
      if(!nodeHasType || retype) {
        if((f2 eq from) && (s2 eq map) && newType == nodeType) this
        else copy(from = f2, map = s2).nodeTyped(newType)
      } else {
        if((f2 eq from) && (s2 eq map)) this
        else copy(from = f2, map = s2).nodeTyped(nodeType)
      }
    }
  def nodeMapServerSide(keepType: Boolean, r: Node => Node) = {
    val this2 = nodeMapScopedChildren {
      case (Some(_), ch) => r(ch)
      case (None, ch) => ch
    }
    if(keepType && nodeHasType) this2.nodeTyped(nodeType)
    else this2
  }
}
