package slick.ast

import slick.ast.Util._
import slick.ast.TypeUtil._
import slick.SlickException

/**
 * An operation which is expected to be run on the client side.
 */
trait ClientSideOp { this: Node =>
  def nodeMapServerSide(keepType: Boolean, r: Node => Node): Self
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
final case class First(val child: Node) extends UnaryNode with SimplyTypedNode with ClientSideOp {
  type Self = First
  protected[this] def rebuild(ch: Node) = copy(child = ch)
  protected def buildType = children.head.nodeType.asCollectionType.elementType
  def nodeMapServerSide(keepType: Boolean, r: Node => Node) = mapChildren(r, keepType)
}

/** A client-side projection of type
  * ``(CollectionType(c, t), u) => CollectionType(c, u)``. Unlike other nodes
  * which only operate on real collections, a ResultSetMapping may use an
  * Identity functor as its collection type constructor ``c``, thus giving it
  * a type of ``(t, u) => u`` where ``t`` and ``u`` are primitive or Option
  * types. */
final case class ResultSetMapping(generator: TermSymbol, from: Node, map: Node) extends BinaryNode with DefNode with ClientSideOp {
  type Self = ResultSetMapping
  def left = from
  def right = map
  override def childNames = Seq("from "+generator, "map")
  protected[this] def rebuild(left: Node, right: Node) = copy(from = left, map = right)
  def generators = Seq((generator, from))
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = "")
  protected[this] def rebuildWithSymbols(gen: IndexedSeq[TermSymbol]) = copy(generator = gen(0))
  def withInferredType(scope: Type.Scope, typeChildren: Boolean): Self = {
    val from2 = from.infer(scope, typeChildren)
    val (map2, newType) = from2.nodeType match {
      case CollectionType(cons, elem) =>
        val map2 = map.infer(scope + (generator -> elem), typeChildren)
        (map2, CollectionType(cons, map2.nodeType))
      case t =>
        val map2 = map.infer(scope + (generator -> t), typeChildren)
        (map2, map2.nodeType)
    }
    withChildren(Vector(from2, map2)) :@ (if(!hasType) newType else nodeType)
  }
  def nodeMapServerSide(keepType: Boolean, r: Node => Node) = {
    val this2 = mapScopedChildren {
      case (Some(_), ch) => r(ch)
      case (None, ch) => ch
    }
    if(keepType && hasType) this2 :@ nodeType
    else this2
  }
}

/** A switch for special-cased parameters that needs to be interpreted in order
  * to find the correct query string for the query arguments. */
final case class ParameterSwitch(cases: Seq[((Any => Boolean), Node)], default: Node) extends SimplyTypedNode with ClientSideOp {
  type Self = ParameterSwitch
  def children = cases.map(_._2) :+ default
  override def childNames = cases.map("[" + _._1 + "]") :+ "default"
  protected[this] def rebuild(ch: IndexedSeq[Node]): Self =
    copy(cases = (cases, ch).zipped.map { (c, n) => (c._1, n) }, default = ch.last)
  protected def buildType = default.nodeType
  def nodeMapServerSide(keepType: Boolean, r: Node => Node): Self = {
    val this2 = mapOrNone(children)(r).map(rebuild).getOrElse(this)
    if(keepType && hasType) this2 :@ nodeType
    else this2
  }
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = "")
}
