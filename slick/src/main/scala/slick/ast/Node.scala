package slick.ast

import scala.collection.mutable.ListBuffer
import scala.language.existentials
import scala.reflect.ClassTag
import slick.SlickException
import slick.util.{Logging, Dumpable, DumpInfo, GlobalConfig, ConstArray}
import Util._
import TypeUtil._

/** A node in the Slick AST.
  * Every Node has a number of child nodes and an optional type annotation. */
trait Node extends Dumpable {
  type Self >: this.type <: Node

  private[this] var seenType: Boolean = false
  private var _type: Type = UnassignedType

  /** All child nodes of this node. Must be implemented by subclasses. */
  def children: ConstArray[Node]

  /** Names for the child nodes to show in AST dumps. Defaults to a numbered sequence starting at 0
    * but can be overridden by subclasses to produce more suitable names. */
  def childNames: Iterable[String] = Stream.from(0).map(_.toString)

  /** Rebuild this node with a new list of children. Implementations of this method must not reuse
    * the current node. This method always returns a fresh copy. */
  protected[this] def rebuild(ch: ConstArray[Node]): Self

  /** Build a copy of this node with the current children. */
  protected[this] def buildCopy: Self = rebuild(children)

  /** Rebuild this node with new child nodes unless all children are identical to the current ones,
    * in which case this node is returned. */
  final def withChildren(ch2: ConstArray[Node]): Self = {
    val ch = children
    val len = ch.length
    var i = 0
    while(i < len) {
      if(ch(i) ne ch2(i)) return rebuild(ch2)
      i += 1
    }
    this
  }

  /** Apply a mapping function to all children of this node and recreate the node with the new
    * children. If all new children are identical to the old ones, this node is returned. If
    * ``keepType`` is true, the type of this node is kept even when the children have changed. */
  def mapChildren(f: Node => Node, keepType: Boolean = false): Self = {
    val ch = children
    val ch2 = ch.endoMap(f)
    val n: Self = if(ch2 eq ch) this else rebuild(ch2)
    if(!keepType || (_type eq UnassignedType)) n else (n :@ _type).asInstanceOf[Self]
  }

  /** Apply a side-effecting function to all direct children from left to right. Note that
    * {{{ n.childrenForeach(f) }}} is equivalent to {{{ n.children.foreach(f) }}} but can be
    * implemented more efficiently in `Node` subclasses. */
  def childrenForeach[R](f: Node => R): Unit =
    children.foreach(f)

  /** The current type of this node. */
  def nodeType: Type = {
    seenType = true
    _type
  }

  /** Get the current type of this node for debug output without marking it as seen. */
  protected[this] def peekType: Type = _type

  /** Check if this node has a type without marking the type as seen. */
  def hasType: Boolean = _type != UnassignedType

  /** Return this Node with no Type assigned (if it has not yet been observed) or an untyped copy. */
  final def untyped: Self =
    if(seenType || _type != UnassignedType) buildCopy else this

  /** Return this Node with a Type assigned (if no other type has been seen for it yet) or a typed copy. */
  final def :@ (newType: Type): Self = {
    val n: Self = if(seenType && newType != _type) buildCopy else this
    n._type = newType
    n
  }

  /** Rebuild this node and all children with their computed type. If this node already has a type,
    * the children are only type-checked again if ``typeChildren`` is true. if ``retype`` is also
    * true, the existing type of this node is replaced. If this node does not yet have a type, the
    * types of all children are computed first. */
  final def infer(scope: Type.Scope = Map.empty, typeChildren: Boolean = false): Self =
    if(hasType && !typeChildren) this else withInferredType(scope, typeChildren)

  protected[this] def withInferredType(scope: Type.Scope, typeChildren: Boolean): Self

  def getDumpInfo = {
    val (objName, mainInfo) = this match {
      case p: Product =>
        val cln = DumpInfo.simpleNameFor(getClass)
        val n = if(cln.endsWith("$")) cln.substring(0, cln.length - 1) else cln.replaceFirst(".*\\$", "")
        val args = p.productIterator.filterNot(_.isInstanceOf[Node]).mkString(", ")
        (n, args)
      case _ => (super.toString, "")
    }
    val t = peekType
    val ch = this match {
      // Omit path details unless dumpPaths is set
      case Path(l @ (_ :: _ :: _)) if !GlobalConfig.dumpPaths => Vector.empty
      case _ => childNames.zip(children.toSeq).toVector
    }
    DumpInfo(objName, mainInfo, if(t != UnassignedType) ": " + t.toString else "", ch)
  }

  override final def toString = getDumpInfo.getNamePlusMainInfo
}

/** A Node which can be typed without access to its scope, and whose children can be typed
  * independently of each other. */
trait SimplyTypedNode extends Node {
  type Self >: this.type <: SimplyTypedNode

  protected def buildType: Type

  final def withInferredType(scope: Type.Scope, typeChildren: Boolean): Self = {
    val this2: Self = mapChildren(_.infer(scope, typeChildren), keepType = true)
    if(!hasType) (this2 :@ this2.buildType).asInstanceOf[Self] else this2
  }
}

/** An expression that represents a conjunction of expressions. */
final case class ProductNode(children: ConstArray[Node]) extends SimplyTypedNode {
  type Self = ProductNode
  override def getDumpInfo = super.getDumpInfo.copy(name = "ProductNode", mainInfo = "")
  protected[this] def rebuild(ch: ConstArray[Node]): Self = copy(ch)
  override def childNames: Iterable[String] = Stream.from(1).map(_.toString)
  protected def buildType: Type = ProductType(children.map { ch =>
    val t = ch.nodeType
    if(t == UnassignedType) throw new SlickException(s"ProductNode child $ch has UnassignedType")
    t
  })
  def flatten: ProductNode = {
    def f(n: Node): ConstArray[Node] = n match {
      case ProductNode(ns) => ns.flatMap(f)
      case StructNode(els) => els.flatMap(el => f(el._2))
      case n => ConstArray(n)
    }
    ProductNode(f(this))
  }
}

/** An expression that represents a structure, i.e. a conjunction where the
  * individual components have Symbols associated with them. */
final case class StructNode(elements: ConstArray[(TermSymbol, Node)]) extends SimplyTypedNode with DefNode {
  type Self = StructNode
  override def getDumpInfo = super.getDumpInfo.copy(name = "StructNode", mainInfo = "")
  override def childNames = elements.map(_._1.toString).toSeq
  val children = elements.map(_._2)
  override protected[this] def rebuild(ch: ConstArray[Node]) =
    new StructNode(elements.zip(ch).map{ case ((s,_),n) => (s,n) })
  def generators = elements
  protected[this] def rebuildWithSymbols(gen: ConstArray[TermSymbol]): Node =
    copy(elements = elements.zip(gen).map { case (e, s) => (s, e._2) })

  override protected def buildType: Type = StructType(elements.map { case (s, n) =>
    val t = n.nodeType
    if(t == UnassignedType) throw new SlickException(s"StructNode child $s has UnassignedType")
    (s, t)
  })
}

/** A literal value expression.
  *
  * @param volatileHint Indicates whether this value should be considered volatile, i.e. it
  *                     contains user-generated data or may change in future executions of what
  *                     is otherwise the same query. A database back-end should usually turn
  *                     volatile constants into bind variables. */
class LiteralNode(val buildType: Type, val value: Any, val volatileHint: Boolean = false) extends NullaryNode with SimplyTypedNode {
  type Self = LiteralNode
  override def getDumpInfo = super.getDumpInfo.copy(name = "LiteralNode", mainInfo = s"$value (volatileHint=$volatileHint)")
  protected[this] def rebuild = new LiteralNode(buildType, value, volatileHint)

  override def hashCode = buildType.hashCode() + (if(value == null) 0 else value.asInstanceOf[AnyRef].hashCode)
  override def equals(o: Any) = o match {
    case l: LiteralNode => buildType == l.buildType && value == l.value
    case _ => false
  }
}

object LiteralNode {
  def apply(tp: Type, v: Any, vol: Boolean = false): LiteralNode = new LiteralNode(tp, v, vol)
  def apply[T](v: T)(implicit tp: ScalaBaseType[T]): LiteralNode = apply(tp, v)
  def unapply(n: LiteralNode): Option[Any] = Some(n.value)

  private[slick] val nullOption = LiteralNode(ScalaBaseType.nullType.optionType, None)
}

trait BinaryNode extends Node {
  def left: Node
  def right: Node
  lazy val children = ConstArray(left, right)
  protected[this] final def rebuild(ch: ConstArray[Node]): Self = rebuild(ch(0), ch(1))
  protected[this] def rebuild(left: Node, right: Node): Self
  override final def mapChildren(f: Node => Node, keepType: Boolean = false): Self = {
    val l = left
    val r = right
    val l2 = f(l)
    val r2 = f(r)
    val n: Self = if((l eq l2) && (r eq r2)) this else rebuild(l2, r2)
    val _type = peekType
    if(!keepType || (_type eq UnassignedType)) n else (n :@ _type).asInstanceOf[Self]
  }
  override final protected[this] def buildCopy: Self = rebuild(left, right)
  override final def childrenForeach[R](f: Node => R): Unit = {
    f(left)
    f(right)
  }
}

trait UnaryNode extends Node {
  def child: Node
  lazy val children = ConstArray(child)
  protected[this] final def rebuild(ch: ConstArray[Node]): Self = rebuild(ch(0))
  protected[this] def rebuild(child: Node): Self
  override final def mapChildren(f: Node => Node, keepType: Boolean = false): Self = {
    val ch = child
    val ch2 = f(child)
    val n: Self = if(ch2 eq ch) this else rebuild(ch2)
    val _type = peekType
    if(!keepType || (_type eq UnassignedType)) n else (n :@ _type).asInstanceOf[Self]
  }
  override final protected[this] def buildCopy: Self = rebuild(child)
  override final def childrenForeach[R](f: Node => R): Unit = f(child)
}

trait NullaryNode extends Node {
  def children = ConstArray.empty
  protected[this] final def rebuild(ch: ConstArray[Node]): Self = rebuild
  protected[this] def rebuild: Self
  override final def mapChildren(f: Node => Node, keepType: Boolean = false): Self = this
  override final protected[this] def buildCopy: Self = rebuild
  override final def childrenForeach[R](f: Node => R): Unit = ()
}

/** An expression that represents a plain value lifted into a Query. */
final case class Pure(value: Node, identity: TypeSymbol = new AnonTypeSymbol) extends UnaryNode with SimplyTypedNode with TypeGenerator {
  type Self = Pure
  def child = value
  override def childNames = Seq("value")
  protected[this] def rebuild(child: Node) = copy(child)
  protected def buildType = CollectionType(TypedCollectionTypeConstructor.seq, NominalType(identity, value.nodeType))
}

final case class CollectionCast(child: Node, cons: CollectionTypeConstructor) extends UnaryNode with SimplyTypedNode {
  type Self = CollectionCast
  protected[this] def rebuild(child: Node) = copy(child = child)
  protected def buildType = CollectionType(cons, child.nodeType.asCollectionType.elementType)
  def nodeMapServerSide(keepType: Boolean, r: Node => Node) = mapChildren(r, keepType)
}

/** Forces a subquery to be created in `mergeToComprehension` if it occurs between two other
  * collection-valued operations that would otherwise be fused, and the subquery condition
  * is true. */
final case class Subquery(child: Node, condition: Subquery.Condition) extends UnaryNode with SimplyTypedNode {
  type Self = Subquery
  protected[this] def rebuild(child: Node) = copy(child = child)
  protected def buildType = child.nodeType
}

object Subquery {
  sealed trait Condition
  /** Always create a subquery but allow purely aliasing projections to be pushed down */
  case object Default extends Condition
  /** A Subquery boundary below the mapping operation that adds a ROWNUM */
  case object BelowRownum extends Condition
  /** A Subquery boundary above the mapping operation that adds a ROWNUM */
  case object AboveRownum extends Condition
  /** A Subquery boundary below the mapping operation that adds a ROW_NUMBER */
  case object BelowRowNumber extends Condition
  /** A Subquery boundary above the mapping operation that adds a ROW_NUMBER */
  case object AboveRowNumber extends Condition
  /** A Subquery boundary above a DISTINCT without explicit column specification */
  case object AboveDistinct extends Condition
}

/** Common superclass for expressions of type (CollectionType(c, t), _) => CollectionType(c, t). */
abstract class FilteredQuery extends Node {
  type Self >: this.type <: FilteredQuery
  def from: Node
}

/** A FilteredQuery without a Symbol. */
abstract class SimpleFilteredQuery extends FilteredQuery with SimplyTypedNode {
  type Self >: this.type <: SimpleFilteredQuery
  def buildType = from.nodeType
}

/** A FilteredQuery with a Symbol. */
abstract class ComplexFilteredQuery extends FilteredQuery with DefNode {
  type Self >: this.type <: ComplexFilteredQuery
  protected[this] def generator: TermSymbol
  def generators = ConstArray((generator, from))
  def withInferredType(scope: Type.Scope, typeChildren: Boolean): Self = {
    val from2 = from.infer(scope, typeChildren)
    val genScope = scope + (generator -> from2.nodeType.asCollectionType.elementType)
    val this2 = mapChildren { ch =>
      if(ch eq from) from2 else ch.infer(genScope, typeChildren)
    }
    (this2 :@ (if(!hasType) this2.from.nodeType else nodeType)).asInstanceOf[Self]
  }
}

/** A .filter call of type (CollectionType(c, t), Boolean) => CollectionType(c, t). */
final case class Filter(generator: TermSymbol, from: Node, where: Node) extends ComplexFilteredQuery with BinaryNode {
  type Self = Filter
  def left = from
  def right = where
  override def childNames = Seq("from "+generator, "where")
  protected[this] def rebuild(left: Node, right: Node) = copy(from = left, where = right)
  protected[this] def rebuildWithSymbols(gen: ConstArray[TermSymbol]) = copy(generator = gen(0))
}

object Filter {
  def ifRefutable(generator: TermSymbol, from: Node, where: Node): Node = where match {
    case LiteralNode(true) => from
    case _ => Filter(generator, from, where)
  }
}

/** A .sortBy call of type (CollectionType(c, t), _) => CollectionType(c, t). */
final case class SortBy(generator: TermSymbol, from: Node, by: ConstArray[(Node, Ordering)]) extends ComplexFilteredQuery {
  type Self = SortBy
  lazy val children = from +: by.map(_._1)
  protected[this] def rebuild(ch: ConstArray[Node]) =
    copy(from = ch(0), by = by.zip(ch.tail).map{ case ((_, o), n) => (n, o) })
  override def childNames = ("from "+generator) +: by.zipWithIndex.map("by" + _._2).toSeq
  protected[this] def rebuildWithSymbols(gen: ConstArray[TermSymbol]) = copy(generator = gen(0))
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = by.map(_._2).mkString(", "))
}

final case class Ordering(direction: Ordering.Direction = Ordering.Asc, nulls: Ordering.NullOrdering = Ordering.NullsDefault) {
  def asc = copy(direction = Ordering.Asc)
  def desc = copy(direction = Ordering.Desc)
  def reverse = copy(direction = direction.reverse)
  def nullsDefault = copy(nulls = Ordering.NullsDefault)
  def nullsFirst = copy(nulls = Ordering.NullsFirst)
  def nullsLast = copy(nulls = Ordering.NullsLast)
}

object Ordering {
  sealed abstract class NullOrdering(val first: Boolean, val last: Boolean)
  final case object NullsDefault extends NullOrdering(false, false)
  final case object NullsFirst extends NullOrdering(true, false)
  final case object NullsLast extends NullOrdering(false, true)

  sealed abstract class Direction(val desc: Boolean) { def reverse: Direction }
  final case object Asc extends Direction(false) { def reverse = Desc }
  final case object Desc extends Direction(true) { def reverse = Asc }
}

/** A .groupBy call. */
final case class GroupBy(fromGen: TermSymbol, from: Node, by: Node, identity: TypeSymbol = new AnonTypeSymbol) extends BinaryNode with DefNode with TypeGenerator {
  type Self = GroupBy
  def left = from
  def right = by
  override def childNames = Seq("from "+fromGen, "by")
  protected[this] def rebuild(left: Node, right: Node) = copy(from = left, by = right)
  protected[this] def rebuildWithSymbols(gen: ConstArray[TermSymbol]) = copy(fromGen = gen(0))
  def generators = ConstArray((fromGen, from))
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = identity.toString)
  def withInferredType(scope: Type.Scope, typeChildren: Boolean): Self = {
    val from2 = from.infer(scope, typeChildren)
    val from2Type = from2.nodeType.asCollectionType
    val by2 = by.infer(scope + (fromGen -> from2Type.elementType), typeChildren)
    val this2 = if((from2 eq from) && (by2 eq by)) this else copy(from = from2, by = by2)
    this2 :@ (
      if(!hasType)
        CollectionType(from2Type.cons, ProductType(ConstArray(NominalType(identity, by2.nodeType), CollectionType(TypedCollectionTypeConstructor.seq, from2Type.elementType))))
      else nodeType)
  }
}

/** A .forUpdate call */
final case class ForUpdate(generator: TermSymbol, from: Node) extends ComplexFilteredQuery {
  type Self = ForUpdate
  lazy val children = ConstArray(from)
  protected[this] def rebuild(ch: ConstArray[Node]) = copy(from = ch(0))
  protected[this] def rebuildWithSymbols(gen: ConstArray[TermSymbol]) = copy(generator = gen(0))
}

/** A .take call. */
final case class Take(from: Node, count: Node) extends SimpleFilteredQuery with BinaryNode {
  type Self = Take
  def left = from
  def right = count
  override def childNames = Seq("from", "count")
  protected[this] def rebuild(left: Node, right: Node) = copy(from = left, count = right)
}

/** A .drop call. */
final case class Drop(from: Node, count: Node) extends SimpleFilteredQuery with BinaryNode {
  type Self = Drop
  def left = from
  def right = count
  override def childNames = Seq("from", "count")
  protected[this] def rebuild(left: Node, right: Node) = copy(from = left, count = right)
}

/** A .distinct call of type (CollectionType(c, t), _) => CollectionType(c, t). */
final case class Distinct(generator: TermSymbol, from: Node, on: Node) extends ComplexFilteredQuery with BinaryNode {
  type Self = Distinct
  def left = from
  def right = on
  override def childNames = Seq("from", "on")
  protected[this] def rebuild(left: Node, right: Node) = copy(from = left, on = right)
  protected[this] def rebuildWithSymbols(gen: ConstArray[TermSymbol]) = copy(generator = gen(0))
}

/** A join expression. For joins without option extension, the type rule is
  * (CollectionType(c, t), CollectionType(_, u)) => CollecionType(c, (t, u)).
  * Option-extended left outer joins are typed as
  * (CollectionType(c, t), CollectionType(_, u)) => CollecionType(c, (t, Option(u))),
  * Option-extended right outer joins as
  * (CollectionType(c, t), CollectionType(_, u)) => CollecionType(c, (Option(t), u))
  * and Option-extended full outer joins as
  * (CollectionType(c, t), CollectionType(_, u)) => CollecionType(c, (Option(t), Option(u))). */
final case class Join(leftGen: TermSymbol, rightGen: TermSymbol, left: Node, right: Node, jt: JoinType, on: Node) extends DefNode {
  type Self = Join
  lazy val children = ConstArray(left, right, on)
  protected[this] def rebuild(ch: ConstArray[Node]) = copy(left = ch(0), right = ch(1), on = ch(2))
  override def childNames = Seq("left "+leftGen, "right "+rightGen, "on")
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = jt.toString)
  def generators = ConstArray((leftGen, left), (rightGen, right))
  protected[this] def rebuildWithSymbols(gen: ConstArray[TermSymbol]) =
    copy(leftGen = gen(0), rightGen = gen(1))
  def withInferredType(scope: Type.Scope, typeChildren: Boolean): Self = {
    val left2 = left.infer(scope, typeChildren)
    val right2 = right.infer(scope, typeChildren)
    val left2Type = left2.nodeType.asCollectionType
    val right2Type = right2.nodeType.asCollectionType
    val on2 = on.infer(scope + (leftGen -> left2Type.elementType) + (rightGen -> right2Type.elementType), typeChildren)
    val (joinedLeftType, joinedRightType) = jt match {
      case JoinType.LeftOption => (left2Type.elementType, OptionType(right2Type.elementType))
      case JoinType.RightOption => (OptionType(left2Type.elementType), right2Type.elementType)
      case JoinType.OuterOption => (OptionType(left2Type.elementType), OptionType(right2Type.elementType))
      case _ => (left2Type.elementType, right2Type.elementType)
    }
    withChildren(ConstArray[Node](left2, right2, on2)) :@ (
      if(!hasType) CollectionType(left2Type.cons, ProductType(ConstArray(joinedLeftType, joinedRightType)))
      else nodeType)
  }
}

/** A union of type
  * (CollectionType(c, t), CollectionType(_, t)) => CollectionType(c, t). */
final case class Union(left: Node, right: Node, all: Boolean) extends BinaryNode with SimplyTypedNode {
  type Self = Union
  protected[this] def rebuild(left: Node, right: Node) = copy(left = left, right = right)
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = if(all) "all" else "")
  override def childNames = Seq("left", "right")
  protected def buildType = left.nodeType
}

/** A .flatMap call of type
  * (CollectionType(c, _), CollectionType(_, u)) => CollectionType(c, u). */
final case class Bind(generator: TermSymbol, from: Node, select: Node) extends BinaryNode with DefNode {
  type Self = Bind
  def left = from
  def right = select
  override def childNames = Seq("from "+generator, "select")
  protected[this] def rebuild(left: Node, right: Node) = copy(from = left, select = right)
  def generators = ConstArray((generator, from))
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = "")
  protected[this] def rebuildWithSymbols(gen: ConstArray[TermSymbol]) = copy(generator = gen(0))
  def withInferredType(scope: Type.Scope, typeChildren: Boolean): Self = {
    val from2 = from.infer(scope, typeChildren)
    val from2Type = from2.nodeType.asCollectionType
    val select2 = select.infer(scope + (generator -> from2Type.elementType), typeChildren)
    val withCh = if((from2 eq from) && (select2 eq select)) this else rebuild(from2, select2)
    withCh :@ (
      if(!hasType) CollectionType(from2Type.cons, select2.nodeType.asCollectionType.elementType)
      else nodeType)
  }
}

/** An aggregation function application which is similar to a Bind(_, _, Pure(_)) where the
  * projection contains a mapping function application. The return type is an aggregated
  * scalar value though, not a collection. */
final case class Aggregate(sym: TermSymbol, from: Node, select: Node) extends BinaryNode with DefNode {
  type Self = Aggregate
  def left = from
  def right = select
  override def childNames = Seq("from "+sym, "select")
  protected[this] def rebuild(left: Node, right: Node) = copy(from = left, select = right)
  def generators = ConstArray((sym, from))
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = "")
  protected[this] def rebuildWithSymbols(gen: ConstArray[TermSymbol]) = copy(sym = gen(0))
  def withInferredType(scope: Type.Scope, typeChildren: Boolean): Self = {
    val from2 :@ CollectionType(_, el) = from.infer(scope, typeChildren)
    val select2 = select.infer(scope + (sym -> el), typeChildren)
    val this2 = if((from2 eq from) && (select2 eq select)) this else copy(from = from2, select = select2)
    this2 :@ (if(!hasType) select2.nodeType else nodeType)
  }
}

/** A table together with its expansion into columns. */
final case class TableExpansion(generator: TermSymbol, table: Node, columns: Node) extends BinaryNode with DefNode {
  type Self = TableExpansion
  def left = table
  def right = columns
  override def childNames = Seq("table "+generator, "columns")
  protected[this] def rebuild(left: Node, right: Node) = copy(table = left, columns = right)
  def generators = ConstArray((generator, table))
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = "")
  protected[this] def rebuildWithSymbols(gen: ConstArray[TermSymbol]) = copy(generator = gen(0))
  def withInferredType(scope: Type.Scope, typeChildren: Boolean): Self = {
    val table2 = table.infer(scope, typeChildren)
    val columns2 = columns.infer(scope + (generator -> table2.nodeType.asCollectionType.elementType), typeChildren)
    val this2 = if((table2 eq table) && (columns2 eq columns)) this else copy(table = table2, columns = columns2)
    this2 :@ (if(!hasType) table2.nodeType else nodeType)
  }
}

trait PathElement extends Node {
  def sym: TermSymbol
  def pathString: String
  def untypedPath: PathElement
}

/** An expression that selects a field in another expression. */
final case class Select(in: Node, field: TermSymbol) extends PathElement with UnaryNode with SimplyTypedNode {
  def sym = field
  type Self = Select
  def child = in
  override def childNames = Seq("in")
  protected[this] def rebuild(child: Node) = copy(in = child)
  override def getDumpInfo = Path.unapply(this) match {
    case Some(l) => super.getDumpInfo.copy(name = "Path", mainInfo = l.reverseIterator.mkString("."))
    case None => super.getDumpInfo
  }
  protected def buildType = in.nodeType.select(field)
  def pathString = in.asInstanceOf[PathElement].pathString+"."+field
  def untypedPath = {
    val in2 = in.asInstanceOf[PathElement].untypedPath
    if(in2 eq in) untyped else Select(in2, field)
  }
}

/** A function call expression. */
final case class Apply(sym: TermSymbol, children: ConstArray[Node])(val buildType: Type) extends SimplyTypedNode {
  type Self = Apply
  protected[this] def rebuild(ch: ConstArray[slick.ast.Node]) = copy(children = ch)(buildType)
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = sym.toString)
}

/** A reference to a Symbol */
final case class Ref(sym: TermSymbol) extends PathElement with NullaryNode {
  type Self = Ref
  def withInferredType(scope: Type.Scope, typeChildren: Boolean): Self =
    if(hasType) this else {
      scope.get(sym) match {
        case Some(t) => this :@ t
        case _ => throw new SlickException("No type for symbol "+sym+" found for "+this)
      }
    }
  def rebuild = copy()
  def pathString = sym.toString
  def untypedPath = untyped
}

/** A constructor/extractor for nested Selects starting at a Ref so that, for example,
  * `c :: b :: a :: Nil` corresponds to path `a.b.c`. */
object Path {
  def apply(l: List[TermSymbol]): PathElement = l match {
    case s :: Nil => Ref(s)
    case s :: l => Select(apply(l), s)
    case _ => throw new SlickException("Empty Path")
  }
  def unapply(n: PathElement): Option[List[TermSymbol]] = {
    var l = new ListBuffer[TermSymbol]
    var el: Node = n
    while(el.isInstanceOf[Select]) {
      val sel = el.asInstanceOf[Select]
      l += sel.sym
      el = sel.child
    }
    el match {
      case Ref(sym) =>
        l += sym
        Some(l.toList)
      case _ => None
    }
  }
  def toString(path: Seq[TermSymbol]): String = path.reverseIterator.mkString("Path ", ".", "")
  def toString(s: Select): String = s match {
    case Path(syms) => toString(syms)
    case n => n.toString
  }
}

/** A constructor/extractor for nested Selects starting at a Ref so that, for example,
  * `a :: b :: c :: Nil` corresponds to path `a.b.c`. */
object FwdPath {
  def apply(ch: List[TermSymbol]): PathElement = {
    var p: PathElement = Ref(ch.head)
    ch.tail.foreach { sym => p = Select(p, sym) }
    p
  }
  def unapply(n: PathElement): Option[List[TermSymbol]] = {
    var l: List[TermSymbol] = Nil
    var el: Node = n
    while(el.isInstanceOf[Select]) {
      val sel = el.asInstanceOf[Select]
      l = sel.sym :: l
      el = sel.child
    }
    el match {
      case Ref(sym) => Some(sym :: l)
      case _ => None
    }
  }
  def toString(path: Seq[TermSymbol]): String = path.mkString("Path ", ".", "")
}

/** A Node representing a database table. */
final case class TableNode(schemaName: Option[String], tableName: String, identity: TableIdentitySymbol, baseIdentity: TableIdentitySymbol)(val profileTable: Any) extends NullaryNode with SimplyTypedNode with TypeGenerator {
  type Self = TableNode
  def buildType = CollectionType(TypedCollectionTypeConstructor.seq, NominalType(identity, UnassignedType))
  def rebuild = copy()(profileTable)
  override def getDumpInfo = super.getDumpInfo.copy(name = "Table", mainInfo = schemaName.map(_ + ".").getOrElse("") + tableName)
}

/** A node that represents an SQL sequence. */
final case class SequenceNode(name: String)(val increment: Long) extends NullaryNode with SimplyTypedNode {
  type Self = SequenceNode
  def buildType = ScalaBaseType.longType
  def rebuild = copy()(increment)
}

/** A Query of this special Node represents an infinite stream of consecutive
  * numbers starting at the given number. This is used as an operand for
  * zipWithIndex. It is not exposed directly in the query language because it
  * cannot be represented in SQL outside of a 'zip' operation. */
final case class RangeFrom(start: Long = 1L) extends NullaryNode with SimplyTypedNode {
  type Self = RangeFrom
  def buildType = CollectionType(TypedCollectionTypeConstructor.seq, ScalaBaseType.longType)
  def rebuild = copy()
}

/** A conditional expression; The clauses should be: `(if then)+ else`.
  * The result type is taken from the first `then` (i.e. the second clause). */
final case class IfThenElse(clauses: ConstArray[Node]) extends SimplyTypedNode {
  type Self = IfThenElse
  def children = clauses
  override def childNames = (0 until clauses.length-1).map { i => if(i%2 == 0) "if" else "then" } :+ "else"
  protected[this] def rebuild(ch: ConstArray[Node]): Self = copy(clauses = ch)
  protected def buildType = clauses(1).nodeType
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = "")
  private[this] def mapClauses(f: Node => Node, keepType: Boolean, pred: Int => Boolean): IfThenElse = {
    var equal = true
    val mapped = clauses.zipWithIndex.map { case (n, i) =>
      val n2 = if(pred(i)) f(n) else n
      if(n2 ne n) equal = false
      n2
    }
    val this2 = if(equal) this else rebuild(mapped)
    if(peekType == UnassignedType || !keepType) this2 else this2 :@ peekType
  }
  def mapConditionClauses(f: Node => Node, keepType: Boolean = false) =
    mapClauses(f, keepType, (i => i%2 == 0 && i != clauses.length-1))
  def mapResultClauses(f: Node => Node, keepType: Boolean = false) =
    mapClauses(f, keepType, (i => i%2 == 1 || i == clauses.length-1))
  def ifThenClauses: Iterator[(Node, Node)] =
    clauses.iterator.grouped(2).withPartial(false).map { case List(i, t) => (i, t) }
  def elseClause = clauses.last
  /** Return a null-extended version of a single-column IfThenElse expression */
  def nullExtend: IfThenElse = { //TODO 3.2: Remove this method. It is only preserved for binary compatibility in 3.1.1
    def isOpt(n: Node) = n match {
      case LiteralNode(null) => true
      case _ :@ OptionType(_) => true
      case _ => false
    }
    val hasOpt = (ifThenClauses.map(_._2) ++ Iterator(elseClause)).exists(isOpt)
    if(hasOpt) mapResultClauses(ch => if(isOpt(ch)) ch else OptionApply(ch)).infer()
    else this
  }
}

/** Lift a value into an Option as Some (or None if the value is a `null` column). */
final case class OptionApply(child: Node) extends UnaryNode with SimplyTypedNode {
  type Self = OptionApply
  protected[this] def rebuild(ch: Node) = copy(child = ch)
  protected def buildType = OptionType(child.nodeType)
}

/** The catamorphism of OptionType. */
final case class OptionFold(from: Node, ifEmpty: Node, map: Node, gen: TermSymbol) extends DefNode {
  type Self = OptionFold
  lazy val children = ConstArray(from, ifEmpty, map)
  def generators = ConstArray((gen, from))
  override def childNames = Vector("from "+gen, "ifEmpty", "map")
  protected[this] def rebuild(ch: ConstArray[Node]) = copy(ch(0), ch(1), ch(2))
  protected[this] def rebuildWithSymbols(gen: ConstArray[TermSymbol]) = copy(gen = gen(0))
  protected[this] def withInferredType(scope: Type.Scope, typeChildren: Boolean) = {
    val from2 = from.infer(scope, typeChildren)
    val ifEmpty2 = ifEmpty.infer(scope, typeChildren)
    val genScope = scope + (gen -> from2.nodeType.structural.asOptionType.elementType)
    val map2 = map.infer(genScope, typeChildren)
    withChildren(ConstArray[Node](from2, ifEmpty2, map2)) :@ (if(!hasType) map2.nodeType else nodeType)
  }
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = "")
}

final case class GetOrElse(child: Node, default: () => Any) extends UnaryNode with SimplyTypedNode {
  type Self = GetOrElse
  protected[this] def rebuild(ch: Node) = copy(child = ch)
  protected def buildType = child.nodeType.structural.asOptionType.elementType
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = "")
}

/** A compiled statement with a fixed type, a statement string and profile-specific extra data. */
final case class CompiledStatement(statement: String, extra: Any, buildType: Type) extends NullaryNode with SimplyTypedNode {
  type Self = CompiledStatement
  def rebuild = copy()
  override def getDumpInfo =
    super.getDumpInfo.copy(mainInfo = if(statement contains '\n') statement else ("\"" + statement + "\""))
}

/** A client-side type mapping */
final case class TypeMapping(child: Node, mapper: MappedScalaType.Mapper, classTag: ClassTag[_]) extends UnaryNode with SimplyTypedNode { self =>
  type Self = TypeMapping
  def rebuild(ch: Node) = copy(child = ch)
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = "")
  protected def buildType = new MappedScalaType(child.nodeType, mapper, classTag)
}

/** Rebuild an Option type on the client side */
final case class RebuildOption(discriminator: Node, data: Node) extends BinaryNode with SimplyTypedNode { self =>
  type Self = RebuildOption
  def left = discriminator
  def right = data
  def rebuild(left: Node, right: Node) = copy(left, right)
  protected def buildType = OptionType(data.nodeType)
}

/** A parameter from a QueryTemplate which gets turned into a bind variable. */
final case class QueryParameter(extractor: (Any => Any), buildType: Type, id: TermSymbol = new AnonSymbol) extends NullaryNode with SimplyTypedNode {
  type Self = QueryParameter
  def rebuild = copy()
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = s"$id $extractor")
}

object QueryParameter {
  import TypeUtil._

  /** Create a LiteralNode or QueryParameter that performs a client-side computation
    * on two primitive values. The given Nodes must also be of type `LiteralNode` or
    * `QueryParameter`. */
  def constOp[T](name: String)(op: (T, T) => T)(l: Node, r: Node)(implicit tpe: ScalaBaseType[T]): Node = (l, r) match {
    case (LiteralNode(lv) :@ (lt: TypedType[_]), LiteralNode(rv) :@ (rt: TypedType[_])) if lt.scalaType == tpe && rt.scalaType == tpe => LiteralNode[T](op(lv.asInstanceOf[T], rv.asInstanceOf[T])).infer()
    case (LiteralNode(lv) :@ (lt: TypedType[_]), QueryParameter(re, rt: TypedType[_], _)) if lt.scalaType == tpe && rt.scalaType == tpe =>
      QueryParameter(new (Any => T) {
        def apply(param: Any) = op(lv.asInstanceOf[T], re(param).asInstanceOf[T])
        override def toString = s"($lv $name $re)"
      }, tpe)
    case (QueryParameter(le, lt: TypedType[_], _), LiteralNode(rv) :@ (rt: TypedType[_])) if lt.scalaType == tpe && rt.scalaType == tpe =>
      QueryParameter(new (Any => T) {
        def apply(param: Any) = op(le(param).asInstanceOf[T], rv.asInstanceOf[T])
        override def toString = s"($le $name $rv)"
      }, tpe)
    case (QueryParameter(le, lt: TypedType[_], _), QueryParameter(re, rt: TypedType[_], _)) if lt.scalaType == tpe && rt.scalaType == tpe =>
      QueryParameter(new (Any => T) {
        def apply(param: Any) = op(le(param).asInstanceOf[T], re(param).asInstanceOf[T])
        override def toString = s"($le $name $re)"
      }, tpe)
    case _ => throw new SlickException(s"Cannot fuse nodes $l, $r as constant operations of type $tpe")
  }
}
