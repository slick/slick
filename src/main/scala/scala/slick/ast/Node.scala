package scala.slick.ast

import scala.language.existentials
import scala.slick.SlickException
import scala.slick.util.Logging
import TypeUtil.typeToTypeUtil
import Util._
import scala.reflect.ClassTag

/**
 * A node in the query AST.
 *
 * Every Node has a number of child nodes and an optional type annotation.
 */
trait Node {
  type Self >: this.type <: Node
  private[this] var seenType: Boolean = false

  /** All child nodes of this node. Must be implemented by subclasses. */
  def nodeChildren: Seq[Node]

  /** Names for the child nodes to show in AST dumps. Defaults to a numbered
    * sequence starting at 0 but can be overridden by subclasses to produce
    * more suitable names. */
  def nodeChildNames: Iterable[String] = Stream.from(0).map(_.toString)

  /** Rebuild this node with a new list of children. Implementations of this
    * method *must not* perform any optimization to reuse the current node.
    * This method always returns a fresh copy. */
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Self

  /** Rebuild this node with a new list of children unless all children are
    * identical to the current ones. */
  protected[this] final def nodeRebuildOrThis(ch: IndexedSeq[Node]): Self = {
    if((nodeChildren, ch).zipped.forall(_ eq _)) this
    else nodeRebuild(ch)
  }

  /** Apply a mapping function to all children of this node and recreate the
    * node with the new children. If all new children are identical to the old
    * ones, this node is returned. If ``keepType`` is set to true, the type
    * of this node is kept even if the children have changed. */
  final def nodeMapChildren(f: Node => Node, keepType: Boolean = false): Self = {
    val this2: Self = mapOrNone(nodeChildren)(f).map(nodeRebuild).getOrElse(this)
    if(_nodeType == UnassignedType || !keepType) this2
    else nodeBuildTypedNode(this2, _nodeType)
  }

  override def toString = this match {
    case p: Product =>
      val cln = getClass.getName.replaceFirst(".*\\.", "")
      val n = if(cln.endsWith("$")) cln.substring(0, cln.length-1) else cln.replaceFirst(".*\\$", "")
      val args = p.productIterator.filterNot(_.isInstanceOf[Node]).mkString(", ")
      if(args.isEmpty) n else (n + ' ' + args)
    case _ => super.toString
  }

  /** The intrinsic symbol that points to this Node object. */
  final def nodeIntrinsicSymbol = new IntrinsicSymbol(this)

  private var _nodeType: Type = UnassignedType

  /** The current type of this node */
  def nodeType: Type = {
    seenType = true
    _nodeType
  }

  def nodePeekType: Type = _nodeType

  def nodeHasType: Boolean = _nodeType != UnassignedType

  /** Return this Node with a Type assigned. This may only be called on
    * freshly constructed nodes with no other existing references, i.e.
    * creating the Node plus assigning it a Type must be atomic. */
  final def nodeTyped(tpe: Type): this.type = {
    if(seenType && tpe != _nodeType)
      throw new SlickException("Trying to reassign node type -- nodeTyped() may only be called on freshly constructed nodes")
    _nodeType = tpe
    Node.logType(this)
    this
  }

  /** Return this Node with no Type assigned (if it's type has not been
    * observed yet) or an untyped copy. */
  final def nodeUntypedOrCopy: Self = {
    if(seenType || _nodeType != UnassignedType) nodeRebuild(nodeChildren.toIndexedSeq)
    else this
  }

  /** Return this Node with a Type assigned (if no other type has been seen
    * for it yet) or a typed copy. */
  final def nodeTypedOrCopy(tpe: Type): Self = {
    if(seenType && tpe != _nodeType)
      nodeRebuild(nodeChildren.toIndexedSeq).nodeTyped(tpe)
    else nodeTyped(tpe)
  }

  def nodeBuildTypedNode[T >: this.type <: Node](newNode: T, newType: Type): T =
    if(newNode ne this) newNode.nodeTyped(newType)
    else if(newType == _nodeType) this
    else nodeRebuildWithType(newType).asInstanceOf[T]

  def nodeRebuildWithType(tpe: Type): Self = nodeRebuild(nodeChildren.toIndexedSeq).nodeTyped(tpe)

  /** Rebuild this node and all children with their computed type. If this
    * node already has a type, the children are only type-checked again if
    * ``typeChildren`` is set to true. if ``retype`` is also set to true, the
    * existing type of this node is replaced. If this node does not yet have
    * a type, the types of all children are computed. */
  final def nodeWithComputedType(scope: SymbolScope = SymbolScope.empty, typeChildren: Boolean = false, retype: Boolean = false): Self =
    if(nodeHasType && !typeChildren) this else nodeWithComputedType2(scope, typeChildren, retype)

  protected[this] def nodeWithComputedType2(scope: SymbolScope = SymbolScope.empty, typeChildren: Boolean = false, retype: Boolean = false): Self
}

/** A Node whose children can be typed independently of each other and which
  * can be typed without access to its scope. */
trait SimplyTypedNode extends Node {
  type Self >: this.type <: SimplyTypedNode

  protected def buildType: Type

  final def nodeWithComputedType2(scope: SymbolScope, typeChildren: Boolean, retype: Boolean): Self = {
    val this2 = nodeMapChildren(_.nodeWithComputedType(scope, typeChildren, retype), !retype)
    if(!nodeHasType || retype) nodeBuildTypedNode(this2, this2.buildType) else this2
  }
}

object Node extends Logging {
  private def logType(n: Node): Unit =
    logger.debug("Assigned type "+n.nodePeekType+" to node "+n)
}

trait TypedNode extends Node with Typed {
  override def nodeType: Type = {
    val t = super.nodeType
    if(t eq UnassignedType) tpe else t
  }
  def nodeWithComputedType2(scope: SymbolScope, typeChildren: Boolean, retype: Boolean): Self =
    nodeMapChildren(_.nodeWithComputedType(scope, typeChildren, retype), !retype)
  override def nodeHasType = (tpe ne UnassignedType) || super.nodeHasType
  override def nodePeekType: Type = super.nodePeekType match {
    case UnassignedType => tpe
    case t => t
  }
}

/** An expression that represents a conjunction of expressions. */
trait ProductNode extends SimplyTypedNode { self =>
  type Self >: this.type <: SimplyTypedNode with ProductNode
  override def toString = "ProductNode"
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Self = new ProductNode {
    val nodeChildren = ch
  }.asInstanceOf[Self]
  override def nodeChildNames: Iterable[String] = Stream.from(1).map(_.toString)
  override def hashCode() = nodeChildren.hashCode()
  override def equals(o: Any) = o match {
    case p: ProductNode => nodeChildren == p.nodeChildren
    case _ => false
  }
  def withComputedTypeNoRec: ProductNode = nodeBuildTypedNode(this, buildType)
  protected def buildType: Type = ProductType(nodeChildren.map { ch =>
    val t = ch.nodeType
    if(t == UnassignedType) throw new SlickException(s"ProductNode child $ch has UnassignedType")
    t
  }(collection.breakOut))
  def numberedElements: Iterator[(ElementSymbol, Node)] =
    nodeChildren.iterator.zipWithIndex.map { case (n, i) => (new ElementSymbol(i+1), n) }
  def flatten: ProductNode = {
    def f(n: Node): IndexedSeq[Node] = n match {
      case ProductNode(ns) => ns.flatMap(f).toIndexedSeq
      case n => IndexedSeq(n)
    }
    ProductNode(f(this))
  }

}

object ProductNode {
  def apply(s: Seq[Node]): ProductNode = new ProductNode {
    val nodeChildren = s
  }
  def unapply(p: ProductNode) = Some(p.nodeChildren)
}

/** An expression that represents a structure, i.e. a conjunction where the
  * individual components have Symbols associated with them. */
final case class StructNode(elements: IndexedSeq[(Symbol, Node)]) extends ProductNode with DefNode {
  type Self = StructNode
  override def toString = "StructNode"
  override def nodeChildNames = elements.map(_._1.toString)
  val nodeChildren = elements.map(_._2)
  override protected[this] def nodeRebuild(ch: IndexedSeq[Node]) =
    new StructNode(elements.zip(ch).map{ case ((s,_),n) => (s,n) })
  override def hashCode() = elements.hashCode()
  override def equals(o: Any) = o match {
    case s: StructNode => elements == s.elements
    case _ => false
  }
  def nodeGenerators = elements
  protected[this] def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]): Node =
    copy(elements = (elements, gen).zipped.map((e, s) => (s, e._2)))
  override def withComputedTypeNoRec: StructNode = nodeBuildTypedNode(this, buildType)
  override protected def buildType: Type = StructType(elements.map { case (s, n) =>
    val t = n.nodeType
    if(t == UnassignedType) throw new SlickException(s"StructNode child $s has UnassignedType")
    (s, t)
  })
}

/** A literal value expression. */
trait LiteralNode extends NullaryNode with TypedNode {
  type Self = LiteralNode
  def value: Any

  /** Indicates whether this value should be considered volatile, i.e. it
    * contains user-generated data or may change in future executions of what
    * is otherwise the same query. A database back-end should usually turn
    * volatile constants into bind variables. */
  def volatileHint: Boolean
}

object LiteralNode {
  def apply(tp: Type, v: Any, vol: Boolean = false): LiteralNode = new LiteralNode {
    val value = v
    val tpe = tp
    def nodeRebuild = apply(tp, v, vol)
    def volatileHint = vol
    override def toString = s"LiteralNode $value (volatileHint=$volatileHint)"
  }
  def apply[T](v: T)(implicit tp: ScalaBaseType[T]): LiteralNode = apply(tp, v)
  def unapply(n: LiteralNode): Option[Any] = Some(n.value)
}

trait BinaryNode extends Node {
  def left: Node
  def right: Node
  lazy val nodeChildren = Seq(left, right)
  protected[this] final def nodeRebuild(ch: IndexedSeq[Node]): Self = nodeRebuild(ch(0), ch(1))
  protected[this] def nodeRebuild(left: Node, right: Node): Self
}

trait UnaryNode extends Node {
  def child: Node
  lazy val nodeChildren = Seq(child)
  protected[this] final def nodeRebuild(ch: IndexedSeq[Node]): Self = nodeRebuild(ch(0))
  protected[this] def nodeRebuild(child: Node): Self
}

trait NullaryNode extends Node {
  val nodeChildren = Nil
  protected[this] final def nodeRebuild(ch: IndexedSeq[Node]): Self = nodeRebuild
  protected[this] def nodeRebuild: Self
}

/** An expression that represents a plain value lifted into a Query. */
final case class Pure(value: Node, identity: TypeSymbol = new AnonTypeSymbol) extends UnaryNode with SimplyTypedNode {
  type Self = Pure
  def child = value
  override def nodeChildNames = Seq("value")
  protected[this] def nodeRebuild(child: Node) = copy(child)
  def withComputedTypeNoRec: Self = nodeBuildTypedNode(this, buildType)
  protected def buildType =
    CollectionType(TypedCollectionTypeConstructor.seq,
      NominalType(identity)(value.nodeType))
}

final case class CollectionCast(child: Node, cons: CollectionTypeConstructor) extends UnaryNode with SimplyTypedNode {
  type Self = CollectionCast
  protected[this] def nodeRebuild(child: Node) = copy(child = child)
  protected def buildType =
    CollectionType(cons, child.nodeType.asCollectionType.elementType)
}

/** Common superclass for expressions of type
  * (CollectionType(c, t), _) => CollectionType(c, t). */
abstract class FilteredQuery extends Node {
  protected[this] def generator: Symbol
  def from: Node
  def nodeGenerators = Seq((generator, from))
  override def toString = this match {
    case p: Product =>
      val n = getClass.getName.replaceFirst(".*\\.", "").replaceFirst(".*\\$", "")
      val args = p.productIterator.filterNot(n => n.isInstanceOf[Node] || n.isInstanceOf[Symbol]).mkString(", ")
      if(args.isEmpty) n else (n + ' ' + args)
    case _ => super.toString
  }
  def nodeWithComputedType2(scope: SymbolScope, typeChildren: Boolean, retype: Boolean): Self = {
    val from2 = from.nodeWithComputedType(scope, typeChildren, retype)
    val genScope = scope + (generator -> from2.nodeType.asCollectionType.elementType)
    val ch2: IndexedSeq[Node] = nodeChildren.map { ch =>
      if(ch eq from) from2 else ch.nodeWithComputedType(genScope, typeChildren, retype)
    }(collection.breakOut)
    nodeRebuildOrThis(ch2).nodeTypedOrCopy(if(!nodeHasType || retype) ch2.head.nodeType else nodeType).asInstanceOf[Self]
  }
}

/** A .filter call of type
  * (CollectionType(c, t), Boolean) => CollectionType(c, t). */
final case class Filter(generator: Symbol, from: Node, where: Node) extends FilteredQuery with BinaryNode with DefNode {
  type Self = Filter
  def left = from
  def right = where
  override def nodeChildNames = Seq("from "+generator, "where")
  protected[this] def nodeRebuild(left: Node, right: Node) = copy(from = left, where = right)
  protected[this] def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(generator = gen(0))
}

object Filter {
  def ifRefutable(generator: Symbol, from: Node, where: Node): Node =
    if(where match { case LiteralNode(true) => true; case _ => false }) from
    else Filter(generator, from, where)
}

/** A .sortBy call of type
  * (CollectionType(c, t), _) => CollectionType(c, t). */
final case class SortBy(generator: Symbol, from: Node, by: Seq[(Node, Ordering)]) extends FilteredQuery with DefNode {
  type Self = SortBy
  lazy val nodeChildren = from +: by.map(_._1)
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]) =
    copy(from = ch(0), by = by.zip(ch.tail).map{ case ((_, o), n) => (n, o) })
  override def nodeChildNames = ("from "+generator) +: by.zipWithIndex.map("by" + _._2)
  protected[this] def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(generator = gen(0))
  override def toString = "SortBy " + by.map(_._2).mkString(", ")
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
final case class GroupBy(fromGen: Symbol, from: Node, by: Node) extends BinaryNode with DefNode {
  type Self = GroupBy
  def left = from
  def right = by
  override def nodeChildNames = Seq("from "+fromGen, "by")
  protected[this] def nodeRebuild(left: Node, right: Node) = copy(from = left, by = right)
  protected[this] def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(fromGen = gen(0))
  def nodeGenerators = Seq((fromGen, from))
  override def toString = "GroupBy"
  def nodeWithComputedType2(scope: SymbolScope, typeChildren: Boolean, retype: Boolean): Self = {
    val from2 = from.nodeWithComputedType(scope, typeChildren, retype)
    val from2Type = from2.nodeType.asCollectionType
    val by2 = by.nodeWithComputedType(scope + (fromGen -> from2Type.elementType), typeChildren, retype)
    nodeRebuildOrThis(Vector(from2, by2)).nodeTypedOrCopy(
      if(!nodeHasType || retype)
        CollectionType(from2Type.cons, ProductType(IndexedSeq(by2.nodeType.structural, CollectionType(TypedCollectionTypeConstructor.seq, from2Type.elementType))))
      else nodeType)
  }
}

/** A .take call. */
final case class Take(from: Node, num: Int) extends FilteredQuery with UnaryNode {
  type Self = Take
  def child = from
  protected[this] val generator = new AnonSymbol
  override def nodeChildNames = Seq("from")
  protected[this] def nodeRebuild(child: Node) = copy(from = child)
}

/** A .drop call. */
final case class Drop(from: Node, num: Int) extends FilteredQuery with UnaryNode {
  type Self = Drop
  def child = from
  protected[this] val generator = new AnonSymbol
  override def nodeChildNames = Seq("from")
  protected[this] def nodeRebuild(child: Node) = copy(from = child)
}

/** A join expression of type
  * (CollectionType(c, t), CollectionType(_, u)) => CollecionType(c, (t, u)). */
final case class Join(leftGen: Symbol, rightGen: Symbol, left: Node, right: Node, jt: JoinType, on: Node) extends DefNode {
  type Self = Join
  lazy val nodeChildren = IndexedSeq(left, right, on)
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]) = copy(left = ch(0), right = ch(1), on = ch(2))
  override def nodeChildNames = Seq("left "+leftGen, "right "+rightGen, "on")
  override def toString = "Join " + jt.sqlName
  def nodeGenerators = Seq((leftGen, left), (rightGen, right))
  protected[this] def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) =
    copy(leftGen = gen(0), rightGen = gen(1))
  def nodeWithComputedType2(scope: SymbolScope, typeChildren: Boolean, retype: Boolean): Self = {
    val left2 = left.nodeWithComputedType(scope, typeChildren, retype)
    val right2 = right.nodeWithComputedType(scope, typeChildren, retype)
    val left2Type = left2.nodeType.asCollectionType
    val right2Type = right2.nodeType.asCollectionType
    val on2 = on.nodeWithComputedType(scope + (leftGen -> left2Type.elementType) + (rightGen -> right2Type.elementType), typeChildren, retype)
    nodeRebuildOrThis(Vector(left2, right2, on2)).nodeTypedOrCopy(
      if(!nodeHasType || retype)
        CollectionType(left2Type.cons, ProductType(IndexedSeq(left2Type.elementType, right2Type.elementType)))
      else nodeType)
  }
}

/** A union of type
  * (CollectionType(c, t), CollectionType(_, t)) => CollectionType(c, t). */
final case class Union(left: Node, right: Node, all: Boolean, leftGen: Symbol = new AnonSymbol, rightGen: Symbol = new AnonSymbol) extends BinaryNode with DefNode with SimplyTypedNode {
  type Self = Union
  protected[this] def nodeRebuild(left: Node, right: Node) = copy(left = left, right = right)
  override def toString = if(all) "Union all" else "Union"
  override def nodeChildNames = Seq("left "+leftGen, "right "+rightGen)
  def nodeGenerators = Seq((leftGen, left), (rightGen, right))
  protected[this] def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(leftGen = gen(0), rightGen = gen(1))
  protected def buildType = left.nodeType
}

/** A .flatMap call of type
  * (CollectionType(c, _), CollectionType(_, u)) => CollectionType(c, u). */
final case class Bind(generator: Symbol, from: Node, select: Node) extends BinaryNode with DefNode {
  type Self = Bind
  def left = from
  def right = select
  override def nodeChildNames = Seq("from "+generator, "select")
  protected[this] def nodeRebuild(left: Node, right: Node) = copy(from = left, select = right)
  def nodeGenerators = Seq((generator, from))
  override def toString = "Bind"
  protected[this] def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(generator = gen(0))
  def nodeWithComputedType2(scope: SymbolScope, typeChildren: Boolean, retype: Boolean): Self = {
    val from2 = from.nodeWithComputedType(scope, typeChildren, retype)
    val from2Type = from2.nodeType.asCollectionType
    val select2 = select.nodeWithComputedType(scope + (generator -> from2Type.elementType), typeChildren, retype)
    nodeRebuildOrThis(Vector(from2, select2)).nodeTypedOrCopy(
      if(!nodeHasType || retype)
        CollectionType(from2Type.cons, select2.nodeType.asCollectionType.elementType)
      else nodeType)
  }
}

/** A table together with its expansion into columns. */
final case class TableExpansion(generator: Symbol, table: Node, columns: Node) extends BinaryNode with DefNode {
  type Self = TableExpansion
  def left = table
  def right = columns
  override def nodeChildNames = Seq("table "+generator, "columns")
  protected[this] def nodeRebuild(left: Node, right: Node) = copy(table = left, columns = right)
  def nodeGenerators = Seq((generator, table))
  override def toString = "TableExpansion"
  protected[this] def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(generator = gen(0))
  def nodeWithComputedType2(scope: SymbolScope, typeChildren: Boolean, retype: Boolean): Self = {
    val table2 = table.nodeWithComputedType(scope, typeChildren, retype)
    val columns2 = columns.nodeWithComputedType(scope + (generator -> table2.nodeType.asCollectionType.elementType), typeChildren, retype)
    nodeRebuildOrThis(Vector(table2, columns2)).nodeTypedOrCopy(if(!nodeHasType || retype) table2.nodeType else nodeType)
  }
}

/** An expression that selects a field in another expression. */
final case class Select(in: Node, field: Symbol) extends UnaryNode with SimplyTypedNode {
  type Self = Select
  def child = in
  override def nodeChildNames = Seq("in")
  protected[this] def nodeRebuild(child: Node) = copy(in = child).nodeTyped(nodeType)
  override def toString = Path.unapply(this) match {
    case Some(l) => Path.toString(l)
    case None => super.toString
  }
  protected def buildType = in.nodeType.select(field)
}

/** A function call expression. */
final case class Apply(sym: Symbol, children: Seq[Node])(val tpe: Type) extends TypedNode {
  type Self = Apply
  def nodeChildren = children
  protected[this] def nodeRebuild(ch: IndexedSeq[scala.slick.ast.Node]) = copy(children = ch)(tpe)
  override def toString = "Apply "+sym
}

/** A reference to a Symbol */
final case class Ref(sym: Symbol) extends NullaryNode {
  type Self = Ref
  def nodeWithComputedType2(scope: SymbolScope, typeChildren: Boolean, retype: Boolean): Self =
    if(nodeHasType && !retype) this else {
      scope.get(sym) match {
        case Some(t) => if(t == nodeType) this else copy().nodeTyped(t)
        case _ => throw new SlickException("No type for symbol "+sym+" found for "+this)
      }
    }
  def nodeRebuild = copy()
}

/** A constructor/extractor for nested Selects starting at a Ref. */
object Path {
  def apply(l: List[Symbol]): Node = l match {
    case s :: Nil => Ref(s)
    case s :: l => Select(apply(l), s)
  }
  def unapply(n: Node): Option[List[Symbol]] = n match {
    case Ref(sym) => Some(List(sym))
    case Select(in, s) => unapply(in).map(l => s :: l)
    case _ => None
  }
  def toString(path: Seq[Symbol]): String = path.reverseIterator.mkString("Path ", ".", "")
  def toString(s: Select): String = s match {
    case Path(syms) => toString(syms)
    case n => n.toString
  }
}

object FwdPath {
  def apply(ch: List[Symbol]) = Path(ch.reverse)
  def unapply(n: Node): Option[List[Symbol]] = Path.unapply(n).map(_.reverse)
  def toString(path: Seq[Symbol]): String = path.mkString("Path ", ".", "")
}

/** A Node representing a database table. */
final case class TableNode(schemaName: Option[String], tableName: String, identity: TableIdentitySymbol, driverTable: Any) extends NullaryNode with TypedNode {
  type Self = TableNode
  def tpe = CollectionType(TypedCollectionTypeConstructor.seq, NominalType(identity)(UnassignedStructuralType(identity)))
  def nodeRebuild = copy()
  override def toString = "Table " + tableName
}

/** A node that represents an SQL sequence. */
final case class SequenceNode(name: String)(val increment: Long) extends NullaryNode with TypedNode {
  type Self = SequenceNode
  def tpe = ScalaBaseType.longType
  def nodeRebuild = copy()(increment)
}

/** A Query of this special Node represents an infinite stream of consecutive
  * numbers starting at the given number. This is used as an operand for
  * zipWithIndex. It is not exposed directly in the query language because it
  * cannot be represented in SQL outside of a 'zip' operation. */
final case class RangeFrom(start: Long = 1L) extends NullaryNode with TypedNode {
  type Self = RangeFrom
  def tpe = CollectionType(TypedCollectionTypeConstructor.seq, ScalaBaseType.longType)
  def nodeRebuild = copy()
}

/** An if-then part of a Conditional node */
final case class IfThen(val left: Node, val right: Node) extends BinaryNode with SimplyTypedNode {
  type Self = IfThen
  protected[this] def nodeRebuild(left: Node, right: Node): Self = copy(left = left, right = right)
  protected def buildType = right.nodeType
}

/** A conditional expression; all clauses should be IfThen nodes */
final case class ConditionalExpr(val clauses: IndexedSeq[Node], val elseClause: Node) extends SimplyTypedNode {
  type Self = ConditionalExpr
  val nodeChildren = elseClause +: clauses
  override def nodeChildNames = "else" +: (1 to clauses.length).map(_.toString)
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Self =
    copy(clauses = ch.tail, elseClause = ch.head)
  protected def buildType = {
    val isNullable = nodeChildren.exists(ch =>
      ch.nodeType.isInstanceOf[OptionType] || ch.nodeType == ScalaBaseType.nullType)
    val base = clauses.head.nodeType
    if(isNullable && !base.isInstanceOf[OptionType]) OptionType(base) else base
  }
  override def toString = "ConditionalExpr"
}

final case class OptionApply(val child: Node) extends UnaryNode with SimplyTypedNode {
  type Self = OptionApply
  protected[this] def nodeRebuild(ch: Node) = copy(child = ch)
  protected def buildType = OptionType(nodeChildren.head.nodeType)
}

final case class GetOrElse(val child: Node, val default: () => Any) extends UnaryNode with SimplyTypedNode {
  type Self = GetOrElse
  protected[this] def nodeRebuild(ch: Node) = copy(child = ch)
  protected def buildType = nodeChildren.head.nodeType.asOptionType.elementType
}

/** A compiled statement with a fixed type, a statement string and
  * driver-specific extra data. */
final case class CompiledStatement(statement: String, extra: Any, tpe: Type) extends NullaryNode with TypedNode {
  type Self = CompiledStatement
  def nodeRebuild = copy()
  override def toString = "CompiledStatement \"" + statement + "\""
}

/** A client-side type mapping */
final case class TypeMapping(val child: Node, val toBase: Any => Any, val toMapped: Any => Any, classTag: ClassTag[_]) extends UnaryNode with SimplyTypedNode { self =>
  type Self = TypeMapping
  def nodeRebuild(ch: Node) = copy(child = ch)
  override def toString = "TypeMapping"
  protected def buildType = new MappedScalaType(child.nodeType, toBase, toMapped, classTag)
}

/** A parameter from a QueryTemplate which gets turned into a bind variable. */
final case class QueryParameter(extractor: (Any => Any), val tpe: Type) extends NullaryNode with TypedNode {
  type Self = QueryParameter
  def nodeRebuild = copy()
  override def toString = "QueryParameter"
}
