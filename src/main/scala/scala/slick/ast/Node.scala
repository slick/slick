package scala.slick.ast

import scala.slick.SlickException
import scala.slick.util.{Logging, SimpleTypeName}
import TypeUtil.typeToTypeUtil
import Util._

/** An object that can produce a Node. */
trait NodeGenerator {
  def nodeDelegate: Node
}

/**
 * A node in the query AST.
 *
 * Every Node has a number of child nodes and an optional type annotation.
 */
trait Node extends NodeGenerator {
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

  /** Apply a mapping function to all children of this node and recreate the
    * node with the new children. If all new children are identical to the old
    * ones, this node is returned. */
  final def nodeMapChildren(f: Node => Node): Self =
    mapOrNone(nodeChildren)(f).map(nodeRebuild).getOrElse(this)

  /** Like nodeMapChildren, except the type of this node is kept even if the
    * children have changed. We don't do this by default in nodeMapChildren
    * because many transformations change the type. */
  final def nodeMapChildrenKeepType(f: Node => Node): Self = {
    val this2 = nodeMapChildren(f)
    if(_nodeType == UnassignedType) this2
    else nodeBuildTypedNode(this2, _nodeType)
  }

  def nodeDelegate: Node = this

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

  def nodeHasType: Boolean = _nodeType != UnassignedType

  /** Return this Node with a Type assigned. This may only be called on
    * freshly constructed nodes with no other existing references, i.e.
    * creating the Node plus assigning it a Type must be atomic. */
  def nodeTyped(tpe: Type): this.type = {
    if(seenType && tpe != _nodeType)
      throw new SlickException("Trying to reassign node type -- nodeTyped() may only be called on freshly constructed nodes")
    _nodeType = tpe
    Node.logType(this)
    this
  }

  /** Return this Node with a Type assigned (if no other type has been seen
    * for it yet) or a typed copy. */
  def nodeTypedOrCopy(tpe: Type): Self = {
    if(seenType && tpe != _nodeType)
      nodeRebuild(nodeChildren.toIndexedSeq).nodeTyped(tpe)
    else nodeTyped(tpe)
  }

  def nodeBuildTypedNode[T >: this.type <: Node](newNode: T, newType: Type): T =
    if(newNode ne this) newNode.nodeTyped(newType)
    else if(newType == nodeType) this
    else nodeRebuildWithType(newType).asInstanceOf[T]

  def nodeRebuildWithType(tpe: Type): Self = nodeRebuild(nodeChildren.toIndexedSeq).nodeTyped(tpe)

  /** Rebuild this node and all children with their computed type. If this
    * node already has a type, it is only recomputed if ``retype`` is set to
    * true. The types of all children are computed recursively (if this node's
    * type is actually computed) using the same ``retype`` setting. */
  def nodeWithComputedType(scope: SymbolScope, retype: Boolean): Self
}

object Node extends Logging {
  def apply(o: Any): Node =
    if(o == null) LiteralNode(null)
    else if(o.isInstanceOf[WithOp] && (o.asInstanceOf[WithOp].op ne null)) Node(o.asInstanceOf[WithOp].op)
    else if(o.isInstanceOf[NodeGenerator]) {
      val gen = o.asInstanceOf[NodeGenerator]
      if(gen.nodeDelegate eq gen) gen.nodeDelegate else Node(gen.nodeDelegate)
    }
    else if(o.isInstanceOf[Product])
      ProductNode(o.asInstanceOf[Product].productIterator.map(apply).toSeq)
    else throw new SlickException("Cannot narrow "+o+" of type "+SimpleTypeName.forVal(o)+" to a Node")

  private def logType(n: Node): Unit =
    logger.debug("Assigned type "+n.nodeType+" to node "+n)
}

trait TypedNode extends Node with Typed {
  override def nodeType: Type = {
    val t = super.nodeType
    if(t eq UnassignedType) tpe else t
  }
  def nodeWithComputedType(scope: SymbolScope, retype: Boolean): Self =
    if(nodeHasType && !retype) this
    else nodeMapChildren(_.nodeWithComputedType(scope, retype))
  override def nodeHasType = tpe ne UnassignedType
}

/** An expression that represents a conjunction of expressions. */
trait ProductNode extends Node { self =>
  type Self = ProductNode
  override def toString = "ProductNode"
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Self = new ProductNode {
    val nodeChildren = ch
  }
  override def nodeChildNames: Iterable[String] = Stream.from(1).map(_.toString)
  override def hashCode() = nodeChildren.hashCode()
  override def equals(o: Any) = o match {
    case p: ProductNode => nodeChildren == p.nodeChildren
    case _ => false
  }
  def nodeWithComputedType(scope: SymbolScope, retype: Boolean): Self = if(nodeHasType && !retype) this else {
    val this2 = nodeMapChildren(_.nodeWithComputedType(scope, retype))
    nodeBuildTypedNode(this2, this2.buildType)
  }
  def withComputedTypeNoRec: ProductNode = nodeBuildTypedNode(this, buildType)
  protected def buildType: Type = ProductType(nodeChildren.map { ch =>
    val t = ch.nodeType
    if(t == UnassignedType) throw new SlickException(s"ProductNode child $ch has UnassignedType")
    t
  }(collection.breakOut))
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
  override def nodePostGeneratorChildren = Seq.empty // for efficiency
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
    def nodeRebuild = apply(tp, v)
    def volatileHint = vol
    override def toString = s"LiteralNode $value (volatileHint=$volatileHint)"
  }
  def apply[T](v: T)(implicit tp: StaticType[T]): LiteralNode = apply(tp, v)
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
final case class Pure(value: Node) extends UnaryNode {
  type Self = Pure
  def child = value
  override def nodeChildNames = Seq("value")
  protected[this] def nodeRebuild(child: Node) = copy(value = child)
  def nodeWithComputedType(scope: SymbolScope, retype: Boolean): Self = if(nodeHasType && !retype) this else {
    val this2 = nodeMapChildren(_.nodeWithComputedType(scope, retype))
    nodeBuildTypedNode(this2, this2.buildType)
  }
  def withComputedTypeNoRec: Self = nodeBuildTypedNode(this, buildType)
  private def buildType: Type = CollectionType(CollectionTypeConstructor.default, value.nodeType)
}

/** Common superclass for expressions of type
  * (CollectionType(c, t), _) => CollectionType(c, t). */
abstract class FilteredQuery extends DefNode {
  def generator: Symbol
  def from: Node
  def nodeGenerators = Seq((generator, from))
  def nodeMapFrom(f: Node => Node) = {
    val fr = from
    nodeMapChildren(n => if(n eq fr) f(n) else n)
  }
  def nodeMapOthers(f: Node => Node) = {
    val fr = from
    nodeMapChildren(n => if(n ne fr) f(n) else n)
  }
  override def toString = this match {
    case p: Product =>
      val n = getClass.getName.replaceFirst(".*\\.", "").replaceFirst(".*\\$", "")
      val args = p.productIterator.filterNot(n => n.isInstanceOf[Node] || n.isInstanceOf[Symbol]).mkString(", ")
      if(args.isEmpty) n else (n + ' ' + args)
    case _ => super.toString
  }
  def nodeWithComputedType(scope: SymbolScope, retype: Boolean): Self = if(nodeHasType && !retype) this else {
    val fr = from
    val fr2 = fr.nodeWithComputedType(scope, retype)
    val genScope = scope + (generator -> fr2.nodeType.asCollectionType.elementType)
    val n2 = nodeMapChildren { ch =>
      if(ch eq fr) fr2 else ch.nodeWithComputedType(genScope, retype)
    }
    nodeBuildTypedNode(n2, n2.nodeChildren.head.nodeType)
  }
}

object FilteredQuery {
  def unapply(f: FilteredQuery) = Some((f.generator, f.from))
}

/** A .filter call of type
  * (CollectionType(c, t), Boolean) => CollectionType(c, t). */
final case class Filter(generator: Symbol, from: Node, where: Node) extends FilteredQuery with BinaryNode {
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
final case class SortBy(generator: Symbol, from: Node, by: Seq[(Node, Ordering)]) extends FilteredQuery {
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
final case class GroupBy(fromGen: Symbol, byGen: Symbol, from: Node, by: Node) extends BinaryNode with DefNode {
  type Self = GroupBy
  def left = from
  def right = by
  override def nodeChildNames = Seq("from "+fromGen, "by "+byGen)
  protected[this] def nodeRebuild(left: Node, right: Node) = copy(from = left, by = right)
  protected[this] def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(fromGen = gen(0), byGen = gen(1))
  def nodeGenerators = Seq((fromGen, from), (byGen, by))
  override def toString = "GroupBy"
  def nodeWithComputedType(scope: SymbolScope, retype: Boolean): Self = if(nodeHasType && !retype) this else {
    val fr = from
    val fr2 = fr.nodeWithComputedType(scope, retype)
    val fromType = fr2.nodeType.asCollectionType
    val b = by
    val b2 = b.nodeWithComputedType(scope + (fromGen -> fromType.elementType), retype)
    val newType = CollectionType(fromType.cons, ProductType(IndexedSeq(b2.nodeType, CollectionType(CollectionTypeConstructor.default, fromType.elementType))))
    if((fr eq fr2) && (b eq b2) && newType == nodeType) this
    else copy(from = fr2, by = b2).nodeTyped(newType)
  }
}

/** A .take call. */
final case class Take(from: Node, num: Int, generator: Symbol = new AnonSymbol) extends FilteredQuery with UnaryNode {
  type Self = Take
  def child = from
  override def nodeChildNames = Seq("from "+generator)
  protected[this] def nodeRebuild(child: Node) = copy(from = child)
  protected[this] def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(generator = gen(0))
}

/** A .drop call. */
final case class Drop(from: Node, num: Int, generator: Symbol = new AnonSymbol) extends FilteredQuery with UnaryNode {
  type Self = Drop
  def child = from
  override def nodeChildNames = Seq("from "+generator)
  protected[this] def nodeRebuild(child: Node) = copy(from = child)
  protected[this] def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(generator = gen(0))
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
  def nodeWithComputedType(scope: SymbolScope, retype: Boolean): Self = if(nodeHasType && !retype) this else {
    val l2 = left.nodeWithComputedType(scope, retype)
    val r2 = right.nodeWithComputedType(scope, retype)
    val lt = l2.nodeType.asCollectionType
    val rt = r2.nodeType.asCollectionType
    val o2 = on.nodeWithComputedType(scope + (leftGen -> lt.elementType) + (rightGen -> rt.elementType), retype)
    val tpe = CollectionType(lt.cons, ProductType(IndexedSeq(lt.elementType, rt.elementType)))
    if((l2 eq left) && (r2 eq right) && (o2 eq on) && tpe == nodeType) this
    else copy(left = l2, right = r2, on = o2).nodeTyped(tpe)
  }
}

/** A union of type
  * (CollectionType(c, t), CollectionType(_, t)) => CollectionType(c, t). */
final case class Union(left: Node, right: Node, all: Boolean, leftGen: Symbol = new AnonSymbol, rightGen: Symbol = new AnonSymbol) extends BinaryNode with DefNode {
  type Self = Union
  protected[this] def nodeRebuild(left: Node, right: Node) = copy(left = left, right = right)
  override def toString = if(all) "Union all" else "Union"
  override def nodeChildNames = Seq("left "+leftGen, "right "+rightGen)
  def nodeGenerators = Seq((leftGen, left), (rightGen, right))
  protected[this] def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) =
    copy(leftGen = gen(0), rightGen = gen(1))
  def nodeWithComputedType(scope: SymbolScope, retype: Boolean): Self = if(nodeHasType && !retype) this else {
    val l2 = left.nodeWithComputedType(scope, retype)
    val r2 = right.nodeWithComputedType(scope, retype)
    if((l2 eq left) && (r2 eq right) && r2.nodeType == nodeType) this
    else copy(left = l2, right = r2).nodeTyped(r2.nodeType)
  }
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
  def nodeWithComputedType(scope: SymbolScope, retype: Boolean): Self = if(nodeHasType && !retype) this else {
    val f2 = from.nodeWithComputedType(scope, retype)
    val fromType = f2.nodeType.asCollectionType
    val s2 = select.nodeWithComputedType(scope + (generator -> fromType.elementType), retype)
    val newType = CollectionType(fromType.cons, s2.nodeType.asCollectionType.elementType)
    if((f2 eq from) && (s2 eq select) && newType == nodeType) this
    else copy(from = f2, select = s2).nodeTyped(newType)
  }
}

/** A table expansion. In phase expandTables, all tables are replaced by
  * TableExpansions to capture the dual nature of tables as as single entity
  * and a structure of columns. TableExpansions are removed again in phase
  * rewritePaths. */
final case class TableExpansion(generator: Symbol, table: Node, columns: Node) extends BinaryNode with DefNode {
  type Self = TableExpansion
  def left = table
  def right = columns
  override def nodeChildNames = Seq("table "+generator, "columns")
  protected[this] def nodeRebuild(left: Node, right: Node) = copy(table = left, columns = right)
  def nodeGenerators = Seq((generator, table))
  override def toString = "TableExpansion"
  protected[this] def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(generator = gen(0))
  def nodeWithComputedType(scope: SymbolScope, retype: Boolean): Self = if(nodeHasType && !retype) this else {
    val t2 = table.nodeWithComputedType(scope, retype)
    val c2 = columns.nodeWithComputedType(scope + (generator -> t2.nodeType), retype)
    val newType = CollectionType(t2.nodeType.asCollectionType.cons, c2.nodeType)
    if((t2 eq table) && (c2 eq columns) && newType == nodeType) this
    else copy(table = t2, columns = c2).nodeTyped(newType)
  }
}

/** Similar to a TableExpansion but used to replace a Ref pointing to a
  * Table(Expansion) (or another TableRefExpansion) instead of a plain Table. */
final case class TableRefExpansion(marker: Symbol, ref: Node, columns: Node) extends BinaryNode with DefNode {
  type Self = TableRefExpansion
  def left = ref
  def right = columns
  override def nodeChildNames = Seq("ref", "columns")
  protected[this] def nodeRebuild(left: Node, right: Node) = copy(ref = left, columns = right)
  def nodeGenerators = Seq((marker, ref))
  override def toString = "TableRefExpansion "+marker
  protected[this] def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(marker = gen(0))
  def nodeWithComputedType(scope: SymbolScope, retype: Boolean): Self = if(nodeHasType && !retype) this else {
    val r2 = ref.nodeWithComputedType(scope, retype)
    val c2 = columns.nodeWithComputedType(scope + (marker -> r2.nodeType), retype)
    if((r2 eq ref) && (c2 eq columns) && c2.nodeType == nodeType) this
    else copy(ref = r2, columns = c2).nodeTyped(c2.nodeType)
  }
}

/** An expression that selects a field in another expression. */
final case class Select(in: Node, field: Symbol) extends UnaryNode with RefNode {
  if(in.isInstanceOf[TableNode])
    throw new SlickException("Select(TableNode, \""+field+"\") found. This is "+
      "typically caused by an attempt to use a \"raw\" table object directly "+
      "in a query without introducing it through a generator.")
  type Self = Select
  def child = in
  override def nodeChildNames = Seq("in")
  protected[this] def nodeRebuild(child: Node) = copy(in = child).nodeTyped(nodeType)
  def nodeReference = field
  protected[this] def nodeRebuildWithReference(s: Symbol) = copy(field = s)
  override def toString = Path.unapply(this) match {
    case Some(l) => Path.toString(l)
    case None => super.toString
  }
  def nodeWithComputedType(scope: SymbolScope, retype: Boolean): Self = if(nodeHasType && !retype) this else {
    val i2 = in.nodeWithComputedType(scope, retype)
    val tpe = Type.select(i2.nodeType, field)
    if((i2 eq in) && tpe == nodeType) this
    else copy(in = i2).nodeTyped(tpe)
  }
}

/** A function call expression. */
final case class Apply(sym: Symbol, children: Seq[Node])(val tpe: Type) extends RefNode with TypedNode {
  type Self = Apply
  def nodeChildren = children
  protected[this] def nodeRebuild(ch: IndexedSeq[scala.slick.ast.Node]) = copy(children = ch)(tpe)
  def nodeReference = sym
  protected[this] def nodeRebuildWithReference(s: Symbol) = copy(sym = s)(tpe)
  override def toString = "Apply "+sym
}

/** A reference to a Symbol */
final case class Ref(sym: Symbol) extends NullaryNode with RefNode {
  type Self = Ref
  def nodeReference = sym
  protected[this] def nodeRebuildWithReference(s: Symbol) = copy(sym = s)
  def nodeWithComputedType(scope: SymbolScope, retype: Boolean): Self = if(nodeHasType && !retype) this else {
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

/** Base class for table nodes. Direct and lifted embedding have different
  * implementations of this class. */
abstract class TableNode extends NullaryNode { self =>
  type Self = TableNode
  def nodeTableProjection: Node
  def schemaName: Option[String]
  def tableName: String
  def nodeTableSymbol: TableSymbol = TableSymbol(tableName)
  def nodeWithComputedType(scope: SymbolScope, retype: Boolean): Self = this
  override def toString = "Table " + tableName
  def nodeRebuild: TableNode = new TableNode {
    def nodeTableProjection = self.nodeTableProjection
    def schemaName = self.schemaName
    def tableName = self.tableName
  }
}

object TableNode {
  def unapply(t: TableNode) = Some(t.tableName)
}

/** A dynamically scoped Let expression where the resulting expression and
  * all definitions may refer to other definitions independent of the order
  * in which they appear in the Let. Circular dependencies are not allowed. */
final case class LetDynamic(defs: Seq[(Symbol, Node)], in: Node) extends DefNode {
  type Self = LetDynamic
  val nodeChildren = defs.map(_._2) :+ in
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]) =
    copy(defs = defs.zip(ch.init).map{ case ((s, _), n) => (s, n) }, in = ch.last)
  override def nodeChildNames = defs.map("let " + _._1.toString) :+ "in"
  def nodeGenerators = defs
  protected[this] def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]): Node =
    copy(defs = (defs, gen).zipped.map((e, s) => (s, e._2)))
  override def toString = "LetDynamic"
  def nodeWithComputedType(scope: SymbolScope, retype: Boolean): Self = if(nodeHasType && !retype) this else {
    var defsMap = defs.toMap.mapValues(n => (n, false))
    var updated = false
    lazy val dynScope: SymbolScope = scope.withDefault { sym =>
      defsMap.get(sym) match {
        case Some((n, false)) =>
          val n2 = n.nodeWithComputedType(dynScope, retype)
          if(n2 ne n) updated = true
          defsMap += (sym -> (n2, true))
          n2.nodeType
        case Some((n, true)) => n.nodeType
        case None => throw new SlickException("Symbol "+sym+" not found")
      }
    }
    val i2 = in.nodeWithComputedType(dynScope, retype)
    if((i2 eq in) && !updated && i2.nodeType == nodeType) this
    else {
      val d2 = defs.map { case (s, _) => (s, defsMap(s)._1) }
      copy(defs = d2, in = i2).nodeTyped(i2.nodeType)
    }
  }
}

/** A node that represents an SQL sequence. */
final case class SequenceNode(name: String)(val increment: Long) extends NullaryNode with TypedNode {
  type Self = SequenceNode
  def tpe = StaticType.Long
  def nodeRebuild = copy()(increment)
}

/** A Query of this special Node represents an infinite stream of consecutive
  * numbers starting at the given number. This is used as an operand for
  * zipWithIndex. It is not exposed directly in the query language because it
  * cannot be represented in SQL outside of a 'zip' operation. */
final case class RangeFrom(start: Long = 1L) extends NullaryNode with TypedNode {
  type Self = RangeFrom
  def tpe = StaticType.Long
  def nodeRebuild = copy()
}

/** An if-then part of a Conditional node */
final case class IfThen(val left: Node, val right: Node) extends BinaryNode {
  type Self = IfThen
  protected[this] def nodeRebuild(left: Node, right: Node): Self = copy(left = left, right = right)
  def nodeWithComputedType(scope: SymbolScope, retype: Boolean): Self = if(nodeHasType && !retype) this else {
    val l2 = left.nodeWithComputedType(scope, retype)
    val r2 = right.nodeWithComputedType(scope, retype)
    if((l2 eq left) && (r2 eq right) && right.nodeType == nodeType) this
    else copy(left = l2, right = r2).nodeTyped(right.nodeType)
  }
}

/** A conditional expression; all clauses should be IfThen nodes */
final case class ConditionalExpr(val clauses: IndexedSeq[Node], val elseClause: Node) extends Node {
  type Self = ConditionalExpr
  val nodeChildren = elseClause +: clauses
  override def nodeChildNames = "else" +: (1 to clauses.length).map(_.toString)
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Self =
    copy(clauses = ch.tail, elseClause = ch.head)
  def nodeWithComputedType(scope: SymbolScope, retype: Boolean): Self = if(nodeHasType && !retype) this else {
    val this2 = nodeMapChildren(_.nodeWithComputedType(scope, retype))
    val tpe = {
      val isNullable = this2.nodeChildren.exists(ch =>
        ch.nodeType.isInstanceOf[OptionType] || ch.nodeType == StaticType.Null)
      val base = this2.clauses.head.nodeType
      if(isNullable && !base.isInstanceOf[OptionType]) OptionType(base) else base
    }
    nodeBuildTypedNode(this2, tpe)
  }
  override def toString = "ConditionalExpr"
}

final case class OptionApply(val child: Node) extends UnaryNode {
  type Self = OptionApply
  protected[this] def nodeRebuild(ch: Node) = copy(child = ch)
  def nodeWithComputedType(scope: SymbolScope, retype: Boolean): Self = if(nodeHasType && !retype) this else {
    val this2 = nodeMapChildren(_.nodeWithComputedType(scope, retype))
    val tp = OptionType(this2.nodeChildren.head.nodeType)
    nodeBuildTypedNode(this2, tp)
  }
}

final case class GetOrElse(val child: Node, val default: () => Any) extends UnaryNode {
  type Self = GetOrElse
  protected[this] def nodeRebuild(ch: Node) = copy(child = ch)
  def nodeWithComputedType(scope: SymbolScope, retype: Boolean): Self = if(nodeHasType && !retype) this else {
    val this2 = nodeMapChildren(_.nodeWithComputedType(scope, retype))
    val tp = this2.nodeChildren.head.nodeType.asOptionType.elementType
    nodeBuildTypedNode(this2, tp)
  }
}

/** A compiled statement with a fixed type, a statement string and
  * driver-specific extra data. */
final case class CompiledStatement(statement: String, extra: Any, tpe: Type) extends NullaryNode with TypedNode {
  type Self = CompiledStatement
  def nodeRebuild = copy()
  override def toString = "CompiledStatement \"" + statement + "\""
}

/** A client-side type mapping */
final case class TypeMapping(val child: Node, val baseType: Type, val toBase: Any => Any, val toMapped: Any => Any) extends UnaryNode with TypedNode { self =>
  type Self = TypeMapping
  def nodeRebuild(ch: Node) = copy(child = ch)
  lazy val tpe: MappedScalaType = new MappedScalaType {
    val baseType =  self.baseType
    def toBase(v: Any): Any = self.toBase(v)
    def toMapped(v: Any): Any = self.toMapped(v)
  }
  override def toString = "TypeMapping"
}

/** A parameter from a QueryTemplate which gets turned into a bind variable. */
final case class QueryParameter(extractor: (Any => Any), val tpe: Type) extends NullaryNode with TypedNode {
  type Self = QueryParameter
  def nodeRebuild = copy()
  override def toString = "QueryParameter"
}
