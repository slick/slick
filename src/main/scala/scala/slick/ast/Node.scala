package scala.slick.ast

import scala.slick.SlickException
import slick.lifted.{Column, ConstColumn, ShapedValue}
import scala.slick.util.SimpleTypeName
import scala.collection.mutable.ArrayBuffer

trait NodeGenerator {
  def nodeDelegate: Node
}

/**
 * A node in the query AST
 */
trait Node extends NodeGenerator {
  def nodeChildren: Seq[Node]
  def nodeChildNames: Iterable[String] = Stream.from(0).map(_.toString)

  def nodeMapChildren(f: Node => Node): Node

  def nodeDelegate: Node = this

  protected[this] final def nodeMapNodes(s: Iterable[Node], f: Node => Node): Option[IndexedSeq[Node]] = {
    var change = false
    val b = new ArrayBuffer[Node]
    for(n <- s) {
      val nn = f(n)
      b append nn
      if(nn ne n) change = true
    }
    if(change) Some(b) else None
  }

  override def toString = this match {
    case p: Product =>
      val cln = getClass.getName.replaceFirst(".*\\.", "")
      val n = if(cln.endsWith("$")) cln.substring(0, cln.length-1) else cln.replaceFirst(".*\\$", "")
      val args = p.productIterator.filterNot(_.isInstanceOf[Node]).mkString(", ")
      if(args.isEmpty) n else (n + ' ' + args)
    case _ => super.toString
  }

  def nodeIntrinsicSymbol = new IntrinsicSymbol(this)
}

trait SimpleNode extends Node {
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Node

  def nodeMapChildren(f: Node => Node): Node =
    nodeMapNodes(nodeChildren, f).map(nodeRebuild).getOrElse(this)
}

trait TypedNode extends Node with Typed

object Node {
  def apply(o:Any): Node =
    if(o == null) ConstColumn.NULL
    else if(o.isInstanceOf[WithOp] && (o.asInstanceOf[WithOp].op ne null)) Node(o.asInstanceOf[WithOp].op)
    else if(o.isInstanceOf[NodeGenerator]) {
      val gen = o.asInstanceOf[NodeGenerator]
      if(gen.nodeDelegate eq gen) gen.nodeDelegate else Node(gen.nodeDelegate)
    }
    else if(o.isInstanceOf[Product]) ProductNode(o.asInstanceOf[Product].productIterator.toSeq)
    else throw new SlickException("Cannot narrow "+o+" of type "+SimpleTypeName.forVal(o)+" to a Node")
}

trait ProductNode extends SimpleNode {
  override def toString = "ProductNode"
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Node = new ProductNode {
    val nodeChildren = ch
  }
  override def nodeChildNames: Iterable[String] = Stream.from(1).map(_.toString)
  override def hashCode() = nodeChildren.hashCode()
  override def equals(o: Any) = o match {
    case p: ProductNode => nodeChildren == p.nodeChildren
    case _ => false
  }
}

object ProductNode {
  def apply(s: Seq[Any]): ProductNode =
    new ProductNode { lazy val nodeChildren = s.map(Node(_)) }
  def unapply(p: ProductNode) = Some(p.nodeChildren)
}

case class StructNode(elements: IndexedSeq[(Symbol, Node)]) extends ProductNode with SimpleDefNode {
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
  def nodePostGeneratorChildren = Seq.empty
  def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]): Node =
    copy(elements = (elements, gen).zipped.map((e, s) => (s, e._2)))
}

trait LiteralNode extends NullaryNode {
  def value: Any
}

object LiteralNode {
  def apply(v: Any): LiteralNode = new LiteralNode {
    val value = v
  }
  def unapply(n: LiteralNode): Option[Any] = Some(n.value)
}

trait BinaryNode extends SimpleNode {
  def left: Node
  def right: Node
  lazy val nodeChildren = Seq(left, right)
  protected[this] final def nodeRebuild(ch: IndexedSeq[Node]): Node = nodeRebuild(ch(0), ch(1))
  protected[this] def nodeRebuild(left: Node, right: Node): Node
}

trait UnaryNode extends SimpleNode {
  def child: Node
  lazy val nodeChildren = Seq(child)
  protected[this] final def nodeRebuild(ch: IndexedSeq[Node]): Node = nodeRebuild(ch(0))
  protected[this] def nodeRebuild(child: Node): Node
}

trait NullaryNode extends SimpleNode {
  val nodeChildren = Nil
  protected[this] final def nodeRebuild(ch: IndexedSeq[Node]): Node = this
}

final case class Pure(value: Node) extends UnaryNode {
  def child = value
  override def nodeChildNames = Seq("value")
  protected[this] def nodeRebuild(child: Node) = copy(value = child)
}

abstract class FilteredQuery extends Node with DefNode {
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
}

object FilteredQuery {
  def unapply(f: FilteredQuery) = Some((f.generator, f.from))
}

final case class Filter(generator: Symbol, from: Node, where: Node) extends FilteredQuery with BinaryNode with SimpleDefNode {
  def left = from
  def right = where
  override def nodeChildNames = Seq("from "+generator, "where")
  protected[this] def nodeRebuild(left: Node, right: Node) = copy(from = left, where = right)
  override def nodeDelegate =
    if(where match { case LiteralNode(true) => true; case _ => false }) left
    else super.nodeDelegate
  def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(generator = gen(0))
  def nodePostGeneratorChildren = Seq(where)
}

final case class SortBy(generator: Symbol, from: Node, by: Seq[(Node, Ordering)]) extends FilteredQuery with SimpleNode with SimpleDefNode {
  lazy val nodeChildren = from +: by.map(_._1)
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]) =
    copy(from = ch(0), by = by.zip(ch.tail).map{ case ((_, o), n) => (n, o) })
  override def nodeChildNames = ("from "+generator) +: by.zipWithIndex.map("by" + _._2)
  def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(generator = gen(0))
  def nodePostGeneratorChildren = by.map(_._1)
  override def toString = "SortBy " + by.map(_._2).mkString(", ")
}

final case class OrderBy(generator: Symbol, from: Node, by: Seq[(Node, Ordering)]) extends FilteredQuery with SimpleNode with SimpleDefNode {
  lazy val nodeChildren = from +: by.map(_._1)
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]) =
    copy(from = ch(0), by = by.zip(ch.tail).map{ case ((_, o), n) => (n, o) })
  override def nodeChildNames = ("from "+generator) +: by.zipWithIndex.map("by" + _._2)
  def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(generator = gen(0))
  def nodePostGeneratorChildren = by.map(_._1)
}

case class Ordering(direction: Ordering.Direction = Ordering.Asc, nulls: Ordering.NullOrdering = Ordering.NullsDefault) {
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

final case class GroupBy(fromGen: Symbol, byGen: Symbol, from: Node, by: Node) extends BinaryNode with SimpleDefNode {
  def left = from
  def right = by
  override def nodeChildNames = Seq("from "+fromGen, "by "+byGen)
  protected[this] def nodeRebuild(left: Node, right: Node) = copy(from = left, by = right)
  def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(fromGen = gen(0), byGen = gen(1))
  def nodePostGeneratorChildren = Nil
  def nodeGenerators = Seq((fromGen, from), (byGen, by))
  override def toString = "GroupBy"
}

final case class Take(from: Node, num: Int, generator: Symbol = new AnonSymbol) extends FilteredQuery with UnaryNode with SimpleDefNode {
  def child = from
  override def nodeChildNames = Seq("from "+generator)
  protected[this] def nodeRebuild(child: Node) = copy(from = child)
  def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(generator = gen(0))
  def nodePostGeneratorChildren = Seq.empty
}

final case class Drop(from: Node, num: Int, generator: Symbol = new AnonSymbol) extends FilteredQuery with UnaryNode with SimpleDefNode {
  def child = from
  override def nodeChildNames = Seq("from "+generator)
  protected[this] def nodeRebuild(child: Node) = copy(from = child)
  def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(generator = gen(0))
  def nodePostGeneratorChildren = Seq.empty
}

final case class Join(leftGen: Symbol, rightGen: Symbol, left: Node, right: Node, jt: JoinType, on: Node) extends SimpleNode with SimpleDefNode {
  lazy val nodeChildren = IndexedSeq(left, right, on)
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]) = copy(left = ch(0), right = ch(1), on = ch(2))
  override def nodeChildNames = Seq("left "+leftGen, "right "+rightGen, "on")
  override def toString = "Join " + jt.sqlName
  def nodeGenerators = Seq((leftGen, left), (rightGen, right))
  def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(leftGen = gen(0), rightGen = gen(1))
  def nodePostGeneratorChildren = Seq(on)
  def nodeCopyJoin(leftGen: Symbol = leftGen, rightGen: Symbol = rightGen, left: Node = left, right: Node = right, jt: JoinType = jt) = {
    if((leftGen eq this.leftGen) && (rightGen eq this.rightGen) && (left eq this.left) && (right eq this.right) && (jt eq this.jt)) this
    else copy(leftGen = leftGen, rightGen = rightGen, left = left, right = right, jt = jt)
  }
}

final case class Union(left: Node, right: Node, all: Boolean, leftGen: Symbol = new AnonSymbol, rightGen: Symbol = new AnonSymbol) extends BinaryNode with SimpleDefNode {
  protected[this] def nodeRebuild(left: Node, right: Node) = copy(left = left, right = right)
  override def toString = if(all) "Union all" else "Union"
  override def nodeChildNames = Seq("left "+leftGen, "right "+rightGen)
  def nodeGenerators = Seq((leftGen, left), (rightGen, right))
  def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(leftGen = gen(0), rightGen = gen(1))
  def nodePostGeneratorChildren = Seq.empty
}

final case class Bind(generator: Symbol, from: Node, select: Node) extends BinaryNode with SimpleDefNode {
  def left = from
  def right = select
  override def nodeChildNames = Seq("from "+generator, "select")
  protected[this] def nodeRebuild(left: Node, right: Node) = copy(from = left, select = right)
  def nodeGenerators = Seq((generator, from))
  override def toString = "Bind"
  def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(generator = gen(0))
  def nodePostGeneratorChildren = Seq(select)
}

final case class TableExpansion(generator: Symbol, table: Node, columns: Node) extends BinaryNode with SimpleDefNode {
  def left = table
  def right = columns
  override def nodeChildNames = Seq("table "+generator, "columns")
  protected[this] def nodeRebuild(left: Node, right: Node) = copy(table = left, columns = right)
  def nodeGenerators = Seq((generator, table))
  override def toString = "TableExpansion"
  def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(generator = gen(0))
  def nodePostGeneratorChildren = Seq(columns)
}

final case class TableRefExpansion(marker: Symbol, ref: Node, columns: Node) extends BinaryNode with SimpleDefNode {
  def left = ref
  def right = columns
  override def nodeChildNames = Seq("ref", "columns")
  protected[this] def nodeRebuild(left: Node, right: Node) = copy(ref = left, columns = right)
  def nodeGenerators = Seq((marker, ref))
  override def toString = "TableRefExpansion "+marker
  def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(marker = gen(0))
  def nodePostGeneratorChildren = Seq(columns)
}

final case class Select(in: Node, field: Symbol) extends UnaryNode with SimpleRefNode {
  if(in.isInstanceOf[TableNode])
    throw new SlickException("Select(TableNode, \""+field+"\") found. This is "+
      "typically caused by an attempt to use a \"raw\" table object directly "+
      "in a query without introducing it through a generator.")
  def child = in
  override def nodeChildNames = Seq("in")
  protected[this] def nodeRebuild(child: Node) = copy(in = child)
  def nodeReferences: Seq[Symbol] = Seq(field)
  def nodeRebuildWithReferences(gen: IndexedSeq[Symbol]) = copy(field = gen(0))
  override def toString = Path.unapply(this) match {
    case Some(l) => super.toString + " for " + Path.toString(l)
    case None => super.toString
  }
}

case class Apply(sym: Symbol, children: Seq[Node]) extends SimpleNode with SimpleRefNode {
  def nodeChildren = children
  protected[this] def nodeRebuild(ch: IndexedSeq[scala.slick.ast.Node]) = copy(children = ch)
  def nodeReferences: Seq[Symbol] = Seq(sym)
  def nodeRebuildWithReferences(syms: IndexedSeq[Symbol]) = copy(sym = syms(0))
  override def toString = "Apply "+sym
}

object Apply {
  /** Create a typed Apply */
  def apply(sym: Symbol, children: Seq[Node], tp: Type): Apply with TypedNode =
    new Apply(sym, children) with TypedNode {
      def tpe = tp
      override protected[this] def nodeRebuild(ch: IndexedSeq[scala.slick.ast.Node]) = Apply(sym, ch, tp)
      override def nodeRebuildWithReferences(syms: IndexedSeq[Symbol]) = Apply(syms(0), children, tp)
    }
}

/** A constructor/extractor for nested Selects starting at a Ref */
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

abstract class TableNode extends Node {
  def nodeShaped_* : ShapedValue[_, _]
  def tableName: String
  def nodeTableSymbol: TableSymbol = TableSymbol(tableName)
  override def toString = "Table " + tableName
}

object TableNode {
  def unapply(t: TableNode) = Some(t.tableName)
}

final case class LetDynamic(defs: Seq[(Symbol, Node)], in: Node) extends SimpleNode with SimpleDefNode {
  val nodeChildren = defs.map(_._2) :+ in
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]) =
    copy(defs = defs.zip(ch.init).map{ case ((s, _), n) => (s, n) }, in = ch.last)
  override def nodeChildNames = defs.map("let " + _._1.toString) :+ "in"
  def nodeGenerators = defs
  def nodePostGeneratorChildren = Seq(in)
  def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]): Node =
    copy(defs = (defs, gen).zipped.map((e, s) => (s, e._2)))
  override def toString = "LetDynamic"
}

final case class SequenceNode(name: String)(val increment: Long) extends NullaryNode

/** A Query of this special Node represents an infinite stream of consecutive
  * numbers starting at the given number. This is used as an operand for
  * zipWithIndex. It is not exposed directly in the query language because it
  * cannot be represented in SQL outside of a 'zip' operation. */
final case class RangeFrom(start: Long = 1L) extends Column[Long] with NullaryNode
//TODO should not have to be a Column
