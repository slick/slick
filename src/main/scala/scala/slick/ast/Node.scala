package scala.slick.ast

import java.io.{StringWriter, PrintWriter, OutputStreamWriter}
import scala.slick.SLICKException
import scala.slick.ql.{ConstColumn, ShapedValue}
import scala.slick.util.SimpleTypeName
import scala.collection.mutable.{HashSet, ArrayBuffer, HashMap}
import opt.Util.nodeToNodeOps

trait NodeGenerator {
  def nodeDelegate: Node

  final def dump(name: String = "", prefix: String = "") {
    val out = new PrintWriter(new OutputStreamWriter(System.out))
    dumpTo(out, name, prefix)
  }

  final def dumpString(name: String = "", prefix: String = "") = {
    val buf = new StringWriter
    dumpTo(new PrintWriter(buf), name, prefix)
    buf.getBuffer.toString
  }

  final def dumpTo(out: PrintWriter, name: String = "", prefix: String = "") {
    val dc = new DumpContext(out, nodeDelegate)
    nodeDelegate.nodeDump(dc, prefix, name)
    for(GlobalSymbol(name, node) <- dc.orphans)
      node.nodeDump(dc, prefix, "/"+name+": ")
    out.flush()
  }
}

/**
 * A node in the query AST
 */
trait Node extends NodeGenerator {
  protected[this] def nodeChildGenerators: Seq[Any]
  protected[this] def nodeChildNames: Iterable[String] = Stream.from(0).map(_.toString)

  final lazy val nodeChildren = nodeChildGenerators.map(Node.apply _)

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

  def nodeDump(dc: DumpContext, prefix: String, name: String) {
    this match {
      case n: DefNode => n.nodeGenerators.foreach(t => dc.addDef(t._1))
      case n: RefNode => n.nodeReferences.foreach(dc.addRef)
      case _ =>
    }
    this match {
      case Path(l) =>
        // Print paths on a single line
        dc.out.println(prefix + name + Path.toString(l))
        this.foreach { case n: RefNode => n.nodeReferences.foreach(dc.addRef) }
      case _ =>
        dc.out.println(prefix + name + this)
        for((chg, n) <- nodeChildGenerators.zip(nodeChildNames))
          Node(chg).nodeDump(dc, prefix + "  ", n+": ")
    }
  }

  override def toString = this match {
    case p: Product =>
      val n = getClass.getName.replaceFirst(".*\\.", "").replaceFirst(".*\\$", "")
      val args = p.productIterator.filterNot(_.isInstanceOf[Node]).mkString(", ")
      if(args.isEmpty) n else (n + ' ' + args)
    case _ => super.toString
  }
}

trait SimpleNode extends Node {
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Node

  def nodeMapChildren(f: Node => Node): Node =
    nodeMapNodes(nodeChildren, f).map(nodeRebuild).getOrElse(this)
}

object Node {
  def apply(o:Any): Node =
    if(o == null) ConstColumn.NULL
    else if(o.isInstanceOf[WithOp] && (o.asInstanceOf[WithOp].op ne null)) Node(o.asInstanceOf[WithOp].op)
    else if(o.isInstanceOf[NodeGenerator]) {
      val gen = o.asInstanceOf[NodeGenerator]
      if(gen.nodeDelegate eq gen) gen.nodeDelegate else Node(gen.nodeDelegate)
    }
    else if(o.isInstanceOf[Product]) ProductNode(o.asInstanceOf[Product].productIterator.toSeq)
    else throw new SLICKException("Cannot narrow "+o+" of type "+SimpleTypeName.forVal(o)+" to a Node")
}

final class DumpContext(val out: PrintWriter, base: Node) {
  private[this] val refs = new HashSet[Symbol]
  private[this] val defs = new HashSet[Symbol]

  def addRef(s: Symbol) = refs.add(s)
  def addDef(s: Symbol) = defs.add(s)

  def orphans: Set[Symbol] = {
    val newRefs = new HashSet[GlobalSymbol] ++ refs.collect { case g: GlobalSymbol => g }
    def scan(): Unit = {
      val toScan = newRefs.toSet
      newRefs.clear()
      toScan.foreach { _.target.foreach {
          case r: RefNode =>
            r.nodeReferences.foreach {
              case g: GlobalSymbol =>
                if(!refs.contains(g)) {
                  refs += g
                  newRefs += g
                }
              case _ =>
            }
          case _ =>
      }}
    }
    while(!newRefs.isEmpty) scan()
    (refs -- defs).toSet
  }
}

trait ProductNode extends SimpleNode {
  override def toString = "ProductNode"
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Node = new ProductNode {
    lazy val nodeChildGenerators = ch
  }
  override protected[this] def nodeChildNames: Iterable[String] = Stream.from(1).map(_.toString)
  override def hashCode() = nodeChildren.hashCode()
  override def equals(o: Any) = o match {
    case p: ProductNode => nodeChildren == p.nodeChildren
    case _ => false
  }
}

object ProductNode {
  def apply(s: Seq[Any]): ProductNode =
    new ProductNode { lazy val nodeChildGenerators = s }
  def unapply(p: ProductNode) = Some(p.nodeChildren)
}

case class StructNode(elements: IndexedSeq[(Symbol, Node)]) extends ProductNode with SimpleDefNode {
  override def toString = "StructNode"
  protected[this] override def nodeChildNames = elements.map(_._1.toString)
  def nodeChildGenerators = elements.map(_._2)
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

trait BinaryNode extends SimpleNode {
  def left: Node
  def right: Node
  protected[this] final def nodeChildGenerators = Seq(left, right)
  protected[this] final def nodeRebuild(ch: IndexedSeq[Node]): Node = nodeRebuild(ch(0), ch(1))
  protected[this] def nodeRebuild(left: Node, right: Node): Node
}

trait UnaryNode extends SimpleNode {
  def child: Node
  protected[this] final def nodeChildGenerators = Seq(child)
  protected[this] final def nodeRebuild(ch: IndexedSeq[Node]): Node = nodeRebuild(ch(0))
  protected[this] def nodeRebuild(child: Node): Node
}

trait NullaryNode extends SimpleNode {
  protected[this] final def nodeChildGenerators = Nil
  protected[this] final def nodeRebuild(ch: IndexedSeq[Node]): Node = this
}

final case class Pure(value: Node) extends UnaryNode {
  def child = value
  protected[this] override def nodeChildNames = Seq("value")
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
  protected[this] override def nodeChildNames = Seq("from "+generator, "where")
  protected[this] def nodeRebuild(left: Node, right: Node) = copy(from = left, where = right)
  override def nodeDelegate = if(where == ConstColumn(true)) left else super.nodeDelegate
  def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(generator = gen(0))
  def nodePostGeneratorChildren = Seq(where)
}

final case class SortBy(generator: Symbol, from: Node, by: Seq[(Node, Ordering)]) extends FilteredQuery with SimpleNode with SimpleDefNode {
  protected[this] def nodeChildGenerators = from +: by.map(_._1)
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]) =
    copy(from = ch(0), by = by.zip(ch.tail).map{ case ((_, o), n) => (n, o) })
  protected[this] override def nodeChildNames = ("from "+generator) +: by.zipWithIndex.map("by" + _._2)
  def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(generator = gen(0))
  def nodePostGeneratorChildren = by.map(_._1)
  override def toString = "SortBy " + by.map(_._2).mkString(", ")
}

final case class OrderBy(generator: Symbol, from: Node, by: Seq[(Node, Ordering)]) extends FilteredQuery with SimpleNode with SimpleDefNode {
  protected[this] def nodeChildGenerators = from +: by.map(_._1)
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]) =
    copy(from = ch(0), by = by.zip(ch.tail).map{ case ((_, o), n) => (n, o) })
  protected[this] override def nodeChildNames = ("from "+generator) +: by.zipWithIndex.map("by" + _._2)
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

final case class GroupBy(from: Node, groupBy: Node, generator: Symbol = new AnonSymbol) extends FilteredQuery with BinaryNode with SimpleDefNode {
  def left = from
  def right = groupBy
  protected[this] override def nodeChildNames = Seq("from "+generator, "groupBy")
  protected[this] def nodeRebuild(left: Node, right: Node) = copy(from = left, groupBy = right)
  def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(generator = gen(0))
  def nodePostGeneratorChildren = Seq(groupBy)
}

final case class Take(from: Node, num: Int, generator: Symbol = new AnonSymbol) extends FilteredQuery with UnaryNode with SimpleDefNode {
  def child = from
  protected[this] override def nodeChildNames = Seq("from "+generator)
  protected[this] def nodeRebuild(child: Node) = copy(from = child)
  def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(generator = gen(0))
  def nodePostGeneratorChildren = Seq.empty
}

final case class Drop(from: Node, num: Int, generator: Symbol = new AnonSymbol) extends FilteredQuery with UnaryNode with SimpleDefNode {
  def child = from
  protected[this] override def nodeChildNames = Seq("from "+generator)
  protected[this] def nodeRebuild(child: Node) = copy(from = child)
  def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(generator = gen(0))
  def nodePostGeneratorChildren = Seq.empty
}

final case class Join(leftGen: Symbol, rightGen: Symbol, left: Node, right: Node, jt: JoinType, on: Node) extends SimpleNode with SimpleDefNode {
  protected[this] def nodeChildGenerators = IndexedSeq(left, right, on)
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]) = copy(left = ch(0), right = ch(1), on = ch(2))
  protected[this] override def nodeChildNames = Seq("left "+leftGen, "right "+rightGen, "on")
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
  protected[this] override def nodeChildNames = Seq("left "+leftGen, "right "+rightGen)
  def nodeGenerators = Seq((leftGen, left), (rightGen, right))
  def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(leftGen = gen(0), rightGen = gen(1))
  def nodePostGeneratorChildren = Seq.empty
}

final case class Bind(generator: Symbol, from: Node, select: Node) extends BinaryNode with SimpleDefNode {
  def left = from
  def right = select
  protected[this] override def nodeChildNames = Seq("from "+generator, "select")
  protected[this] def nodeRebuild(left: Node, right: Node) = copy(from = left, select = right)
  def nodeGenerators = Seq((generator, from))
  override def toString = "Bind"
  def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(generator = gen(0))
  def nodePostGeneratorChildren = Seq(select)
}

final case class TableExpansion(generator: Symbol, table: Node, columns: Node) extends BinaryNode with SimpleDefNode {
  def left = table
  def right = columns
  protected[this] override def nodeChildNames = Seq("table "+generator, "columns")
  protected[this] def nodeRebuild(left: Node, right: Node) = copy(table = left, columns = right)
  def nodeGenerators = Seq((generator, table))
  override def toString = "TableExpansion"
  def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(generator = gen(0))
  def nodePostGeneratorChildren = Seq(columns)
}

final case class TableRefExpansion(marker: Symbol, ref: Node, columns: Node) extends BinaryNode with SimpleDefNode {
  def left = ref
  def right = columns
  protected[this] override def nodeChildNames = Seq("ref", "columns")
  protected[this] def nodeRebuild(left: Node, right: Node) = copy(ref = left, columns = right)
  def nodeGenerators = Seq((marker, ref))
  override def toString = "TableRefExpansion "+marker
  def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(marker = gen(0))
  def nodePostGeneratorChildren = Seq(columns)
}

final case class Select(in: Node, field: Symbol) extends UnaryNode with SimpleRefNode {
  def child = in
  protected[this] override def nodeChildNames = Seq("in")
  protected[this] def nodeRebuild(child: Node) = copy(in = child)
  def nodeReferences: Seq[Symbol] = Seq(field)
  def nodeRebuildWithReferences(gen: IndexedSeq[Symbol]) = copy(field = gen(0))
}

/** A constructor/extractor for nested Selects starting at a Ref */
object Path {
  def apply(l: List[Symbol]): Node = l match {
    case s :: Nil => Ref(s)
    case s :: l => Select(apply(l), s)
  }
  def unapply(n: Node): Option[List[Symbol]] = n match {
    case Select(Ref(s1), s2) => Some(s2 :: s1 :: Nil)
    case Select(in, s) => unapply(in).map(l => s :: l)
    case _ => None
  }
  def toString(path: Seq[Symbol]): String = path.reverseIterator.mkString("Path ", ".", "")
  def toString(s: Select): String = s match {
    case PathOrRef(syms) => toString(syms)
    case n => n.toString
  }
}

/** An extractor for a possibly empty Path */
object PathOrRef {
  def unapply(n: Node): Option[List[Symbol]] = n match {
    case Path(l) => Some(l)
    case Ref(sym) => Some(List(sym))
    case _ => None
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
  protected[this] def nodeChildGenerators = defs.map(_._2) :+ in
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]) =
    copy(defs = defs.zip(ch.init).map{ case ((s, _), n) => (s, n) }, in = ch.last)
  protected[this] override def nodeChildNames = defs.map("let " + _._1.toString) :+ "in"
  def nodeGenerators = defs
  def nodePostGeneratorChildren = Seq(in)
  def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]): Node =
    copy(defs = (defs, gen).zipped.map((e, s) => (s, e._2)))
  override def toString = "LetDynamic"
}
