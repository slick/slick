package org.scalaquery.ast

import scala.math.{min, max}
import scala.collection.mutable.ArrayBuffer
import org.scalaquery.ql.Join.JoinType
import org.scalaquery.SQueryException
import org.scalaquery.util.SimpleTypeName
import org.scalaquery.ql.{Unpackable, ConstColumn}
import java.io.{StringWriter, PrintWriter, OutputStreamWriter}

trait NodeGenerator {
  def nodeDelegate: Node

  final def dump(name: String = "", prefix: String = "", nc: IdContext = IdContext(nodeDelegate)) {
    val out = new PrintWriter(new OutputStreamWriter(System.out))
    nodeDelegate.nodeDump(new DumpContext(out, nc), prefix, name)
    out.flush()
  }

  final def dumpString(name: String = "", prefix: String = "", nc: IdContext = IdContext(nodeDelegate)) = {
    val buf = new StringWriter
    val out = new PrintWriter(buf)
    nodeDelegate.nodeDump(new DumpContext(out, nc), prefix, name)
    out.flush()
    buf.getBuffer.toString
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
  def isNamedTable = false

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
    val check = if(this.isInstanceOf[Ref]) None else dc.nc.checkIdFor(this)
    val details = check match {
      case Some((id, true)) =>
        dc.out.println(prefix + name + "[" + id + "] " + this)
        true
      case Some((id, false)) =>
        dc.out.println(prefix + name + "<" + id + ">")
        false
      case None =>
        dc.out.println(prefix + name + this)
        true
    }
    if(details)
      for((chg, n) <- nodeChildGenerators.zip(nodeChildNames))
        Node(chg).nodeDump(dc, prefix + "  ", n+": ")
  }

  override def toString = this match {
    case p: Product =>
      val n = getClass.getName.replaceFirst(".*\\.", "").replaceFirst(".*\\$", "")
      val args = p.productIterator.filterNot(_.isInstanceOf[Node]).mkString(", ")
      if(args isEmpty) n else (n + ' ' + args)
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
    else if(o.isInstanceOf[Product]) ProductNode(o.asInstanceOf[Product])
    else throw new SQueryException("Cannot narrow "+o+" of type "+SimpleTypeName.forVal(o)+" to a Node")
}

final class DumpContext(val out: PrintWriter, val nc: IdContext)

trait ProductNode extends SimpleNode {
  override def toString = "ProductNode"
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Node = new ProductNode {
    lazy val nodeChildGenerators = ch
  }
  override def hashCode() = nodeChildren.hashCode()
  override def equals(o: Any) = o match {
    case p: ProductNode => nodeChildren == p.nodeChildren
    case _ => false
  }
}

object ProductNode {
  def apply(p: Product): ProductNode =
    new ProductNode { lazy val nodeChildGenerators = p.productIterator.toSeq }
  def apply(s: Any*): ProductNode =
    new ProductNode { lazy val nodeChildGenerators = s }
  def unapplySeq(p: ProductNode) = Some(p.nodeChildren)
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
  def nodeMapGenerators(f: Symbol => Symbol) =
    copy(elements = elements.map { case (s, n) => (f(s), n) })
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

final case class Wrapped(in: Node, what: Node) extends BinaryNode {
  def left = in
  def right = what
  protected[this] override def nodeChildNames = Seq("in", "what")
  protected[this] def nodeRebuild(left: Node, right: Node): Node = copy(in = left, what = right)
}

object Wrapped {
  def ifNeeded(in: Node, what: Node): Node =
    if(in eq what) what else Wrapped(in, what)
  def wrapUnpackable[E, U](in: Node, u: Unpackable[E, U]) = u.endoMap(n => WithOp.mapOp(n, { x => Wrapped.ifNeeded(Node(in), Node(x)) }))
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
  def nodeMapGenerators(f: Symbol => Symbol): FilteredQuery = {
    val fs = f(generator)
    if(fs eq generator) this else withGenerator(fs)
  }
  def withGenerator(gen: Symbol): FilteredQuery
  override def toString = this match {
    case p: Product =>
      val n = getClass.getName.replaceFirst(".*\\.", "").replaceFirst(".*\\$", "")
      val args = p.productIterator.filterNot(n => n.isInstanceOf[Node] || n.isInstanceOf[Symbol]).mkString(", ")
      if(args isEmpty) n else (n + ' ' + args)
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
  def withGenerator(gen: Symbol) = copy(generator = gen)
  def nodePostGeneratorChildren = Seq(where)
}

final case class GroupBy(from: Node, groupBy: Node, generator: Symbol = new AnonSymbol) extends FilteredQuery with BinaryNode with SimpleDefNode {
  def left = from
  def right = groupBy
  protected[this] override def nodeChildNames = Seq("from "+generator, "groupBy")
  protected[this] def nodeRebuild(left: Node, right: Node) = copy(from = left, groupBy = right)
  def withGenerator(gen: Symbol) = copy(generator = gen)
  def nodePostGeneratorChildren = Seq(groupBy)
}

final case class Take(from: Node, num: Int, generator: Symbol = new AnonSymbol) extends FilteredQuery with UnaryNode with SimpleDefNode {
  def child = from
  protected[this] override def nodeChildNames = Seq("from "+generator)
  protected[this] def nodeRebuild(child: Node) = copy(from = child)
  def withGenerator(gen: Symbol) = copy(generator = gen)
  def nodePostGeneratorChildren = Seq.empty
}

final case class Drop(from: Node, num: Int, generator: Symbol = new AnonSymbol) extends FilteredQuery with UnaryNode with SimpleDefNode {
  def child = from
  protected[this] override def nodeChildNames = Seq("from "+generator)
  protected[this] def nodeRebuild(child: Node) = copy(from = child)
  def withGenerator(gen: Symbol) = copy(generator = gen)
  def nodePostGeneratorChildren = Seq.empty
}

/** An extractor for nested Take and Drop nodes */
object TakeDrop {
  def unapply(n: Node): Option[(Node, Option[Int], Option[Int])] = n match {
    case Take(from, num, sym) => unapply(from) match {
      case Some((f, Some(t), d)) => Some((f, Some(min(t, num)), d))
      case Some((f, None, d)) => Some((f, Some(num), d))
      case _ => Some((from, Some(num), None))
    }
    case Drop(from, num, sym) => unapply(from) match {
      case Some((f, Some(t), None)) => Some((f, Some(max(0, t-num)), Some(num)))
      case Some((f, None, Some(d))) => Some((f, None, Some(d+num)))
      case Some((f, Some(t), Some(d))) => Some((f, Some(max(0, t-num)), Some(d+num)))
      case _ => Some((from, None, Some(num)))
    }
    case _ => None
  }
}

final case class BaseJoin(leftGen: Symbol, rightGen: Symbol, left: Node, right: Node, jt: JoinType) extends BinaryNode with SimpleDefNode {
  protected[this] def nodeRebuild(left: Node, right: Node) = copy(left = left, right = right)
  protected[this] override def nodeChildNames = Seq("left "+leftGen, "right "+rightGen)
  override def toString = "BaseJoin " + jt.sqlName
  def nodeGenerators = Seq((leftGen, left), (rightGen, right))
  def nodeMapGenerators(f: Symbol => Symbol) = {
    val fl = f(leftGen)
    val fr = f(rightGen)
    if((fl eq leftGen) && (fr eq rightGen)) this else copy(leftGen = fl, rightGen = fr)
  }
  def nodePostGeneratorChildren = Seq.empty
}

final case class FilteredJoin(leftGen: Symbol, rightGen: Symbol, left: Node, right: Node, jt: JoinType, on: Node) extends SimpleNode with SimpleDefNode {
  protected[this] def nodeChildGenerators = IndexedSeq(left, right, on)
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]) = copy(left = ch(0), right = ch(1), on = ch(2))
  protected[this] override def nodeChildNames = Seq("left "+leftGen, "right "+rightGen, "on")
  override def toString = "FilteredJoin " + jt.sqlName
  def nodeGenerators = Seq((leftGen, left), (rightGen, right))
  def nodeMapGenerators(f: Symbol => Symbol) = {
    val fl = f(leftGen)
    val fr = f(rightGen)
    if((fl eq leftGen) && (fr eq rightGen)) this else copy(leftGen = fl, rightGen = fr)
  }
  def nodePostGeneratorChildren = Seq(on)
}

final case class Union(left: Node, right: Node, all: Boolean, leftGen: Symbol = new AnonSymbol, rightGen: Symbol = new AnonSymbol) extends BinaryNode with SimpleDefNode {
  protected[this] def nodeRebuild(left: Node, right: Node) = copy(left = left, right = right)
  override def toString = if(all) "Union all" else "Union"
  protected[this] override def nodeChildNames = Seq("left "+leftGen, "right "+rightGen)
  def nodeGenerators = Seq((leftGen, left), (rightGen, right))
  def nodeMapGenerators(f: Symbol => Symbol) = {
    val fl = f(leftGen)
    val fr = f(rightGen)
    if((fl eq leftGen) && (fr eq rightGen)) this else copy(leftGen = fl, rightGen = fr)
  }
  def nodePostGeneratorChildren = Seq.empty
}

final case class Bind(generator: Symbol, from: Node, select: Node) extends BinaryNode with SimpleDefNode {
  def left = from
  def right = select
  protected[this] override def nodeChildNames = Seq("from "+generator, "select")
  protected[this] def nodeRebuild(left: Node, right: Node) = copy(from = left, select = right)
  def nodeGenerators = Seq((generator, from))
  override def toString = "Bind"
  def nodeMapGenerators(f: Symbol => Symbol) = {
    val fs = f(generator)
    if(fs eq generator) this else copy(generator = fs)
  }
  def nodePostGeneratorChildren = Seq(select)
}
