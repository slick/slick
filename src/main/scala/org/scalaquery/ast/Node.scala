package org.scalaquery.ast

import scala.collection.mutable.ArrayBuffer
import java.io.{PrintWriter, OutputStreamWriter}
import org.scalaquery.SQueryException
import org.scalaquery.util.SimpleTypeName
import org.scalaquery.ql.{Unpackable, ConstColumn}

trait NodeGenerator {
  def nodeDelegate: Node

  final def dump(name: String, nc: IdContext = IdContext(nodeDelegate)) {
    val out = new PrintWriter(new OutputStreamWriter(System.out))
    nodeDelegate.nodeDump(new Node.DumpContext(out, nc), "", name)
    out.flush()
  }
}

/**
 * A node in the query AST
 */
trait Node extends NodeGenerator {
  protected[this] def nodeChildGenerators: Iterable[Any]
  protected[this] def nodeChildNames: Iterable[String] = Stream.from(0).map(_.toString)

  final def nodeChildren = nodeChildGenerators.map(Node.apply _)

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

  def nodeDump(dc: Node.DumpContext, prefix: String, name: String) {
    val details = dc.nc.checkIdFor(this) match {
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
    else if(o.isInstanceOf[WithOp] && (o.asInstanceOf[WithOp].op ne null)) o.asInstanceOf[WithOp].op
    else if(o.isInstanceOf[NodeGenerator]) o.asInstanceOf[NodeGenerator].nodeDelegate
    else if(o.isInstanceOf[Product]) ProductNode(o.asInstanceOf[Product])
    else throw new SQueryException("Cannot narrow "+o+" of type "+SimpleTypeName.forVal(o)+" to a Node")

  final class DumpContext(val out: PrintWriter, val nc: IdContext)
}

trait ProductNode extends SimpleNode {
  override def toString = "ProductNode"
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Node = new ProductNode {
    lazy val nodeChildGenerators = ch
  }
}

object ProductNode {
  def apply(p: Product): ProductNode =
    new ProductNode { lazy val nodeChildGenerators = p.productIterator.toSeq }
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

final case class Wrapped(what: Node, in: Node) extends SimpleNode {
  protected[this] def nodeChildGenerators = Seq(what, in)
  protected[this] override def nodeChildNames = Seq("what", "in")
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Node = copy(what = ch(0), in = ch(1))
}

final case class Alias(child: Node) extends UnaryNode {
  protected[this] override def nodeChildNames = Seq("of")
  protected[this] def nodeRebuild(child: Node): Node = copy(child = child)
  override def hashCode = System.identityHashCode(this)
  override def equals(o: Any) = this eq o.asInstanceOf[AnyRef]
  override def isNamedTable = true
}

object Alias {
  def forUnpackable[E, U](u: Unpackable[E, U]) = u.endoMap(n => WithOp.mapOp(n, { x => Alias(Node(x)) }))
}
