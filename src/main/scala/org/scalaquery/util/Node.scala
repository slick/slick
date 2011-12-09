package org.scalaquery.util

import scala.collection.mutable.ArrayBuffer
import java.io.{PrintWriter, OutputStreamWriter}
import org.scalaquery.SQueryException
import org.scalaquery.ql.ConstColumn

trait NodeGenerator {
  def nodeDelegate: Node

  final def dump(name: String, nc: NamingContext = NamingContext()) {
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
    val (tname, details) = if(isNamedTable) {
      val (s, newName) = dc.nc.checkNameFor(this)
      ("<" + s + "> ", newName)
    } else ("", true)
    dc.out.println(prefix + name + tname + (if(details) this else "..."))
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
    else if(o.isInstanceOf[Product]) new ProductNode { lazy val nodeChildGenerators = o.asInstanceOf[Product].productIterator.toSeq }
    else throw new SQueryException("Cannot narrow "+o+" of type "+SimpleTypeName.forVal(o)+" to a Node")

  final class DumpContext(val out: PrintWriter, val nc: NamingContext)
}

trait ProductNode extends SimpleNode {
  override def toString = "ProductNode"
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Node = new ProductNode {
    lazy val nodeChildGenerators = ch
  }
}

trait BinaryNode extends SimpleNode {
  def left: Node
  def right: Node
  protected[this] def nodeChildGenerators = Seq(left, right)
  protected[this] final def nodeRebuild(ch: IndexedSeq[Node]): Node = nodeRebuild(ch(0), ch(1))
  protected[this] def nodeRebuild(left: Node, right: Node): Node
}

trait UnaryNode extends SimpleNode {
  def child: Node
  protected[this] def nodeChildGenerators = Seq(child)
  protected[this] final def nodeRebuild(ch: IndexedSeq[Node]): Node = nodeRebuild(ch(0))
  protected[this] def nodeRebuild(child: Node): Node
}

trait NullaryNode extends SimpleNode {
  protected[this] def nodeChildGenerators = Nil
  protected[this] final def nodeRebuild(ch: IndexedSeq[Node]): Node = this
}
