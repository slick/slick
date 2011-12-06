package org.scalaquery.util

import java.io.{PrintWriter, OutputStreamWriter}
import org.scalaquery.SQueryException
import org.scalaquery.ql.ConstColumn

trait NodeGenerator {
  def nodeDelegate: Node
}

/**
 * A node in the query AST
 */
trait Node extends NodeGenerator {
  protected[this] def nodeChildGenerators: Iterable[Any]
  protected[this] def nodeChildNames: Iterable[String] = Stream.from(0).map(_.toString)

  final def nodeChildren = nodeChildGenerators.map(Node.apply _)

  def nodeDelegate: Node = this
  def isNamedTable = false

  def dump(dc: Node.DumpContext, prefix: String, name: String) {
    val (tname, details) = if(isNamedTable) {
      val (s, newName) = dc.nc.checkNameFor(this)
      ("<" + s + "> ", newName)
    } else ("", true)
    dc.out.println(prefix + name + tname + (if(details) this else "..."))
    if(details)
      for((chg, n) <- nodeChildGenerators.zip(nodeChildNames))
        Node(chg).dump(dc, prefix + "  ", n+": ")
  }

  final def dump(name: String, nc: NamingContext = NamingContext()) {
    val out = new PrintWriter(new OutputStreamWriter(System.out))
    dump(new Node.DumpContext(out, nc), "", name)
    out.flush()
  }

  override def toString = this match {
    case p: Product =>
      val n = getClass.getName.replaceFirst(".*\\.", "").replaceFirst(".*\\$", "")
      val args = p.productIterator.filterNot(_.isInstanceOf[Node]).mkString(", ")
      if(args isEmpty) n else (n + ' ' + args)
    case _ => super.toString
  }
}

object Node {
  def apply(o:Any): Node = o match {
    case null => ConstColumn.NULL
    case n:NodeGenerator => n.nodeDelegate
    case p:Product => new ProductNode { val product = p }
    case r:AnyRef => throw new SQueryException("Cannot narrow "+o+" of type "+SimpleTypeName.forVal(r)+" to a Node")
    case _ => throw new SQueryException("Cannot narrow "+o+" to a Node")
  }

  final class DumpContext(val out: PrintWriter, val nc: NamingContext)
}

trait ProductNode extends Node {
  val product: Product
  lazy val nodeChildGenerators = product.productIterator.toSeq
  override def toString = "ProductNode"
}

trait BinaryNode extends Node {
  val left: Node
  val right: Node
  protected[this] def nodeChildGenerators = Seq(left, right)
}

trait UnaryNode extends Node {
  val child: Node
  protected[this] def nodeChildGenerators = Seq(child)
}

trait NullaryNode extends Node {
  protected[this] def nodeChildGenerators = Nil
}
