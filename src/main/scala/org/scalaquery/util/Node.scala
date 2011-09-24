package org.scalaquery.util

import java.io.{PrintWriter, OutputStreamWriter}
import org.scalaquery.SQueryException
import org.scalaquery.ql.ConstColumn

/**
 * A node in the query AST
 */
trait Node {
  def nodeChildren: List[Node]
  def nodeDelegate: Node = this
  def isNamedTable = false

  def nodeNamedChildren: Seq[(Node, String)] = nodeChildren.toStream.zip(Stream.from(0).map(_.toString))

  def dump(dc: Node.DumpContext, prefix: String, name: String) {
    val (tname, details) = if(isNamedTable) {
      val (s, newName) = dc.nc.checkNameFor(this)
      ("<" + s + "> ", newName)
    } else ("", true)
    dc.out.println(prefix + name + tname + (if(details) this else "..."))
    if(details)
      for((ch, n) <- nodeNamedChildren)
        ch.dump(dc, prefix + "  ", n+": ")
  }

  final def dump(name: String, nc: NamingContext = NamingContext()) {
    val out = new PrintWriter(new OutputStreamWriter(System.out))
    dump(new Node.DumpContext(out, nc), "", name)
    out.flush()
  }
}

object Node {
  def apply(o:Any): Node = o match {
    case null => ConstColumn.NULL
    case n:Node => n.nodeDelegate
    case p:Product => new ProductNode { val product = p }
    case r:AnyRef => throw new SQueryException("Cannot narrow "+o+" of type "+SimpleTypeName.forVal(r)+" to a Node")
    case _ => throw new SQueryException("Cannot narrow "+o+" to a Node")
  }

  final class DumpContext(val out: PrintWriter, val nc: NamingContext)
}

trait ProductNode extends Node {
  val product: Product
  lazy val nodeChildren =
    ( for(i <- 0 until product.productArity)
        yield Node(product.productElement(i)) ).toList

  override def toString = "ProductNode "+product
}

trait BinaryNode extends Node {
  val left: Node
  val right: Node
  def nodeChildren = left :: right :: Nil
}

trait UnaryNode extends Node {
  val child: Node
  def nodeChildren = child :: Nil
}

trait NullaryNode extends Node {
  def nodeChildren = Nil
}
