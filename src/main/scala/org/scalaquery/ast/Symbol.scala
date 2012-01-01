package org.scalaquery.ast

import OptimizerUtil._

/**
 * A symbol defined in the AST.
 */
class Symbol(private[this] var _name: String = null) {
  def name = if(_name eq null) "@"+System.identityHashCode(this) else _name
  def name_= (s: String) = _name = s
  override def toString = name
}

object Symbol {
  def assignNames(tree: Node) = {
    var num = 0
    val symName = memoized[Symbol, String](_ => { s => num += 1; "s" + num })
    def f(n: Node) {
      n match {
        case d : DefNode => d.nodeSymDefs.foreach(s => s.name = symName(s))
        case _ =>
      }
      n.nodeChildren.foreach(f)
    }
    f(tree)
  }
}

/**
 * An expression introduced by a Symbol, wrapped in a reference to the Symbol.
 */
case class InRef(sym: Symbol, child: Node) extends UnaryNode {
  protected[this] def nodeRebuild(child: Node) = copy(child = child)
  override def toString = "InRef "+sym
  override def nodeChildNames = Seq("value")
}

/**
 * A reference to a Symbol
 */
case class Ref(sym: Symbol) extends NullaryNode

/**
 * A Node which introduces Symbols.
 */
trait DefNode extends Node {
  def nodeSymDefs: Seq[Symbol]
}
