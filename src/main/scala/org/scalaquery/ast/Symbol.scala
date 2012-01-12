package org.scalaquery.ast

import OptimizerUtil._
import org.scalaquery.ql.Unpackable

/**
 * A symbol defined in the AST.
 */
class Symbol(private[this] var _name: String = null) {
  def name = if(_name eq null) "@"+System.identityHashCode(this) else _name
  def name_= (s: String) = _name = s
  def hasName = _name ne null
  override def toString = name
}

object Symbol {
  def assignNames(tree: Node, prefix: String = "s", force: Boolean = false) = {
    var num = 0
    val symName = memoized[Symbol, String](_ => { s => num += 1; prefix + num })
    def f(n: Node) {
      n match {
        case d : DefNode => d.nodeSymDefs.foreach { s =>
          if(force || !s.hasName) s.name = symName(s)
        }
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
  override def nodeChildNames = Seq("value")
}

object InRef {
  def forUnpackable[E, U](sym: Symbol, u: Unpackable[E, U]) = u.endoMap(n => WithOp.mapOp(n, { x => InRef(sym, x) }))
}

/**
 * A reference to a Symbol
 */
case class Ref(sym: Symbol) extends NullaryNode

/**
 * A node which introduces a column alias
 */
case class As(sym: Symbol, child: Node) extends UnaryNode with DefNode {
  protected[this] def nodeRebuild(child: Node) = copy(child = child)
  override def nodeChildNames = Seq("value")
  def nodeGenerators = Seq((sym, child))
  def nodeMapGenerators(f: Symbol => Symbol) = {
    val fs = f(sym)
    if(fs eq sym) this else copy(sym = fs)
  }
}

/**
 * A Node which introduces Symbols.
 */
trait DefNode extends Node {
  final def nodeSymDefs: Seq[Symbol] = nodeGenerators.map(_._1)
  def nodeGenerators: Seq[(Symbol, Node)]
  def nodeMapGenerators(f: Symbol => Symbol): Node
}
