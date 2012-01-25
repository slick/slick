package org.scalaquery.ast

import OptimizerUtil._
import org.scalaquery.ql.Unpackable

/**
 * A symbol which can be used in the AST.
 */
abstract class Symbol {
  def name: String
  override def toString = name
}

/**
 * A named symbol which refers to an unaliased field.
 */
case class FieldSymbol(name: String) extends Symbol

/**
 * An anonymous symbol defined in the AST.
 */
class AnonSymbol extends Symbol {
  private[this] var _name: String = null
  def name = if(_name eq null) "@"+System.identityHashCode(this) else _name
  def name_= (s: String) = _name = s
  def hasName = _name ne null
  override def toString = name
}

object AnonSymbol {
  def assignNames(tree: Node, prefix: String = "s", force: Boolean = false) = {
    var num = 0
    val symName = memoized[AnonSymbol, String](_ => { s => num += 1; prefix + num })
    tree.foreach {
      case d : DefNode => d.nodeGenerators.foreach {
        case (s: AnonSymbol, _) if force || !s.hasName => s.name = symName(s)
        case _ =>
      }
      case _ =>
    }
  }
}

/**
 * An expression introduced by a Symbol, wrapped in a reference to the Symbol.
 */
case class InRef(sym: Symbol, child: Node) extends UnaryNode with RefNode {
  protected[this] def nodeRebuild(child: Node) = copy(child = child)
  override def nodeChildNames = Seq("value")
  def nodeReferences = Seq(sym)
  def nodeMapReferences(f: Symbol => Symbol) = copy(sym = f(sym))
}

object InRef {
  def forUnpackable[E, U](sym: Symbol, u: Unpackable[E, U]) = u.endoMap(n => WithOp.mapOp(n, { x => InRef(sym, x) }))
}

/**
 * A reference to a Symbol
 */
case class Ref(sym: Symbol) extends NullaryNode with RefNode {
  def nodeReferences = Seq(sym)
  def nodeMapReferences(f: Symbol => Symbol) = copy(sym = f(sym))
}

/**
 * A reference to a field in a struct
 */
case class Path(path: Symbol*) extends NullaryNode with RefNode {
  override def toString = "Path " + path.mkString(".")
  def nodeReferences = path
  def nodeMapReferences(f: Symbol => Symbol) =
    mapOrNone(path, f).map(newp => Path(newp:_*)).getOrElse(this)
}

/**
 * A Node which introduces Symbols.
 */
trait DefNode extends Node {
  def nodeGenerators: Seq[(Symbol, Node)]
  def nodeMapGenerators(f: Symbol => Symbol): Node
}

/**
 * A Node which references Symbols.
 */
trait RefNode extends Node {
  def nodeReferences: Seq[Symbol]
  def nodeMapReferences(f: Symbol => Symbol): Node
}
