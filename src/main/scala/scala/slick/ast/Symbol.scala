package scala.slick.ast

import OptimizerUtil._
import scala.slick.ql.{RawNamedColumn, ShapedValue}

/**
 * A symbol which can be used in the AST.
 */
abstract class Symbol {
  def name: String
  override def toString = name
}

/**
 * A named symbol which refers to an (aliased or unaliased) field.
 */
case class FieldSymbol(name: String)(val column: Option[RawNamedColumn] = None) extends Symbol

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
  def named(name: String) = {
    val s = new AnonSymbol
    s.name = name
    s
  }
  def unapply(a: AnonSymbol) = Some(a.name)
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
  def forShapedValue[E, U](sym: Symbol, u: ShapedValue[E, U]) = u.endoMap(n => WithOp.mapOp(n, { x => InRef(sym, x) }))
}

/**
 * A reference to a Symbol
 */
case class Ref(sym: Symbol) extends NullaryNode with RefNode {
  def nodeReferences = Seq(sym)
  def nodeMapReferences(f: Symbol => Symbol) = copy(sym = f(sym))
}

/**
 * A reference to a Symbol pointing to a table which should not be expanded
 */
case class TableRef(sym: Symbol) extends NullaryNode with RefNode {
  def nodeReferences = Seq(sym)
  def nodeMapReferences(f: Symbol => Symbol) = copy(sym = f(sym))
}

/**
 * A reference to a field in a struct
 */
case class FieldRef(struct: Symbol, field: Symbol) extends NullaryNode with RefNode {
  override def toString = "FieldRef " + struct + "." + field
  def nodeReferences = Seq(struct, field)
  def nodeMapReferences(f: Symbol => Symbol) = {
    val s2 = f(struct)
    val f2 = f(field)
    if((s2 eq struct) && (f2 eq field)) this
    else FieldRef(s2, f2)
  }
}

/**
 * A Node which introduces Symbols.
 */
trait DefNode extends Node {
  def nodeGenerators: Seq[(Symbol, Node)]
  def nodePostGeneratorChildren: Seq[Node]
  def nodeMapGenerators(f: Symbol => Symbol): Node
  def nodeMapScopedChildren(f: (Option[Symbol], Node) => Node): Node
}

trait SimpleDefNode extends DefNode { _: SimpleNode =>
  def nodeMapScopedChildren(f: (Option[Symbol], Node) => Node) = {
    val ft = f.tupled
    val all = (nodeGenerators.iterator.map{ case (sym, n) => (Some(sym), n) } ++
      nodePostGeneratorChildren.iterator.map{ n => (None, n) }).toIndexedSeq
    nodeRebuild(all.map(ft))
  }
}

/**
 * A Node which references Symbols.
 */
trait RefNode extends Node {
  def nodeReferences: Seq[Symbol]
  def nodeMapReferences(f: Symbol => Symbol): Node
}
