package scala.slick.ast

import opt.Util._
import scala.slick.ql.RawNamedColumn

/** A symbol which can be used in the AST. */
abstract class Symbol {
  def name: String
  override def toString = name
}

/** A named symbol which refers to an (aliased or unaliased) field. */
case class FieldSymbol(name: String)(val column: Option[RawNamedColumn] = None) extends Symbol

/** An element of a ProductNode */
case class ElementSymbol(idx: Int) extends Symbol {
  def name = "_" + idx
}

/** A named symbol which refers to a proper database table. */
case class TableSymbol(name: String) extends Symbol

/** An anonymous symbol defined in the AST. */
class AnonSymbol extends Symbol {
  private[this] var _name: String = null
  def name = if(_name eq null) "@"+System.identityHashCode(this) else _name
  def name_= (s: String) = _name = s
  def hasName = _name ne null
  override def toString = name
}

object AnonSymbol {
  def assignNames(tree: Node, prefix: String = "s", force: Boolean = false, start: Int = 0): Int = {
    var num = start
    val symName = memoized[AnonSymbol, String](_ => { s => num += 1; prefix + num })
    tree.foreach {
      case d : DefNode => d.nodeGenerators.foreach {
        case (s: AnonSymbol, _) if force || !s.hasName => s.name = symName(s)
        case _ =>
      }
      case _ =>
    }
    num
  }
  def named(name: String) = {
    val s = new AnonSymbol
    s.name = name
    s
  }
  def unapply(a: AnonSymbol) = Some(a.name)
}

case class IntrinsicSymbol(val target: Node) extends Symbol {
  def name = "/"+System.identityHashCode(this)
  override def hashCode = System.identityHashCode(target)
  override def equals(o: Any) = o match {
    case i: IntrinsicSymbol => target eq i.target
    case _ => false
  }
}

/** A reference to a Symbol */
case class Ref(sym: Symbol) extends NullaryNode with SimpleRefNode {
  def nodeReferences = Seq(sym)
  def nodeRebuildWithReferences(gen: IndexedSeq[Symbol]) = copy(sym = gen(0))
}

/** A Node which introduces Symbols. */
trait DefNode extends Node {
  def nodeGenerators: Seq[(Symbol, Node)]
  def nodePostGeneratorChildren: Seq[Node]
  def nodeMapGenerators(f: Symbol => Symbol): Node
  def nodeMapScopedChildren(f: (Option[Symbol], Node) => Node): DefNode
}

trait SimpleDefNode extends DefNode { this: SimpleNode =>
  def nodeMapScopedChildren(f: (Option[Symbol], Node) => Node) = {
    val all = (nodeGenerators.iterator.map{ case (sym, n) => (Some(sym), n) } ++
      nodePostGeneratorChildren.iterator.map{ n => (None, n) }).toIndexedSeq
    val mapped = all.map(f.tupled)
    if((all, mapped).zipped.map((a, m) => a._2 eq m).contains(false))
      nodeRebuild(mapped).asInstanceOf[DefNode]
    else this
  }
  def nodeMapGenerators(f: Symbol => Symbol): Node =
    mapOrNone(nodeGenerators.map(_._1), f).fold[Node](this)(s => nodeRebuildWithGenerators(s.toIndexedSeq))
  def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]): Node
}

/** A Node which references Symbols. */
trait RefNode extends Node {
  def nodeReferences: Seq[Symbol]
  def nodeMapReferences(f: Symbol => Symbol): Node
}

trait SimpleRefNode extends RefNode {
  def nodeMapReferences(f: Symbol => Symbol): Node =
    mapOrNone(nodeReferences, f).fold[Node](this)(s => nodeRebuildWithReferences(s.toIndexedSeq))
  def nodeRebuildWithReferences(gen: IndexedSeq[Symbol]): Node
}
