package scala.slick.ast

import Util._
import scala.slick.lifted.{TypeMapper, ColumnOption}
import scala.collection.mutable.HashMap
import scala.util.DynamicVariable

/** A symbol which can be used in the AST. */
trait Symbol {
  def name: String
  override def toString = SymbolNamer(this)
}

/** A named symbol which refers to an (aliased or unaliased) field. */
case class FieldSymbol(name: String)(val options: Seq[ColumnOption[_]], val typeMapper: TypeMapper[_]) extends Symbol

/** An element of a ProductNode */
case class ElementSymbol(idx: Int) extends Symbol {
  def name = "_" + idx
}

/** A named symbol which refers to a proper database table. */
case class TableSymbol(name: String) extends Symbol

/** An anonymous symbol defined in the AST. */
class AnonSymbol extends Symbol {
  def name = "@"+System.identityHashCode(this)
}

object AnonSymbol {
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

/** Provides names for symbols */
class SymbolNamer(symbolPrefix: String, parent: Option[SymbolNamer] = None) {
  private var curSymbolId = 1
  private val map = new HashMap[Symbol, String]

  def create = {
    curSymbolId += 1
    symbolPrefix + curSymbolId
  }

  def get(s: Symbol): Option[String] =
    map.get(s) orElse parent.flatMap(_.get(s))

  def apply(s: Symbol): String = get(s).getOrElse(s match {
    case a: AnonSymbol =>
      val n = create
      update(a, n)
      n
    case s => namedSymbolName(s)
  })

  def namedSymbolName(s: Symbol) = s.name

  def update(s: Symbol, n: String): Unit = map += s -> n

  def use[T](f: => T): T = SymbolNamer.dyn.withValue(this)(f)
}

object SymbolNamer {
  private val dyn = new DynamicVariable[SymbolNamer](null)
  def apply(s: Symbol): String = {
    val n = dyn.value
    if(n eq null) s.name else n(s)
  }
}
