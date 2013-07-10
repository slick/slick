package scala.slick.ast

import Util._
import scala.collection.mutable.HashMap
import scala.util.DynamicVariable

/** A symbol which can be used in the AST. */
trait Symbol {
  def name: String
  override def toString = SymbolNamer(this)
}

/** A named symbol which refers to an (aliased or unaliased) field. */
case class FieldSymbol(name: String)(val options: Seq[ColumnOption[_]], val tpe: Type) extends Symbol with Typed

/** An element of a ProductNode (using a 1-based index) */
case class ElementSymbol(idx: Int) extends Symbol {
  def name = "_" + idx
}

/** The symbol of a nominal type */
trait TypeSymbol extends Symbol

/** A TypeSymbol which uniquely identifies a table type */
trait TableIdentitySymbol extends TypeSymbol

/** Default implementation of TableIdentitySymbol */
case class SimpleTableIdentitySymbol(constituents: AnyRef*) extends TableIdentitySymbol {
  def name = constituents.mkString("@(", ".", ")")
}

/** An anonymous symbol defined in the AST. */
class AnonTypeSymbol extends TypeSymbol {
  def name = "$@"+System.identityHashCode(this)
}

object AnonTypeSymbol {
  def unapply(a: AnonTypeSymbol) = Some(a.name)
}

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

/** A Node which introduces Symbols. */
trait DefNode extends Node {
  def nodeGenerators: Seq[(Symbol, Node)]
  def nodePostGeneratorChildren: Seq[Node] =
    nodeChildren.drop(nodeGenerators.length)
  protected[this] def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]): Node

  final def nodeMapScopedChildren(f: (Option[Symbol], Node) => Node): Self with DefNode = {
    val all = (nodeGenerators.iterator.map{ case (sym, n) => (Some(sym), n) } ++
      nodePostGeneratorChildren.iterator.map{ n => (None, n) }).toIndexedSeq
    val mapped = all.map(f.tupled)
    if((all, mapped).zipped.map((a, m) => a._2 eq m).contains(false)) nodeRebuild(mapped).asInstanceOf[Self with DefNode]
    else this
  }
  final def nodeMapGenerators(f: Symbol => Symbol): Node =
    mapOrNone(nodeGenerators.map(_._1))(f).fold[Node](this) { s => nodeRebuildWithGenerators(s.toIndexedSeq) }
}

/** A Node which references a Symbol. */
trait RefNode extends Node {
  def nodeReference: Symbol
  final def nodeWithReference(s: Symbol): RefNode =
    if(s eq nodeReference) this else nodeRebuildWithReference(s)
  protected[this] def nodeRebuildWithReference(s: Symbol): RefNode
}

object RefNode {
  def unapply(r: RefNode) = Some(r.nodeReference)
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
