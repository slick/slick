package scala.slick.ast

import OptimizerUtil._
import scala.slick.ql.RawNamedColumn
import scala.slick.util.WeakIdentityHashMap
import java.lang.ref.WeakReference

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
  def assignNames(tree: Node, prefix: String = "s", force: Boolean = false, allRefs: Boolean = false, start: Int = 0): Int = {
    var num = start
    val symName = memoized[AnonSymbol, String](_ => { s => num += 1; prefix + num })
    tree.foreach {
      case d : DefNode => d.nodeGenerators.foreach {
        case (s: AnonSymbol, _) if force || !s.hasName => s.name = symName(s)
        case _ =>
      }
      case r: RefNode if allRefs => r.nodeReferences.foreach {
        case s: AnonSymbol if force || !s.hasName => s.name = symName(s)
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

class GlobalSymbol(val target: Node) extends AnonSymbol {
  override def toString = "/" + name
}

object GlobalSymbol {
  def unapply(g: GlobalSymbol) = Some(g.name, g.target)

  // Keep symbols around as long as the Node and the existing Symbol are
  // reachable. We can safely create a new Symbol when no references are left
  // to the old one. We must not keep hard references to Symbols, so that
  // GlobalSymbol#target will not prevent Nodes from being garbage-collected.
  private val symbols = new WeakIdentityHashMap[Node, WeakReference[GlobalSymbol]]

  private def newSym(n: Node): GlobalSymbol = {
    val sym = new GlobalSymbol(n)
    symbols.update(n, new WeakReference(sym))
    sym
  }

  /** Return the GlobalSymbol for the given Node */
  def forNode(n: Node): GlobalSymbol = symbols.synchronized {
    val g: GlobalSymbol = symbols.get(n) match {
      case Some(wr) =>
        val sym = wr.get()
        if(sym eq null) newSym(n) else sym
      case None => newSym(n)
    }
    g
  }
}

/** An expression introduced by a Symbol, wrapped in a reference to the Symbol. */
case class InRef(sym: Symbol, child: Node) extends UnaryNode with RefNode {
  protected[this] def nodeRebuild(child: Node) = copy(child = child)
  override def nodeChildNames = Seq("value")
  def nodeReferences = Seq(sym)
  def nodeMapReferences(f: Symbol => Symbol) = copy(sym = f(sym))
}

/** A reference to a Symbol */
case class Ref(sym: Symbol) extends NullaryNode with RefNode {
  def nodeReferences = Seq(sym)
  def nodeMapReferences(f: Symbol => Symbol) = copy(sym = f(sym))
}

/** A reference to a Symbol pointing to a table which should not be expanded */
case class TableRef(sym: Symbol) extends NullaryNode with RefNode {
  def nodeReferences = Seq(sym)
  def nodeMapReferences(f: Symbol => Symbol) = copy(sym = f(sym))
}

/** A reference to a field in a struct */
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

/** A Node which introduces Symbols. */
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

/** A Node which references Symbols. */
trait RefNode extends Node {
  def nodeReferences: Seq[Symbol]
  def nodeMapReferences(f: Symbol => Symbol): Node
}
