package org.scalaquery.ast

import OptimizerUtil._
import org.scalaquery.util.{Logging, RefId}
import org.scalaquery.ql.RawNamedColumn
import scala.collection.mutable.{HashSet, ArrayBuffer, HashMap}

/**
 * Basic optimizers for the ScalaQuery AST
 */
object Optimizer extends Logging {

  lazy val all =
    eliminateIndirections andThen
    unwrapGenerators andThen
    reverseProjectionWrapping /*andThen
    removeFilterRefs*/

  /**
   * Eliminate unnecessary nodes from the AST.
   */
  val eliminateIndirections = new Transformer {
    def replace = {
      // Remove wrapping of the entire result of a FilteredQuery
      case Wrapped(q @ FilteredQuery(_, from), what) if what == from => q
      // Remove dual wrapping (remnant of a removed Filter(_, ConstColumn(true)))
      case Wrapped(in2, w2 @ Wrapped(in1, what)) if in1 == in2 => w2
      // Remove identity binds
      case Bind(_, x, Pure(y)) if x == y => x
      // Remove unnecessary wrapping of pure values
      case Wrapped(p @ Pure(n1), n2) if n1 == n2 => p
      // Remove unnecessary wrapping of binds
      case Wrapped(b @ Bind(_, _, s1), s2) if s1 == s2 => b
      // Remove unnecessary wrapping of filters in FROM clauses
      case b @ Bind(gen, Wrapped(f: FilteredQuery, _), what) => b.copy(from = f)
      case b @ Bind(gen, Wrapped(f: FilteredJoin, _), what) => b.copy(from = f)
      // Unwrap unions
      case Wrapped(u @ Union(left, _, _, _, _), what) if left == what => u
    }
  }

  /**
   * Since Nodes cannot be encoded into Scala Tuples, they have to be encoded
   * into the Tuples' children instead, so we end up with trees like
   * ProductNode(Wrapped(p, x), Wrapped(p, y)). This optimizer rewrites those
   * forms to Wrapped(p, ProductNode(x, y)).
   */
  val reverseProjectionWrapping = new Transformer {
    def allWrapped(in: Node, xs: Seq[Node]) = xs.forall(_ match {
      case Wrapped(in2, _) if in == in2 => true
      case _ => false
    })
    def replace = {
      case p @ ProductNode(Wrapped(in, _), xs @ _*) if allWrapped(in, xs) =>
        val unwrapped = p.nodeChildren.collect { case Wrapped(_, what) => what }
        Wrapped(in, ProductNode(unwrapped: _*))
    }
  }

  /**
   * Remove unnecessary wrappings of generators
   */
  val unwrapGenerators = new Transformer.DefRefsBidirectional {
    val pureParents = new HashMap[RefId[Pure], Bind]
    override def initTree(tree: Node) {
      super.initTree(tree)
      pureParents.clear()
      pureParents ++= tree.collect[(RefId[Pure], Bind)] {
        case b @ Bind(_, _, p @ Pure(_)) => (RefId(p), b)
      }
    }
    def replace = {
      case InRef(sym, Wrapped(in, what)) if {
        (defs.get(sym) == Some(RefId(in))) ||
          defs.get(sym).map(_.e match {
            case Bind(_, _, select) if select eq in => true
            case _ => false
          }).getOrElse(false)
      } => InRef(sym, what)
      case r @ InRef(sym, what) if defs.get(sym) == Some(RefId(what)) => Ref(sym)
      case Wrapped(in, what) if reverse.get(RefId(in)).isDefined => InRef(reverse.get(RefId(in)).get, what)
      case Wrapped(Ref(sym), what) => InRef(sym, what)
      case Wrapped(InRefChain(syms, Ref(sym2)), what) => InRefChain(syms :+ sym2, what)
      //case Wrapped(u @ Union(sym1, _, _, _, _), Ref(sym2)) if sym1 == sym2 => u
      //case Wrapped(f @ FilteredQuery(gen1, from), InRef(gen2, what)) if gen1 == gen2 && from == what => f
      case InRef(sym, what) if (defs.get(sym) match {
        case Some(RefId(FilteredQuery(_, from))) if what == from => true
        case _ => false
      }) => defs(sym).e
      //-- case InRef(sym, RawNamedColumn(name, _)) => Path(sym, FieldSymbol(name))
      // Rewrap from Pure to the enclosing Bind (which can then get resolved to a symbol ref)
      case Wrapped(p: Pure, what) if p.ne(what) && pureParents.contains(RefId(p)) =>
        Wrapped(pureParents(RefId(p)), what)
      case Wrapped(in @ Bind(gen, from, Pure(what)), what2) if what eq what2 => in
    }
  }

  /**
   * Remove unnecessary InRef nestings for filtered queries
   */
  val removeFilterRefs = new Transformer {
    val filterSyms = new HashSet[Symbol]
    override def initTree(n: Node) {
      filterSyms.clear()
      filterSyms ++= n.collect[Symbol] { case FilteredQuery(gen, _) => gen }
    }
    def replace = {
      case InRef(sym1, InRef(sym2, what)) if filterSyms contains sym2 => InRef(sym1, what)
    }
  }

  /**
   * Ensure that all symbol definitions in a tree are unique
   */
  def assignUniqueSymbols(tree: Node): Node = {
    class Scope(val symbol: Symbol, parent: Option[Scope]) extends (Symbol => Symbol) {
      val replacement = new AnonSymbol
      private val local = new HashMap[Symbol, Scope]
      def in(s: Symbol) = local.getOrElseUpdate(s, new Scope(s, Some(this)))
      def find(s: Symbol): Option[Scope] =
        local.get(s).orElse(parent.flatMap(_.find(s)))
      def apply(s: Symbol) = find(s).map(_.replacement).getOrElse(s)
      def dumpString(prefix: String = "", indent: String = "", builder: StringBuilder = new StringBuilder): StringBuilder = {
        builder.append(indent + prefix + symbol + " -> " + replacement + "\n")
        local.foreach { case (_, scope) => scope.dumpString("", indent + "  ", builder) }
        builder
      }
    }
    def buildSymbolTable(n: Node, scope: Scope) {
      n match {
        case d: DefNode =>
          val defs = d.nodeGenerators.toMap
          defs.foreach{ case (sym, ch) => buildSymbolTable(ch, scope.in(sym)) }
          val other = d.nodePostGeneratorChildren.foreach(ch => buildSymbolTable(ch, scope))
        case n => n.nodeChildren.foreach(ch => buildSymbolTable(ch, scope))
      }
    }
    val rootSym = new AnonSymbol
    rootSym.name = "-root-"
    val rootScope = new Scope(rootSym, None)
    buildSymbolTable(tree, rootScope)
    logger.debug("rootScope:\n" + rootScope.dumpString(indent = "  "))
    def tr(n: Node, scope: Scope): Node = n match {
      case d: DefNode =>
        d.nodeMapScopedChildren{ case (symO, ch) =>
          val chScope = symO match {
            case None => scope
            case Some(sym) => scope.find(sym).getOrElse(scope)
          }
          tr(ch, chScope)
        }.asInstanceOf[DefNode].nodeMapGenerators(scope)
      case r @ Ref(s) =>
        val ns = scope(s)
        if(s == ns) r else Ref(ns)
      case r @ TableRef(s) =>
        val ns = scope(s)
        if(s == ns) r else TableRef(ns)
      case i @ InRef(s, what) => scope.find(s) match {
        case None => i
        case Some(refScope) =>
          InRef(refScope.replacement, tr(what, refScope))
      }
      case n => n.nodeMapChildren(ch => tr(ch, scope))
    }
    tr(tree, rootScope)
  }
}
