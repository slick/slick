package scala.slick.ast

import OptimizerUtil._
import scala.slick.util.{Logging, RefId}
import scala.collection.mutable.HashMap

/**
 * Basic optimizers for the ScalaQuery AST
 */
object Optimizer extends Logging {

  def apply(n: Node): Node = {
    if(logger.isDebugEnabled) {
      //AnonSymbol.assignNames(n, force = true)
      logger.debug("source:", n)
    }
    val n2 = standard(n)
    if(n2 ne n) logger.debug("optimized:", n2)
    val n3 = Columnizer(n2)
    if(n3 ne n2) logger.debug("columnized:", n3)
    val n4 = assignUniqueSymbols(n3)
    if(logger.isDebugEnabled && (n4 ne n3)) {
      AnonSymbol.assignNames(n4, "u")
      logger.debug("unique symbols:", n4)
    }
    val n5 = RewriteGenerators(n4)
    if(logger.isDebugEnabled && (n5 ne n4)) {
      AnonSymbol.assignNames(n5, "r")
      logger.debug("generators rewritten:", n5)
    }
    n5
  }

  lazy val standard =
    eliminateIndirections andThen
    unwrapGenerators andThen
    reverseProjectionWrapping

  /**
   * Eliminate unnecessary nodes from the AST.
   */
  val eliminateIndirections = new Transformer.Defs {
    def replace = {
      // Remove wrapping of the entire result of a FilteredQuery
      case Wrapped(q @ FilteredQuery(_, from), what) if what == from => q
      // Remove dual wrapping (remnant of a removed Filter(_, ConstColumn(true)))
      case Wrapped(in2, w2 @ Wrapped(in1, _)) if in1 == in2 => w2
      case InRef(in2, i @ InRef(in1, _)) if in1 == in2 => i
      // Remove identity binds
      case Bind(_, x, Pure(y)) if x == y => x
      case Bind(s1, x, Pure(TableRef(s2))) if s1 == s2 => x
      case Bind(s1, x, Pure(Ref(s2))) if s1 == s2 => x
      // Remove unnecessary wrapping of pure values
      case Wrapped(p @ Pure(n1), n2) if n1 == n2 => p
      // Remove unnecessary wrapping of binds
      case Wrapped(b @ Bind(_, _, s1), s2) if s1 == s2 => b
      // Remove unnecessary wrapping of filters in FROM clauses
      case b @ Bind(gen, Wrapped(f: FilteredQuery, _), what) => b.copy(from = f)
      case b @ Bind(gen, Wrapped(f: FilteredJoin, _), what) => b.copy(from = f)
      // Unwrap unions
      case Wrapped(u @ Union(left, _, _, _, _), what) if left == what => u
      // Remove incorrect wrapping caused by multiple uses of the referenced sub-tree
      case InRef(sym1, InRef(sym2, what)) if defs.get(sym1) == defs.get(sym2) => InRef(sym1, what)
      // Back to using ResolvedInRef with virtpatmat in 2.10 perhaps? It's broken in the old patmat
      /*case r @ ResolvedInRef(sym1, _, ResolvedInRef(sym2, _, what)) =>
        println("++++ resolved refs "+sym1+", "+sym2)
        r
      case r @ InRef(sym1, InRef(sym2, what)) =>
        println("++++ unresolved refs "+sym1+", "+sym2)
        println("++++ resolved: "+defs.get(sym1)+", "+defs.get(sym2))
        r*/
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
    def checkSelect(b: Bind, sel: Node): Boolean = {
      if(b.select eq sel) true
      else b.select match {
        case b2: Bind => checkSelect(b2, sel)
        case _ => false
      }
    }
    def replace = {
      case InRef(sym, Wrapped(in, what)) if {
        (defs.get(sym) == Some(RefId(in))) ||
          defs.get(sym).map(_.e match {
            case b: Bind if checkSelect(b, in) => true
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
