package scala.slick.ast

import OptimizerUtil._
import scala.slick.util.{Logging, RefId}
import scala.collection.mutable.{HashSet, HashMap}

/**
 * Basic optimizers for the SLICK AST
 */
object Optimizer extends Logging {

  def apply(n: Node): Node = {
    if(logger.isDebugEnabled) {
      logger.debug("source:", n)
    }
    val n2 = localizeRefs(n)
    if(logger.isDebugEnabled) {
      AnonSymbol.assignNames(n2, prefix = "s", force = true)
      if(n2 ne n) logger.debug("localized refs:", n2)
    }
    val n3 = reconstructProducts(n2)
    if(n3 ne n2) logger.debug("products reconstructed:", n3)
    val n4 = inlineUniqueRefs(n3)
    if(n4 ne n3) logger.debug("unique refs inlined:", n4)
    n4
  }

  /*def apply(n: Node): Node = {
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
      logger.debug("unique symbols:", n4)
    }
    val n5 = RewriteGenerators(n4)
    if(logger.isDebugEnabled && (n5 ne n4)) {
      AnonSymbol.assignNames(n5, "r")
      logger.debug("generators rewritten:", n5)
    }
    n5
  }*/

  /** Replace GlobalSymbols by AnonSymbols and collect them in a DynamicLet */
  def localizeRefs(tree: Node): Node = {
    val map = new HashMap[AnonSymbol, Node]
    val newNodes = new HashMap[AnonSymbol, Node]
    val tr = new Transformer {
      def replace = {
        case r: RefNode => r.nodeMapReferences {
          case GlobalSymbol(name, target) =>
            val s = new AnonSymbol
            map += s -> target
            newNodes += s -> target
            s
          case s => s
        }
      }
    }
    val tree2 = tr.applyOnce(tree)
    while(!newNodes.isEmpty) {
      val m = newNodes.toMap
      newNodes.clear()
      m.foreach { case (sym, n) => map += sym -> tr.applyOnce(n) }
    }
    if(map.isEmpty) tree2 else DynamicLet(map.toSeq, tree2)
  }

  /**
   * Since Nodes cannot be encoded into Scala Tuples, they have to be encoded
   * into the Tuples' children instead, so we end up with trees like
   * ProductNode(ProductElement(1, r), ProductElement(2, r)) where r is a Ref
   * to an expression that yields a Query of ProductNode(_, _). This optimizer
   * rewrites those forms to the raw Ref r.
   */
  def reconstructProducts(tree: Node): Node = {
    def findCommonRef(n: Node): Option[AnonSymbol] = n match {
      case Ref(sym: AnonSymbol) => Some(sym)
      case ProductElement(in, _) => findCommonRef(in)
      case ProductNode(ch @ _*) =>
        val chref = ch.map(findCommonRef)
        if(chref.contains(None)) None
        else {
          val refs = chref.map(_.get).toSet
          if(refs.size == 1) Some(refs.head)
          else None
        }
      case _ => None
    }
    def shapeMatches(p: ProductNode, t: Node): Boolean = {
      true//--
    }
    val tr = new Transformer.Defs {
      def replace = {
        case p: ProductNode =>
          (for {
            sym <- findCommonRef(p)
            target <- defs.get(sym)
          } yield {
            logger.debug("Target for common ref "+sym+": "+target)
            if(shapeMatches(p, target)) Ref(sym) else p
          }).getOrElse(p)
      }
    }
    tr.applyOnce(tree)

    /*
    // ADT for representing shapes of ProductNodes
    sealed abstract class PShape
    case object PLeaf extends PShape
    case class PProduct(children: Seq[PShape]) extends PShape
    // Extractor for ProductElement chains
    object ProductElements {
      def unapply(n: Node): Option[(List[Int], Symbol)] = n match {
        case Ref(sym) => Some((Nil, sym))
        case ProductElement(in, idx) => unapply(in) match {
          case None => None
          case Some((l, sym)) => Some((idx :: l, sym))
        }
      }
    }
    // Find the shape and common ref of a product if it exists
    def shape(n: Node): Option[(PShape, Ref)] = n match {
      case ProductNode(ch @ _*) =>
      case
    }
    def dropElemRefs(n: Node, drop: List[Int]): Option[Node] = n match {
      case ProductNode(ch @ _*) =>
        ch.zipWithIndex.map { case (c, i) => dropElemRefs(c, (i+1) :: drop) }
    }*/
  }

  /** Inline references to global symbols which occur only once in a Ref node */
  def inlineUniqueRefs(tree: Node): (Node) = {
    val counts = new HashMap[AnonSymbol, Int]
    tree.foreach {
      case r: RefNode => r.nodeReferences.foreach {
        case a: AnonSymbol =>
          counts += a -> (counts.getOrElse(a, 0) + 1)
        case s =>
      }
      case _ =>
    }
    val (tree2, globals) = tree match {
      case DynamicLet(defs, in) => (in, defs.toMap)
      case n => (n, Map[Symbol, Node]())
    }
    logger.debug("counts: "+counts)
    val toInline = counts.iterator.filter{ case (a, i) => i == 1 && globals.contains(a) }.map(_._1).toSet
    logger.debug("symbols to inline: "+toInline)
    val inlined = new HashSet[Symbol]
    lazy val tr: Transformer = new Transformer {
      def replace = {
        case Ref(a: AnonSymbol) if toInline.contains(a) =>
          inlined += a
          tr.applyOnce(globals(a))
      }
    }
    val tree3 = tr.applyOnce(tree2)
    val globalsLeft = globals.filterKeys(a => !inlined.contains(a))
    if(globalsLeft.isEmpty) tree3
    else DynamicLet(globalsLeft.iterator.map{ case (sym, n) => (sym, tr.applyOnce(n)) }.toSeq, tree3)
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
      // Remove indirections through global symbols
      case Ref(GlobalSymbol(_, r: Ref)) => r
      // Remove wrapping of the entire result of a FilteredQuery
      case InRef(GlobalSymbol(_, q @ FilteredQuery(_, from)), what) if what == from => q
      // Remove dual wrapping (remnant of a removed Filter(_, ConstColumn(true)))
      case InRef(GlobalSymbol(_, in2), w2 @ InRef(GlobalSymbol(_, in1), _)) if in1 == in2 => w2
      case InRef(in2, i @ InRef(in1, _)) if in1 == in2 => i
      // Remove identity binds
      case Bind(_, x, Pure(y)) if x == y => x
      case Bind(s1, x, Pure(TableRef(s2))) if s1 == s2 => x
      case Bind(s1, x, Pure(Ref(s2))) if s1 == s2 => x
      // Remove unnecessary wrapping of pure values
      case InRef(GlobalSymbol(_, p @ Pure(n1)), n2) if n1 == n2 => p
      // Remove unnecessary wrapping of binds
      case InRef(GlobalSymbol(_, b @ Bind(_, _, s1)), s2) if s1 == s2 => b
      // Remove unnecessary wrapping of filters in FROM clauses
      case b @ Bind(gen, InRef(GlobalSymbol(_, f: FilteredQuery), _), what) => b.copy(from = f)
      case b @ Bind(gen, InRef(GlobalSymbol(_, f: FilteredJoin), _), what) => b.copy(from = f)
      // Unwrap unions
      case InRef(GlobalSymbol(_, u @ Union(left, _, _, _, _)), what) if left == what => u
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
    def allFrom(sym: Symbol, xs: Seq[Node]) = xs.forall(_ match {
      case InRef(sym2, _) if sym == sym2 => true
      case _ => false
    })
    def replace = {
      case p @ ProductNode(InRef(sym: GlobalSymbol, _), xs @ _*) if allFrom(sym, xs) =>
        val unwrapped = p.nodeChildren.collect { case InRef(_, what) => what }
        InRef(sym, ProductNode(unwrapped: _*))
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
      case InRef(sym, InRef(GlobalSymbol(_, in), what)) if {
        (defs.get(sym) == Some(RefId(in))) ||
          defs.get(sym).map(_.e match {
            case b: Bind if checkSelect(b, in) => true
            case _ => false
          }).getOrElse(false)
      } => InRef(sym, what)
      case r @ InRef(sym, what) if defs.get(sym) == Some(RefId(what)) => Ref(sym)
      case InRef(GlobalSymbol(_, in), what) if reverse.get(RefId(in)).isDefined => InRef(reverse.get(RefId(in)).get, what)
      case InRef(GlobalSymbol(_, Ref(sym)), what) => InRef(sym, what)
      case InRef(GlobalSymbol(_, InRefChain(syms, Ref(sym2))), what) => InRefChain(syms :+ sym2, what)
      case InRef(sym, what) if (defs.get(sym) match {
        case Some(RefId(FilteredQuery(_, from))) if what == from => true
        case _ => false
      }) => defs(sym).e
      // Rewrap from Pure to the enclosing Bind (which can then get resolved to a symbol ref)
      case InRef(GlobalSymbol(_, p: Pure), what) if p.ne(what) && pureParents.contains(RefId(p)) =>
        InRef(GlobalSymbol.forNode(pureParents(RefId(p))), what)
      case InRef(GlobalSymbol(_, in @ Bind(gen, from, Pure(what))), what2) if what eq what2 => in
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
    val res = tr(tree, rootScope)
    if(logger.isDebugEnabled && (res ne tree)) {
      AnonSymbol.assignNames(res, "u")
      logger.debug("rootScope:\n" + rootScope.dumpString(indent = "  "))
    }
    res
  }
}
