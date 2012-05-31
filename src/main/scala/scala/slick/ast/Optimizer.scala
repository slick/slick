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

  /** Replace GlobalSymbols by AnonSymbols and collect them in a LetDynamic */
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
    val tree2 = tr.once(tree)
    while(!newNodes.isEmpty) {
      val m = newNodes.toMap
      newNodes.clear()
      m.foreach { case (sym, n) => map += sym -> tr.once(n) }
    }
    if(map.isEmpty) tree2 else LetDynamic(map.toSeq, tree2)
  }

  /**
   * Since Nodes cannot be encoded into Scala Tuples, they have to be encoded
   * into the Tuples' children instead, so we end up with trees like
   * ProductNode(ProductElement(1, r), ProductElement(2, r)) where r is a Ref
   * to an expression that yields a Query of ProductNode(_, _). This optimizer
   * rewrites those forms to the raw Ref r.
   */
  def reconstructProducts(tree: Node): Node = {
    // Find the common reference in a candidate node
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
    def shapeFor(n: Node, expect: List[Int] = Nil): Option[PShape] = n match {
      case ProductNode(ch @ _*) =>
        val chsh = ch.zipWithIndex.map { case (c, i) => shapeFor(c, (i+1) :: expect) }
        if(chsh.contains(None)) None else Some(PProduct(chsh.map(_.get)))
      case ProductElements(idxs, _) =>
        if(idxs == expect) Some(PLeaf) else None
    }
    // Check if the shape matches the ref target
    def shapeMatches(s: PShape, t: Node): Boolean = (s, t) match {
      case (PLeaf, _) => true
      case (PProduct(pch), ProductNode(nch @ _*)) if(pch.length == nch.length) =>
        (pch, nch).zipped.map(shapeMatches).forall(identity)
      case _ => false
    }
    // Replace matching products
    (new Transformer.Defs {
      // Narrow the target of a ref to the actual Pure value produced
      def narrow(n: Node): Option[Pure] = n match {
        case p: Pure => Some(p)
        case FilteredQuery(_, from) => narrow(from)
        case Bind(_, _, select) => narrow(select)
        case Ref(Def(n)) => narrow(n)
        case _ => None
      }
      def replace = {
        case p: ProductNode =>
          (for {
            sym <- findCommonRef(p)
            target <- {
              logger.debug("Found ProductNode with common ref "+sym)
              defs.get(sym)
            }
            ntarget <- {
              logger.debug("  Ref target: "+target)
              narrow(target)
            }
            shape <- {
              logger.debug("  Narrowed target: "+ntarget)
              shapeFor(p)
            }
          } yield {
            logger.debug("  Shape: "+shape)
            if(shapeMatches(shape, ntarget.value)) {
              logger.debug("  Shape matches")
              Ref(sym)
            } else p
          }).getOrElse(p)
      }
    }).repeat(tree)
  }

  /**
   * Inline references to global symbols which occur only once in a Ref node.
   *
   * We also remove identity Binds here to avoid an extra pass just for that.
   * TODO: Necessary? The conversion to relational trees should inline them anyway.
   */
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
      case LetDynamic(defs, in) => (in, defs.toMap)
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
          tr.once(globals(a))
        // Remove identity Bind
        case Bind(gen, from, Pure(Ref(sym))) if gen == sym => from
      }
    }
    val tree3 = tr.once(tree2)
    val globalsLeft = globals.filterKeys(a => !inlined.contains(a))
    if(globalsLeft.isEmpty) tree3
    else LetDynamic(globalsLeft.iterator.map{ case (sym, n) => (sym, tr.once(n)) }.toSeq, tree3)
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
