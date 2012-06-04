package scala.slick.ast
package opt

import scala.slick.util.Logging
import scala.collection.mutable.HashMap
import Util._

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
    val n3 = ReconstructProducts(n2)
    if(n3 ne n2) logger.debug("products reconstructed:", n3)
    val n4 = (new Inliner)(n3)
    if(n4 ne n3) logger.debug("refs inlined:", n4)
    val n5 = assignUniqueSymbols(n4)
    if(n5 ne n4) logger.debug("unique symbols:", n5)
    n5
  }

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
