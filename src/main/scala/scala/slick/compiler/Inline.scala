package scala.slick.compiler

import scala.collection.mutable.{HashSet, HashMap}
import scala.slick.ast._
import Util._

/**
 * Inline references to global symbols which occur only once in a Ref node.
 * Paths are always inlined, no matter how many times they occur.
 * Symbols used in a FROM position are always inlined.
 *
 * Inlining behaviour can be controlled with optional parameters.
 *
 * We also remove identity Binds here to avoid an extra pass just for that.
 * TODO: Necessary? The conversion to relational trees should inline them anyway.
 */
class Inline(unique: Boolean = true, paths: Boolean = true, from: Boolean = true, all: Boolean = false) extends Phase {
  val name = "inline"

  def apply(tree: Node, state: CompilationState): Node = {
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
    val globalCounts = counts.filterKeys(globals.contains)
    val toInlineAll = globalCounts.iterator.map(_._1).toSet
    logger.debug("symbols to inline in FROM positions: "+toInlineAll)
    val toInline = globalCounts.iterator.filter { case (a, i) =>
      all ||
        (unique && i == 1) ||
        (paths && Path.unapply(globals(a)).isDefined) ||
        /* This happens when referencing a raw TableNode from multiple Selects.
           We inline it here to make the subsequent Select(TableNode, _) fails */
        globals(a).isInstanceOf[TableNode]
    }.map(_._1).toSet
    logger.debug("symbols to inline everywhere: "+toInline)
    val inlined = new HashSet[Symbol]
    def deref(a: AnonSymbol) = { inlined += a; globals(a) }
    def tr(n: Node): Node = n match {
      case f @ FilteredQuery(_, Ref(a: AnonSymbol)) if (all || from) && toInlineAll.contains(a) =>
        tr(f.nodeMapFrom(_ => deref(a)))
      case b @ Bind(_, Ref(a: AnonSymbol), _) if (all || from) && toInlineAll.contains(a) =>
        tr(b.copy(from = deref(a)))
      case j @ Join(_, _, left, right, _, _) if(all || from) =>
        val l = left match {
          case Ref(a: AnonSymbol) if toInlineAll.contains(a) => deref(a)
          case x => x
        }
        val r = right match {
          case Ref(a: AnonSymbol) if toInlineAll.contains(a) => deref(a)
          case x => x
        }
        if((l eq left) && (r eq right)) j.nodeMapChildren(tr) else tr(j.copy(left = l, right = r))
      case Ref(a: AnonSymbol) if toInline.contains(a) => tr(deref(a))
      // Remove identity Bind
      case Bind(gen, from, Pure(Ref(sym))) if gen == sym => tr(from)
      case n => n.nodeMapChildren(tr)
    }
    val tree3 = tr(tree2)
    val globalsLeft = globals.filterKeys(a => !inlined.contains(a))
    if(globalsLeft.isEmpty) tree3
    else LetDynamic(globalsLeft.iterator.map{ case (sym, n) => (sym, tr(n)) }.toSeq, tree3)
  }
}
