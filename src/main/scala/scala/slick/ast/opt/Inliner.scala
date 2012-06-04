package scala.slick.ast
package opt

import scala.slick.util.Logging
import scala.collection.mutable.{HashSet, HashMap}
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
class Inliner(unique: Boolean = true, paths: Boolean = true, from: Boolean = true, all: Boolean = false) extends (Node => Node) with Logging {

  def apply(tree: Node): Node = {
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
        (paths && Path.unapply(globals(a)).isDefined)
    }.map(_._1).toSet
    logger.debug("symbols to inline everywhere: "+toInline)
    val inlined = new HashSet[Symbol]
    def deref(a: AnonSymbol) = { inlined += a; globals(a) }
    lazy val tr: Transformer = new Transformer {
      def replace = {
        case f @ FilteredQuery(_, Ref(a: AnonSymbol)) if (all || from) && toInlineAll.contains(a) =>
          tr.once(f.nodeMapFrom(_ => deref(a)))
        case b @ Bind(_, Ref(a: AnonSymbol), _) if (all || from) && toInlineAll.contains(a) =>
          tr.once(b.copy(from = deref(a)))
        case j: JoinNode if(all || from) =>
          val l = j.left match {
            case Ref(a: AnonSymbol) if toInlineAll.contains(a) => deref(a)
            case x => x
          }
          val r = j.right match {
            case Ref(a: AnonSymbol) if toInlineAll.contains(a) => deref(a)
            case x => x
          }
          if((l eq j.left) && (r eq j.right)) j else tr.once(j.nodeCopyJoin(left = l, right = r))
        case Ref(a: AnonSymbol) if toInline.contains(a) =>
          tr.once(deref(a))
        // Remove identity Bind
        case Bind(gen, from, Pure(Ref(sym))) if gen == sym => tr.once(from)
      }
    }
    val tree3 = tr.once(tree2)
    val globalsLeft = globals.filterKeys(a => !inlined.contains(a))
    if(globalsLeft.isEmpty) tree3
    else LetDynamic(globalsLeft.iterator.map{ case (sym, n) => (sym, tr.once(n)) }.toSeq, tree3)
  }
}
