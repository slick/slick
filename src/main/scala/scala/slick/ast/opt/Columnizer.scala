package scala.slick.ast
package opt

import Util._
import scala.slick.util.Logging
import scala.slick.ast.WithOp
import scala.collection.mutable.HashMap

/**
 * Expand columns in queries
 */
object Columnizer extends (Node => Node) with Logging {

  def apply(tree: Node): Node = {
    val t1 = forceOuterBind(tree)
    if(t1 ne tree) logger.debug("With outer binds:", t1)
    val t2 = expandTables(t1)
    if(t2 ne t1) logger.debug("Tables expanded:", t2)
    val t3 = expandRefs(t2)
    if(t3 ne t2) logger.debug("Refs expanded:", t3)
    val t4 = replaceFieldSymbols(t3)
    if(t4 ne t3) logger.debug("FieldSymbols replaced:", t4)
    t4
  }

  /** Ensure that all collection operations are wrapped in a Bind so that we
    * have a place for expanding references later. */
  def forceOuterBind(n: Node): Node = {
    def idBind(n: Node) = {
      val gen = new AnonSymbol
      logger.debug("Introducing new Bind "+gen)
      Bind(gen, n, Ref(gen))
    }
    def wrap(n: Node): Node = n match {
      case b: Bind => b.nodeMapChildren(nowrap)
      case n => idBind(n.nodeMapChildren(nowrap))
    }
    def nowrap(n: Node): Node = n match {
      case j: Join => j.nodeMapChildren { ch =>
        if((ch eq j.left) || (ch eq j.right)) nowrap(ch) else maybewrap(ch)
      }
      case u: Union => u.nodeMapChildren(nowrap)
      case f: FilteredQuery => f.nodeMapChildren { ch =>
        if(ch eq f.from) nowrap(ch) else maybewrap(ch)
      }
      case b: Bind => b.nodeMapChildren(nowrap)
      case n => n.nodeMapChildren(maybewrap)
    }
    def maybewrap(n: Node): Node = n match {
      case j: Join => wrap(n)
      case u: Union => wrap(n)
      case f: FilteredQuery => wrap(n)
      case n => nowrap(n)
    }
    wrap(n)
  }

  /** Replace all TableNodes with TableExpansions which contain both the
    * expansion and the original table */
  def expandTables(n: Node): Node = n match {
    case t: TableExpansion => t
    case t: TableNode =>
      val sym = new AnonSymbol
      val expanded = WithOp.encodeRef(t, sym).nodeShaped_*.packedNode
      val processed = expandTables(Optimizer.prepareTree(expanded, true))
      TableExpansion(sym, t, ProductNode(processed.flattenProduct: _*))
    case n => n.nodeMapChildren(expandTables)
  }

  /** Expand Paths to ProductNodes and TableExpansions into ProductNodes of
    * Paths, so that all Paths point to individual columns by index */
  def expandRefs(n: Node, scope: Scope = Scope.empty): Node = n match {
    case p @ PathOrRef(psyms) =>
      psyms.head match {
        case f: FieldSymbol => p
        case _ =>
        val syms = psyms.reverse
        scope.get(syms.head) match {
          case Some((Structure(ntarget), _)) =>
            select(syms.tail, ntarget) match {
              case t: TableExpansion =>
                logger.debug("Narrowed "+p+" to "+t)
                burstPath(Ref(syms.head), syms.tail, t)
              case pr: ProductNode =>
                logger.debug("Narrowed "+p+" to "+pr)
                burstPath(Ref(syms.head), syms.tail, pr)
              case _ => p
            }
          case None => p
        }
      }
    case n => n.mapChildrenWithScope(expandRefs, scope)
  }

  /** Expand a path of selects into a given target on top of a base node */
  def burstPath(base: Node, selects: List[Symbol], target: Node): Node = target match {
    case ProductNode(ch @ _*) =>
      ProductNode(ch.zipWithIndex.map { case (n, idx) =>
        burstPath(Select(base, ElementSymbol(idx+1)), selects, n)
      }: _*)
    case TableExpansion(gen, t, cols) =>
      ProductNode(cols.nodeChildren.zipWithIndex.map { case (n, idx) =>
        burstPath(Select(base, ElementSymbol(idx+1)), selects, n)
      }: _*)
    case _ => selects.foldLeft(base){ case (z,sym) => Select(z, sym) }
  }

  /** Replace references to FieldSymbols in TableExpansions by the
    * appropriate ElementSymbol */
  def replaceFieldSymbols(n: Node): Node = {
    val updatedTables = new HashMap[Symbol, ProductNode]
    def tr(n: Node, scope: Scope = Scope.empty): Node = n match {
      case sel @ Select(p @ PathOrRef(psyms), field: FieldSymbol) =>
        val syms = psyms.reverse
        scope.get(syms.head) match {
          case Some((Structure(ntarget), _)) =>
            select(syms.tail, ntarget) match {
              case t: TableExpansion =>
                logger.debug("Narrowed "+p+"."+field+" to "+t)
                val columns: ProductNode = updatedTables.get(t.generator).getOrElse(t.columns.asInstanceOf[ProductNode])
                val needed = Select(Ref(t.generator), field)
                val newSel = columns.nodeChildren.zipWithIndex.find(needed == _._1) match {
                  case Some((_, idx)) => Select(p, ElementSymbol(idx+1))
                  case None =>
                    val col = Select(Ref(t.generator), field)
                    updatedTables += t.generator -> ProductNode((columns.nodeChildren :+ col): _*)
                    Select(p, ElementSymbol(columns.nodeChildren.size + 1))
                }
                logger.debug("Replaced "+sel+" by "+newSel)
                newSel
              case _ => // a Table within a TableExpansion -> don't rewrite
                sel
            }
          case None => sel
        }
      case n => n.mapChildrenWithScope(tr, scope)
    }
    val n2 = tr(n)
    val n3 = if(!updatedTables.isEmpty) {
      logger.debug("Patching "+updatedTables.size+" updated TableExpansion(s) "+updatedTables.keysIterator.mkString(", ")+" into the tree")
      def update(n: Node): Node = n match {
        case t: TableExpansion =>
          updatedTables.get(t.generator).fold(t)(c => t.copy(columns = c)).nodeMapChildren(update)
        case n => n.nodeMapChildren(update)
      }
      update(n2)
    } else n2
    n3
  }

  /** Navigate into ProductNodes along a path */
  def select(selects: List[Symbol], base: Node): Node = (selects, base) match {
    case (Nil, n) => n
    case ((s: ElementSymbol) :: t, ProductNode(ch @ _*)) => select(t, ch(s.idx-1))
  }

  /** Extractor that finds the actual structure produced by a Node */
  object Structure {
    def unapply(n: Node): Option[Node] = n match {
      case Pure(n) => Some(n)
      case Join(_, _, l, r, _, _) =>
        for { l <- unapply(l); r <- unapply(r) } yield ProductNode(l, r)
      case Union(l, _, _, _, _) => unapply(l)
      case FilteredQuery(_, from) => unapply(from)
      case Bind(_, _, select) => unapply(select)
      case t: TableExpansion => Some(t)
      case n => Some(n)
    }
  }
}
