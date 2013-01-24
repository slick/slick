package scala.slick.compiler

import scala.collection.mutable.HashMap
import scala.slick.SlickException
import scala.slick.ast._
import Util._

/** Replace references to FieldSymbols in TableExpansions by the
  * appropriate ElementSymbol */
class ReplaceFieldSymbols extends Phase with ColumnizerUtils {
  val name = "replaceFieldSymbols"

  def apply(state: CompilerState) = state.map { n =>
    ClientSideOp.mapServerSide(n)(applyServerSide)
  }

  def applyServerSide(n: Node) = {
    val updatedTables = new HashMap[Symbol, ProductNode]
    val seenDefs = new HashMap[Symbol, Node]

    def rewrite(target: Node, p: Node, field: FieldSymbol, syms: List[Symbol], tpe: Type): Option[Select] = {
      val ntarget = narrowStructure(target)
      logger.debug("Narrowed to structure "+ntarget+" with tail "+Path.toString(syms.tail.reverse))
      select(syms.tail, ntarget, seenDefs.get _).map {
        case t: TableExpansion =>
          logger.debug("Narrowed to element "+t)
          val columns: ProductNode = updatedTables.get(t.generator).getOrElse(t.columns.asInstanceOf[ProductNode])
          val needed = Select(Ref(t.generator), field).nodeTyped(tpe)
          Some(columns.nodeChildren.zipWithIndex.find(needed == _._1) match {
            case Some((_, idx)) => Select(p, ElementSymbol(idx+1))
            case None =>
              updatedTables += t.generator -> ProductNode((columns.nodeChildren :+ needed))
              Select(p, ElementSymbol(columns.nodeChildren.size + 1))
          })
        case t: TableRefExpansion =>
          logger.debug("Narrowed to element "+t)
          Path.unapply(t.ref).flatMap { psyms =>
            val syms = psyms.reverse
            logger.debug("Looking for seen def "+syms.head)
            seenDefs.get(syms.head).flatMap { n =>
              logger.debug("Trying to rewrite recursive match "+t.ref+" ."+field)
              rewrite(n, t.ref, field, syms, tpe).map { recSel =>
                logger.debug("Found recursive replacement "+recSel.in+" ."+recSel.field)
                val columns: ProductNode = updatedTables.get(t.marker).getOrElse(t.columns.asInstanceOf[ProductNode])
                val needed = Select(t.ref, recSel.field).nodeTyped(tpe)
                columns.nodeChildren.zipWithIndex.find(needed == _._1) match {
                  case Some((_, idx)) => Select(p, ElementSymbol(idx+1))
                  case None =>
                    updatedTables += t.marker -> ProductNode((columns.nodeChildren :+ needed))
                    Select(p, ElementSymbol(columns.nodeChildren.size + 1))
                }
              }
            }
          }
        case _: TableNode =>
          None // A table within a TableExpansion -> don't rewrite
        case Path(psyms) =>
          logger.debug("Narrowed to "+Path.toString(psyms))
          val syms = psyms.reverse
          seenDefs.get(syms.head).flatMap { n =>
            logger.debug("Trying to rewrite target "+p+" ."+field)
            rewrite(n, p, field, syms, tpe)
          }
        case n =>
          throw new SlickException("Unexpected target node "+n+" (from "+Path.toString(syms.reverse)+")")
          None
      }.head // we have to assume that the structure is the same for all Union expansions
    }

    def tr(n: Node, scope: Scope = Scope.empty): Node = n match {
      case d: DefNode =>
        val r = d.mapChildrenWithScope({ (symO, ch, chscope) =>
          val ch2 = tr(ch, chscope)
          symO.foreach { sym => seenDefs += sym -> ch2 }
          ch2
        }, scope)
        r
      case sel @ Select(p @ Path(psyms), field: FieldSymbol) =>
        val syms = psyms.reverse
        scope.get(syms.head).flatMap { case (n, _) =>
          logger.debug(s"Trying to rewrite $p .$field : ${sel.nodeType}")
          val newSelO = rewrite(n, p, field, syms, sel.nodeType)
          newSelO.foreach(newSel => logger.debug("Replaced "+Path.toString(sel)+" by "+Path.toString(newSel)))
          newSelO
        }.getOrElse(sel)
      case n => n.mapChildrenWithScope(((_, ch, chsc) => tr(ch, chsc)), scope)
    }

    val n2 = tr(n)
    val n3 = if(!updatedTables.isEmpty) {
      logger.debug("Patching "+updatedTables.size+" updated Table(Ref)Expansion(s) "+updatedTables.keysIterator.mkString(", ")+" into the tree")
      for((sym, n) <- updatedTables) logger.debug("Updated expansion: "+sym, n)
      def update(n: Node): Node = n match {
        case t: TableExpansion =>
          updatedTables.get(t.generator).fold(t)(c => t.copy(columns = c)).nodeMapChildren(update)
        case t: TableRefExpansion =>
          updatedTables.get(t.marker).fold(t)(c => t.copy(columns = c)).nodeMapChildren(update)
        case n => n.nodeMapChildren(update)
      }
      update(n2)
    } else n2
    n3
  }
}

/** Replace all TableNodes with TableExpansions which contain both the
  * expansion and the original table. */
class ExpandTables extends Phase {
  val name = "expandTables"

  def apply(state: CompilerState): CompilerState = state.map { n =>
    ClientSideOp.mapServerSide(n)(ch => apply(ch, state))
  }

  def apply(n: Node, state: CompilerState): Node = n match {
    case t: TableExpansion => t
    case t: TableNode =>
      val sym = new AnonSymbol
      val expanded = WithOp.encodeRef(t, sym).nodeShaped_*.packedNode
      val processed = apply(state.compiler.runBefore(this, state.withNode(expanded)).tree, state)
      TableExpansion(sym, t, ProductNode(processed.flattenProduct))
    case n => n.nodeMapChildren(ch => apply(ch, state))
  }
}

/** Expand Paths to ProductNodes and TableExpansions into ProductNodes of
  * Paths and TableRefExpansions of Paths, so that all Paths point to
  * individual columns by index */
class ExpandRefs extends Phase with ColumnizerUtils {
  val name = "expandRefs"

  def apply(state: CompilerState) = state.map { n =>
    ClientSideOp.mapServerSide(n)(ch => expandRefs(ch))
  }

  def expandRefs(n: Node, scope: Scope = Scope.empty, keepRef: Boolean = false): Node = n match {
    case p @ Path(psyms) =>
      logger.debug("Checking path "+Path.toString(psyms))
      psyms.head match {
        case f: FieldSymbol => p
        case _ if keepRef => p
        case _ =>
          val syms = psyms.reverse
          scope.get(syms.head) match {
            case Some((target, _)) =>
              val exp = select(syms.tail, narrowStructure(target), (s => scope.get(s).map(_._1))).head
              logger.debug("  narrowed "+p+" to "+exp)
              exp match {
                case t: TableExpansion => burstPath(Path(syms.reverse), t)
                case t: TableRefExpansion => burstPath(Path(syms.reverse), t)
                case pr: ProductNode => burstPath(Path(syms.reverse), pr)
                case n => p
              }
            case None => p
          }
      }
    case n @ Apply(sym: Library.AggregateFunctionSymbol, _) =>
      // Don't expand children of aggregate functions
      n.mapChildrenWithScope(((_, ch, chsc) => expandRefs(ch, chsc, true)), scope)
    case n =>
      // Don't expand children in 'from' positions
      n.mapChildrenWithScope(((symO, ch, chsc) => expandRefs(ch, chsc, symO.isDefined)), scope)
  }

  /** Expand a base path into a given target */
  def burstPath(base: Node, target: Node): Node = target match {
    case ProductNode(ch) =>
      ProductNode(ch.zipWithIndex.map { case (n, idx) =>
        burstPath(Select(base, ElementSymbol(idx+1)), n)
      })
    case TableExpansion(_, t, cols) =>
      TableRefExpansion(new AnonSymbol, base, ProductNode(cols.nodeChildren.zipWithIndex.map { case (n, idx) =>
        burstPath(Select(base, ElementSymbol(idx+1)), n)
      }))
    case TableRefExpansion(_, t, cols) =>
      TableRefExpansion(new AnonSymbol, base, ProductNode(cols.nodeChildren.zipWithIndex.map { case (n, idx) =>
        burstPath(Select(base, ElementSymbol(idx+1)), n)
      }))
    case _ => base
  }
}

trait ColumnizerUtils { _: Phase =>

  /** Navigate into ProductNodes along a path */
  def select(selects: List[Symbol], base: Node, lookup: (Symbol => Option[Node]) = (_ => None)): Vector[Node] = {
    logger.debug("  select("+selects+", "+base+")")
    (selects, base) match {
      case (s, Union(l, r, _, _, _)) => select(s, l, lookup) ++ select(s, r, lookup)
      case (Nil, n) => Vector(n)
      case ((s: ElementSymbol) :: t, ProductNode(ch)) => select(t, ch(s.idx-1), lookup)
      case (selects, Path(rpath)) =>
        val path = rpath.reverse
        logger.debug("  encountered reference "+Path.toString(rpath)+" -> resolving "+path.head)
        lookup(path.head) match {
          case Some(n) =>
            select(path.tail ::: selects, n, lookup)
          case None => throw new SlickException("Cannot resolve "+path.head+" in "+Path.toString(rpath))
        }
      case _ =>
        val narrowed = narrowStructure(base)
        if(narrowed eq base)
          throw new SlickException("Cannot select "+Path.toString(selects.reverse)+" in "+base)
        else select(selects, narrowed, lookup)
    }
  }

  /** Find the actual structure produced by a Node */
  def narrowStructure(n: Node): Node = n match {
    case Pure(n) => n
    case Join(_, _, l, r, _, _) => ProductNode(Seq(narrowStructure(l), narrowStructure(r)))
    case GroupBy(_, _, from, by) => ProductNode(Seq(narrowStructure(by), narrowStructure(from)))
    case u: Union => u.copy(left = narrowStructure(u.left), right = narrowStructure(u.right))
    case FilteredQuery(_, from) => narrowStructure(from)
    case Bind(_, _, select) => narrowStructure(select)
    case n => n
  }
}
