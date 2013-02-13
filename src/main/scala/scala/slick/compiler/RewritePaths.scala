package scala.slick.compiler

import scala.collection.mutable.{HashSet, HashMap}
import scala.slick.SlickException
import scala.slick.ast._
import Util._
import TypeUtil._

/**
 * Remove TableExpansions and TableRefExpansions, and flatten ProductNodes
 * into StructNodes and remove unnecessary columns from them.
 */
class RewritePaths extends Phase {
  val name = "rewritePaths"

  def apply(state: CompilerState) = state.map { n =>
    ClientSideOp.mapServerSide(n)(applyServerSide)
  }

  def applyServerSide(n: Node) = {
    def flattenToStruct(n: Node): (Node, Vector[(Symbol, Node)]) = n match {
      case ProductNode(ch) =>
        val chf = ch.map(flattenToStruct)
        (ProductNode(chf.map(_._1)), chf.map(_._2).foldLeft[Vector[(Symbol, Node)]](Vector())(_ ++ _))
      case n =>
        val sym = new AnonSymbol
        (Ref(sym), Vector((sym, n)))
    }

    val flattened = new HashMap[List[Symbol], Node]
    val defs = new HashMap[Symbol, Node]

    def narrowRef(s: Symbol): Symbol = defs.get(s) match {
      case Some(u: Union) => narrowRef(u.leftGen)
      case Some(FilteredQuery(gen, _)) => narrowRef(gen)
      case _ => s
    }

    def findFlattened(syms: List[Symbol], base: List[Symbol]): Option[(Node, List[Symbol], List[Symbol])] = syms match {
      case Nil => None
      case h :: t =>
        val nh = narrowRef(h)
        defs.get(nh) match {
          case Some(j: Join) =>
            logger.debug("  found Join for "+nh+" (from "+h+")")
            t match {
              case (e @ ElementSymbol(1)) :: tt => findFlattened(j.leftGen :: tt, e :: base)
              case (e @ ElementSymbol(2)) :: tt => findFlattened(j.rightGen :: tt, e :: base)
              case _ => None
            }
          case Some(g: GroupBy) =>
            logger.debug("  found GroupBy for "+nh+" (from "+h+")")
            t match {
              case (e @ ElementSymbol(1)) :: tt => findFlattened(g.byGen :: tt, e :: base)
              case (e @ ElementSymbol(2)) :: tt => findFlattened(g.fromGen :: tt, e :: base)
              case _ => None
            }
          case Some(Path(syms)) =>
            logger.debug("  found path for "+nh+" (from "+h+"): "+Path.toString(syms)+", remaining: "+t)
            val target = findFlattened(syms.reverse, Nil)
            logger.debug("    pointing to "+target)
            target.map { case (struct, _, _) => (struct, t, Nil) }
          case o =>
            logger.debug("  found non-Join/GroupBy for "+nh+" (from "+h+"): "+o)
            flattened.get(List(nh)).map(n => (n, t, base))
        }
    }

    /** Remove expansions, flatten structs, and gather defs and flattened structs */
    def gather(refO: Option[Symbol], n: Node): Node = removeExpansion(n) match {
      case Bind(gen, from, Pure(x)) =>
        val from2 = from match {
          case Pure(_) =>
            val x2 = gather(Some(gen), from).asInstanceOf[Pure].child
            val (mapping, repl) = flattenToStruct(x2)
            logger.debug("Storing flattened Pure struct as "+Path.toString(List(gen)))
            flattened += List(gen) -> mapping
            Pure(StructNode(repl))
          case n =>
            gather(Some(gen), n)
        }
        logger.debug("Storing def for "+gen+" from Pure Bind")
        defs += gen -> from2
        val x2 = gather(None, x)
        val pure2 = refO match {
          case Some(ref) =>
            val (mapping, repl) = flattenToStruct(x2)
            logger.debug("Storing flattened struct as "+Path.toString(List(ref)))
            flattened += List(ref) -> mapping
            StructNode(repl)
          case None =>
            ProductNode(x2.flattenProduct)
        }
        Bind(gen, from2, Pure(pure2))
      case b @ Bind(gen, from, sel) =>
        val from2 = gather(Some(gen), from)
        logger.debug("Storing def for "+gen+" from non-Pure Bind")
        defs += gen -> from2
        val sel2 = gather(refO, sel) // the "select" clause inherits our ref
        if((from2 eq from) && (sel2 eq sel)) b
        else Bind(gen, from2, sel2)
      case d: DefNode =>
        d.nodeMapScopedChildren { case (symO, ch) =>
          val ch2 = gather(symO, ch)
          symO.foreach { sym =>
            logger.debug("Storing def for "+sym)
            defs += sym -> ch2
          }
          ch2
        }
      case n =>
        n.nodeMapChildren(ch => gather(None, ch))
    }

    def replaceRefs(n: Node): Node = n match {
      case Path(syms) => syms.head match {
        case f: FieldSymbol => n // inside a former TableExpansion - no need to go down this path
        case _ =>
          logger.debug("Trying to replace "+Path.toString(syms))
          val rsyms = syms.reverse
          findFlattened(rsyms, Nil) match {
            case Some(fl @ (struct, rest, base)) =>
              logger.debug("  found flattened: "+fl)
              findFieldSymbol(struct, rest) match {
                case Some(fsym) => Path(fsym :: base ::: rsyms.head :: Nil)
                case None => n
              }
            case _ => n
          }
      }
      case n => n.nodeMapChildren(replaceRefs)
    }

    val n2 = gather(None, n)
    if(n2 ne n) logger.debug("Expansions removed, ProductNodes rewritten to StructNodes", n2)
    val n3 = replaceRefs(n2)
    if(n3 ne n2) logger.debug("Refs replaced", n3)
    n3
  }

  def findFieldSymbol(n: Node, path: List[Symbol]): Option[Symbol] = (path, n) match {
    case (ElementSymbol(idx) :: t, ProductNode(ch)) => findFieldSymbol(ch(idx-1), t)
    case (Nil, Ref(sym)) => Some(sym)
    case (Nil, _) => None
    case _ => throw new SlickException("Illegal "+Path.toString(path)+" into TableExpansion structure "+n)
  }

  def removeExpansion(n: Node) = n match {
    case TableExpansion(gen, t, cols) =>
      val tableRefs = cols.collect[Select] { case s @ Select(Ref(gen), _) => s }
      val structType = StructType(tableRefs.map{ case sel @ Select(_, sym) => (sym, sel.nodeType) }(collection.breakOut))
      val cons = t.nodeType.asCollectionType.cons
      Bind(gen, t.nodeRebuildWithType(CollectionType(cons, structType)), Pure(cols))
    case TableRefExpansion(_, ref, cols) => cols
    case n => n
  }
}

/** Assign the AnonSymbols of fields from the left side of a Union to the
  * right side. This ensures that both sides are protected when we prune
  * unused references pointing to left-side Symbols. */
class RelabelUnions extends Phase {
  val name = "relabelUnions"

  def apply(state: CompilerState) = state.map { n =>
    ClientSideOp.mapServerSide(n)(relabelUnions)
  }

  def relabelUnions(n: Node): Node = n match {
    case u @ Union(BindTarget(Pure(StructNode(ls))), rb @ BindTarget(Pure(StructNode(rs))), _, _, _)
      if ls.size == rs.size =>
      val rs2 = (ls, rs).zipped.map { case ((s, _), (_, n)) => (s, n) }
      u.copy(right = BindTarget.replace(rb, Pure(StructNode(rs2)))).nodeMapChildren(relabelUnions)
    case n => n.nodeMapChildren(relabelUnions)
  }

  object BindTarget {
    def unapply(n: Node): Option[Node] = n match {
      case Bind(_, _, t) =>
        if(t.isInstanceOf[Bind]) unapply(t)
        else Some(t)
      case _ => None
    }
    def replace(n: Node, sel: Node): Node = n match {
      case b @ Bind(_, _, t) =>
        if(t.isInstanceOf[Bind]) b.copy(select = replace(t, sel))
        else b.copy(select = sel)
    }
  }
}

/** Remove unreferenced fields from StructNodes */
class PruneFields extends Phase {
  val name = "pruneFields"

  def apply(state: CompilerState) = state.map { n =>
    ClientSideOp.mapServerSide(n)(prune.repeat)
  }

  def prune = new Transformer {
    val refs = new HashSet[Symbol]()
    override def initTree(n: Node) {
      super.initTree(n)
      refs.clear()
      refs ++= n.collect[Symbol] { case Select(_, f: Symbol) => f }
      logger.debug("Protecting refs: "+refs)
    }
    def replace = {
      case n @ StructNode(ch) =>
        val ch2 = ch.filter { case (sym, n) => refs.contains(sym) }
        if(ch2.length == ch.length) n else StructNode(ch2)
    }
  }
}
