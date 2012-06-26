package scala.slick.ast
package opt

import scala.math.{min, max}
import scala.collection.mutable.{HashMap, ArrayBuffer}
import scala.slick.SlickException
import scala.slick.util.Logging
import scala.slick.ql.ConstColumn
import Util._

/**
 * Conversion of basic ASTs to a shape suitable for relational DBs.
 *
 * This phase replaces all nodes of types Bind, Filter, SortBy, Take and Drop
 * by Comprehension nodes and merges nested Comprehension nodes.
 */
object Relational extends Logging {
  def apply(n: Node): Node = {
    val n3 = convert.repeat(n)
    if(n3 ne n) logger.debug("converted: ", n3)
    val n4 = fuse(n3)
    if(n4 ne n3) logger.debug("fused: ", n4)
    n4
  }

  val convert = new Transformer {
    def mkFrom(s: Symbol, n: Node): Seq[(Symbol, Node)] = n match {
      case Pure(ProductNode(Seq())) => Seq.empty
      case n => Seq((s, n))
    }
    def replace = {
      // Bind to Comprehension
      case Bind(gen, from, select) => Comprehension(from = mkFrom(gen, from), select = Some(select))
      // Filter to Comprehension
      case Filter(gen, from, where) => Comprehension(from = mkFrom(gen, from), where = Seq(where))
      // SortBy to Comprehension
      case SortBy(gen, from, by) => Comprehension(from = mkFrom(gen, from), orderBy = by)
      // Take and Drop to Comprehension
      case TakeDrop(from, take, drop, gen) =>
        val drop2 = if(drop == Some(0)) None else drop
        if(take == Some(0)) Comprehension(from = mkFrom(gen, from), where = Seq(ConstColumn.FALSE))
        else Comprehension(from = mkFrom(gen, from), fetch = take.map(_.toLong), offset = drop2.map(_.toLong))
      // Merge Comprehension which selects another Comprehension
      case Comprehension(from1, where1, orderBy1, Some(c2 @ Comprehension(from2, where2, orderBy2, select, None, None)), fetch, offset) =>
        c2.copy(from = from1 ++ from2, where = where1 ++ where2, orderBy = orderBy2 ++ orderBy1, fetch = fetch, offset = offset)
    }
  }

  def fuse(n: Node): Node = n.nodeMapChildren(fuse) match {
    case c: Comprehension =>
      val fused = createSelect(c) match {
        case c2 @ Comprehension(_, _, _, Some(sel), _, _) => fuseComprehension(c2)
        case c2 => c2
      }
      val f2 = liftAggregates(fused)
      //if(f2 eq fused) f2 else fuse(f2)
      f2
    case n => n
  }

  /** Check if a Comprehension should be fused into its parent. This happens
    * in the following cases:
    * - It has a Pure generator.
    * - It does not have any generators.
    * - The Comprehension has a 'select' clause which consists only of Paths
    *   and constant values. */
  def isFuseable(c: Comprehension): Boolean = {
    c.from.isEmpty || c.from.exists {
      case (sym, Pure(_)) => true
      case _ => false
    } || (c.select match {
      case Some(Pure(ProductNode(ch))) =>
        ch.map {
          case Path(_) => true
          case _: LiteralNode => true
          case _ => false
        }.forall(identity)
      case _ => false
    })
  }

   /** Fuse simple Comprehensions (no orderBy, fetch or offset), which are
    * contained in the 'from' list of another Comprehension, into their
    * parent. */
  def fuseComprehension(c: Comprehension): Comprehension = {
    var newFrom = new ArrayBuffer[(Symbol, Node)]
    val newWhere = new ArrayBuffer[Node]
    val newOrderBy = new ArrayBuffer[(Node, Ordering)]
    val structs = new HashMap[Symbol, Node]
    var fuse = false

    def inline(n: Node): Node = n match {
      case p @ Path(psyms) =>
        logger.debug("Inlining "+Path.toString(psyms)+" with structs "+structs.keySet)
        val syms = psyms.reverse
        structs.get(syms.head).map{ base =>
          logger.debug("  found struct "+base)
          val repl = select(syms.tail, base)(0)
          inline(repl)
        }.getOrElse(p)
      case n => n.nodeMapChildren(inline)
    }

    c.from.foreach {
      case (sym, from @ Comprehension(_, _, _, _, None, None)) if isFuseable(from) =>
        logger.debug("Found fuseable generator "+sym+": "+from)
        from.from.foreach { case (s, n) => newFrom += s -> inline(n) }
        for(n <- from.where) newWhere += inline(n)
        for((n, o) <- from.orderBy) newOrderBy += inline(n) -> o
        structs += sym -> narrowStructure(from)
        fuse = true
      case t =>
        newFrom += t
    }
    if(fuse) {
      logger.debug("Fusing Comprehension:", c)
      val c2 = Comprehension(
        newFrom,
        newWhere ++ c.where.map(inline),
        c.orderBy.map { case (n, o) => (inline(n), o) } ++ newOrderBy,
        c.select.map { case n => inline(n) },
        c.fetch, c.offset)
      logger.debug("Fused to:", c2)
      c2
    }
    else c
  }

  /** Lift aggregates of sub-queries into the 'from' list. */
  def liftAggregates(c: Comprehension): Comprehension = {
    val lift = ArrayBuffer[(AnonSymbol, AnonSymbol, Library.AggregateFunctionSymbol, Comprehension)]()
    def tr(n: Node): Node = n match {
      //TODO Once we can recognize structurally equivalent sub-queries and merge them, c2 could be a Ref
      case Apply(s: Library.AggregateFunctionSymbol, Seq(c2: Comprehension)) =>
        val a = new AnonSymbol
        val f = new AnonSymbol
        lift += ((a, f, s, c2))
        Select(Ref(a), f)
      case c: Comprehension => c // don't recurse into sub-queries
      case n => n.nodeMapChildren(tr)
    }
    if(c.select.isEmpty) c else {
      val sel = c.select.get
      val sel2 = tr(sel)
      if(lift.isEmpty) c else {
        val newFrom = lift.map { case (a, f, s, c2) =>
          val a2 = new AnonSymbol
          val (c2b, call) = s match {
            case Library.CountAll =>
              (c2, Apply(Library.Count, Seq(ConstColumn(1))))
            case s =>
              val c3 = ensureStruct(c2)
              // All standard aggregate functions operate on a single column
              val Some(Pure(StructNode(Seq((f2, _))))) = c3.select
              (c3, Apply(s, Seq(Select(Ref(a2), f2))))
          }
          a -> Comprehension(from = Seq(a2 -> c2b),
            select = Some(Pure(StructNode(IndexedSeq(f -> call)))))
        }
        c.copy(from = c.from ++ newFrom, select = Some(sel2))
      }
    }
  }

  /** Rewrite a Comprehension to always return a StructNode */
  def ensureStruct(c: Comprehension): Comprehension = {
    val c2 = createSelect(c)
    c2.select match {
      case Some(Pure(_: StructNode)) => c2
      case Some(Pure(ProductNode(ch))) =>
        c2.copy(select = Some(Pure(StructNode(ch.iterator.map(n => (new AnonSymbol) -> n).toIndexedSeq))))
      case Some(Pure(n)) =>
        c2.copy(select = Some(Pure(StructNode(IndexedSeq((new AnonSymbol) -> n)))))
    }
  }

  def select(selects: List[Symbol], base: Node): Vector[Node] = {
    logger.debug("select("+selects+", "+base+")")
    (selects, base) match {
      //case (s, Union(l, r, _, _, _)) => select(s, l) ++ select(s, r)
      case (Nil, n) => Vector(n)
      case ((s: AnonSymbol) :: t, StructNode(ch)) => select(t, ch.find{ case (s2,_) => s == s2 }.get._2)
      //case ((s: ElementSymbol) :: t, ProductNode(ch @ _*)) => select(t, ch(s.idx-1))
      case _ => throw new SlickException("Cannot select "+Path.toString(selects.reverse)+" in "+base)
    }
  }

  def narrowStructure(n: Node): Node = n match {
    case Pure(n) => n
    //case Join(_, _, l, r, _, _) => ProductNode(narrowStructure(l), narrowStructure(r))
    //case u: Union => u.copy(left = narrowStructure(u.left), right = narrowStructure(u.right))
    case Comprehension(from, _, _, None, _, _) => narrowStructure(from.head._2)
    case Comprehension(_, _, _, Some(n), _, _) => narrowStructure(n)
    case n => n
  }

  /** Create a select for a Comprehension without one. */
  def createSelect(c: Comprehension): Comprehension = if(c.select.isDefined) c else {
    c.from.last match {
      case (sym, Comprehension(_, _, _, Some(Pure(StructNode(struct))), _, _)) =>
        val r = Ref(sym)
        val copyStruct = StructNode(struct.map { case (field, _) =>
          (field, Select(r, field))
        })
        c.copy(select = Some(Pure(copyStruct)))
      /*case (sym, Pure(StructNode(struct))) =>
        val r = Ref(sym)
        val copyStruct = StructNode(struct.map { case (field, _) =>
          (field, Select(r, field))
        })
        c.copy(select = Some(Pure(copyStruct)))*/
      case _ => c
    }
  }

  /** An extractor for nested Take and Drop nodes */
  object TakeDrop {
    def unapply(n: Node): Option[(Node, Option[Int], Option[Int], Symbol)] = n match {
      case Take(from, num, sym) => unapply(from) match {
        case Some((f, Some(t), d, _)) => Some((f, Some(min(t, num)), d, sym))
        case Some((f, None, d, _)) => Some((f, Some(num), d, sym))
        case _ => Some((from, Some(num), None, sym))
      }
      case Drop(from, num, sym) => unapply(from) match {
        case Some((f, Some(t), None, _)) => Some((f, Some(max(0, t-num)), Some(num), sym))
        case Some((f, None, Some(d), _)) => Some((f, None, Some(d+num), sym))
        case Some((f, Some(t), Some(d), _)) => Some((f, Some(max(0, t-num)), Some(d+num), sym))
        case _ => Some((from, None, Some(num), sym))
      }
      case _ => None
    }
  }
}
