package org.scalaquery.ast

import scala.collection.mutable.{HashMap, ArrayBuffer}
import org.scalaquery.SQueryException
import org.scalaquery.util.Logging
import OptimizerUtil._
import org.scalaquery.ql.{AbstractTable, JoinType}

/**
 * Conversion of basic ASTs to a shape suitable for relational DBs.
 */
object Relational extends Logging {

  def apply(n: Node): Node = {
    val n2 = aliasTableColumns(n)
    if(n2 ne n) {
      AnonSymbol.assignNames(n2, "a")
      logger.debug("aliased:", n2)
    }
    val n3 = convert(n2)
    if(n3 ne n2) logger.debug("converted: ", n3)
    val n4 = inline(n3)
    if(n4 ne n3) logger.debug("inlined: ", n4)
    val n5 = eliminatePureFrom(n4)
    if(n5 ne n4) logger.debug("Pure from eliminated: ", n5)
    n5
  }

  def eliminatePureFrom(tree: Node): Node = {
    val tr = new Transformer.Defs {
      def replace = {
        case FieldRef(Def(Pure(TableRef(table))), fieldSym) => FieldRef(table, fieldSym)
        case FieldRef(Def(Pure(StructNode(defs))), fieldSym) =>
          defs.find(_._1 == fieldSym).get._2
        case c @ Comprehension(from, _, _, _) =>
          val filtered = from.filter {
            case (sym, Pure(t: TableRef)) => false
            case (sym, Pure(s: StructNode)) => false
            case _ => true
          }
          if(filtered.length == from.length) c else c.copy(from = filtered)
      }
    }
    val res = tr.applyOnce(tree)
    // Ensure that no TableRefs and StructNodes remain inside Pure generators
    res.foreach {
      case d: DefNode => d.nodeGenerators.foreach {
        case (sym, Pure(t: TableRef)) => throw new SQueryException("Could not eliminate "+sym+" <- "+t)
        case (sym, Pure(s: StructNode)) => throw new SQueryException("Could not eliminate "+sym+" <- "+s)
        case _ =>
      }
      case _ =>
    }
    res
  }

  val convert = new Transformer {
    def replace = {
      // Bind to Comprehension
      case Bind(gen, from, select) => Comprehension(from = Seq((gen, from)), select = Some(select))
      // Filter to Comprehension
      case Filter(gen, from, where) => Comprehension(from = Seq((gen, from)), where = Seq(where))
      // SortBy to Comprehension
      case SortBy(gen, from, by) => Comprehension(from = Seq((gen, from)), orderBy = by)
      // Merge Comprehension which selects another Comprehension
      case Comprehension(from1, where1, orderBy1, Some(c2 @ Comprehension(from2, where2, orderBy2, select))) =>
        c2.copy(from = from1 ++ from2, where = where1 ++ where2, orderBy = orderBy2 ++ orderBy1)
    }
  }

  def inline(tree: Node) = {
    object ComprehensionStruct {
      def unapply(c: Comprehension): Option[Node] = c match {
        case Comprehension(_, _, _, Some(Pure(s : ProductNode))) => Some(s)
        case Comprehension(_, _, _, Some(Pure(t: TableRef))) => Some(t)
        case Comprehension(_, _, _, Some(c2: Comprehension)) => unapply(c2)
        case Comprehension(from, _, _, None) =>
          from.last._2 match {
            case c2: Comprehension => unapply(c2)
            case Pure(s: StructNode) => Some(TableRef(from.last._1))
            case Pure(t: TableRef) => Some(TableRef(from.last._1))
            case _ => None
          }
        case _ => None
      }
    }
    def protectedRefs = tree.collect[Symbol]{ case TableRef(sym) => sym }.toSet
    logger.debug("protected refs: "+protectedRefs)

    def f(n: Node): Node = n match {
      case c: Comprehension =>
        val newGens = new ArrayBuffer[(Symbol, Node)]
        val newWhere = new ArrayBuffer[Node]
        val newOrderBy = new ArrayBuffer[(Node, Ordering)]
        val eliminated = new HashMap[Symbol, Node]
        var rewrite = false
        def scanFrom(c: Comprehension): Option[Node] = {
          logger.debug("Scanning from clauses of Comprehension "+c.from.map(_._1).mkString(", "))
          val sel = c.from.map {
            case (s,  n @ ComprehensionStruct(target)) if(!protectedRefs(s)) =>
              logger.debug("found ComprehensionStruct at "+s)
              rewrite = true
              eliminated += ((s, target))
              scanFrom(n)
            case (s, f @ FilteredJoin(leftGen, rightGen, left, right, JoinType.Inner, on)) =>
              logger.debug("found FilteredJoin at "+s)
              rewrite = true
              newGens += ((leftGen, left))
              newGens += ((rightGen, right))
              newWhere += on
              c.select
            case (s, f @ BaseJoin(leftGen, rightGen, left, right, JoinType.Inner)) =>
              logger.debug("found BaseJoin at "+s)
              rewrite = true
              newGens += ((leftGen, left))
              newGens += ((rightGen, right))
              c.select
            case t =>
              logger.debug("found other (keeping) at "+t._1)
              newGens += t
              c.select
          }.lastOption
          newWhere ++= c.where
          newOrderBy ++= c.orderBy
          c.select.orElse(sel.getOrElse(None))
        }
        val newSelect = scanFrom(c)
        logger.debug("eliminated: "+eliminated)
        def findProductRef(p: ProductNode, sym: Symbol): Option[Node] = p.nodeChildren.collectFirst {
          case f @ FieldRef(t, s) if sym == s && !eliminated.contains(t) => f
        }
        def findAliasingTarget(p: ProductNode): Option[Symbol] = {
          val targets = p.collect[Symbol]{ case FieldRef(t, _) => t }.toSet
          if(targets.size != 1) None
          else {
            val target = targets.head
            eliminated.get(target) match {
              case Some(p: ProductNode) => findAliasingTarget(p)
              case None => Some(target)
            }
          }
        }
        def replaceRefs(n: Node): Node = n match {
          case FieldRef(t, c) if eliminated.contains(t) =>
            eliminated(t) match {
              case TableRef(tab) =>
                logger.debug("replacing FieldRef("+t+", "+c+") by FieldRef("+tab+", "+c+") [TableRef]")
                replaceRefs(FieldRef(tab, c))
              case StructNode(mapping) =>
                logger.debug("replacing FieldRef("+t+", "+c+") by "+mapping.toMap.apply(c)+" [StructNode]")
                replaceRefs(mapping.toMap.apply(c))
              case p: ProductNode =>
                logger.debug("Finding "+c+" in ProductNode "+t)
                findProductRef(p, c) match {
                  case Some(n) =>
                    logger.debug("replacing FieldRef("+t+", "+c+") by "+n+" [ProductNode]")
                    replaceRefs(n)
                  case None =>
                    findAliasingTarget(p) match {
                      case Some(target) =>
                        val f = FieldRef(target, c)
                        logger.debug("replacing FieldRef("+t+", "+c+") by "+f+" [ProductNode aliasing]")
                        replaceRefs(f)
                      case None => n
                    }
                }
              case _ => n
            }
          case n => n.nodeMapChildren(replaceRefs)
        }
        if(rewrite) replaceRefs(Comprehension(newGens, newWhere, newOrderBy, newSelect)).nodeMapChildren(f)
        else c.nodeMapChildren(f)
      case n => n.nodeMapChildren(f)
    }
    f(tree)
  }

  def filterSource(n: Node): Node = n match {
    case Comprehension(from, _, _, None) => filterSource(from.last._2)
    case n => n
  }

  /**
   * Alias all table columns. Requires a tree with unique symbols.
   */
  def aliasTableColumns(tree: Node): Node = {
    val allDefs = tree.collectAll[(Symbol, Node)]{ case d: DefNode => d.nodeGenerators }.toMap
    def narrow(s: Symbol): Option[AbstractTable[_]] = allDefs.get(s) match {
      case Some(t: AbstractTable[_]) => Some(t)
      case Some(f: FilteredQuery) => narrow(f.generator)
      case _ => None
    }
    def chain(s: Symbol): Seq[Symbol] = allDefs.get(s) match {
      case Some(t: AbstractTable[_]) => Seq(s)
      case Some(f: FilteredQuery) => chain(f.generator) match {
        case Seq() => Seq.empty
        case seq => s +: seq
      }
      case Some(Pure(TableRef(sym))) => chain(sym) match {
        case Seq() => Seq.empty
        case seq => s +: seq
      }
      case _ => Seq.empty
    }
    val tableRefs = tree.collectAll[(Seq[Symbol], FieldSymbol)]{
      case FieldRef(t, f: FieldSymbol) =>
        val ch = chain(t)
        if(ch.isEmpty) Seq.empty
        else Seq((ch, f))
    }
    logger.debug("tableRefs: "+tableRefs)
    val needed = tableRefs.foldLeft(Map.empty[Symbol, (Map[FieldSymbol, AnonSymbol], Set[Symbol])]) { case (m, (seq, f)) =>
      val t = seq.last
      m.updated(t, m.get(t) match {
        case Some((fields, in)) => (fields.updated(f, new AnonSymbol), in ++ seq)
        case None => (Map((f, new AnonSymbol)), seq.toSet)
      })
    }
    logger.debug("needed: "+needed)
    val baseTables: Map[Symbol, Symbol] =
      tableRefs.flatMap{ case (seq, _) => val l = seq.last; seq.map(i => (i, l)) }(collection.breakOut)
    logger.debug("baseTables: "+baseTables)
    def tr(sym: Option[Symbol], n: Node): Node = n match {
      case p @ FieldRef(t, f: FieldSymbol) => baseTables.get(t).flatMap(needed.get) match {
        case Some((symMap, _)) => symMap.get(f) match {
          case Some(a) => FieldRef(t, a)
          case _ => p
        }
        case _ => p
      }
      case t: AbstractTable[_] if(sym.isDefined && needed.contains(sym.get)) =>
        val gen = new AnonSymbol
        val (symMap, _) = needed(sym.get)
        //val struct = symMap.toIndexedSeq[(FieldSymbol, AnonSymbol)].map{ case (oldS, newS) => (newS, FieldRef(gen, oldS)) }
        val struct = (symMap.toIndexedSeq: IndexedSeq[(FieldSymbol, AnonSymbol)]).map{ case (oldS, newS) => (newS, FieldRef(gen, oldS)) }
        Bind(gen, t, Pure(StructNode(struct)))
      case d: DefNode => d.nodeMapScopedChildren(tr)
      case n => n.nodeMapChildren{ ch => tr(None, ch) }
    }
    tr(None, tree)
  }
}
