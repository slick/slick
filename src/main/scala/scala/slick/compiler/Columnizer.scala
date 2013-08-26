package scala.slick.compiler

import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.slick.SlickException
import scala.slick.ast._
import Util._
import TypeUtil._

/** Expand table-valued expressions in the result type to their star projection. */
class ExpandTables extends Phase {
  val name = "expandTables"

  def apply(state: CompilerState) = state.map { n => ClientSideOp.mapServerSide(n) { tree =>
    // Check for table types
    val tsyms: Set[TableIdentitySymbol] =
      tree.nodeType.collect { case NominalType(sym: TableIdentitySymbol) => sym }.toSet
    logger.debug("Tables for expansion in result type: " + tsyms.mkString(", "))
    if(tsyms.isEmpty) tree else {
      // Find the corresponding TableExpansions
      val tables: Map[TableIdentitySymbol, (Symbol, Node)] = tree.collect {
        case TableExpansion(s, TableNode(_, _, ts, _), ex) if tsyms contains ts => ts -> (s, ex)
      }.toMap
      logger.debug("Table expansions: " + tables.mkString(", "))
      // Create a mapping that expands the tables
      val sym = new AnonSymbol
      val mapping = createResult(tables, sym :: Nil, tree.nodeType.asCollectionType.elementType)
        .nodeWithComputedType(SymbolScope.empty + (sym -> tree.nodeType.asCollectionType.elementType), typeChildren = true)
      Bind(sym, tree, Pure(mapping)).nodeWithComputedType()
    }
  }}

  /** Create an expression that copies a structured value, expanding tables in it. */
  def createResult(expansions: Map[TableIdentitySymbol, (Symbol, Node)], path: List[Symbol], tpe: Type): Node = tpe match {
    case p: ProductType =>
      ProductNode(p.numberedElements.map { case (s, t) => createResult(expansions, s :: path, t) }.toVector)
    case NominalType(tsym: TableIdentitySymbol) if expansions contains tsym =>
      val (sym, exp) = expansions(tsym)
      val p = Path(path)
      exp.replace { case Ref(s) if s == sym => p }
    case tpe: NominalType => createResult(expansions, path, tpe.structuralView)
    case _ => Path(path)
  }
}


/** Expand paths of record types to reference all fields individually and
  * recreate the record structure at the call site.
  * We also remove TableExpansions here because we need to transform the whole
  * tree anyway (Doing that in expandTables would be more logical). */
class ExpandRecords extends Phase {
  val name = "expandRecords"

  def apply(state: CompilerState) = state.map { n => ClientSideOp.mapServerSide(n){ tree =>
    def tr(n: Node): Node = n match {
      case Path(_) => expandPath(n)
      case TableExpansion(_, table, _) => table
      case n => n.nodeMapChildren(tr, keepType = true)
    }
    tr(tree)
  }}

  def expandPath(n: Node): Node = n.nodeType.structural match {
    case StructType(ch) =>
      StructNode(ch.map { case (s, t) =>
        (s, expandPath(Select(n, s).nodeTyped(t)))
      }(collection.breakOut)).nodeTyped(n.nodeType)
    case p: ProductType =>
      ProductNode(p.numberedElements.map { case (s, t) =>
        expandPath(Select(n, s).nodeTyped(t))
      }.toVector).nodeTyped(n.nodeType)
    case t => n
  }
}

/** Flatten all Pure node contents into a single StructNode. */
class FlattenProjections extends Phase {
  val name = "flattenProjections"

  def apply(state: CompilerState) = state.map { n => ClientSideOp.mapServerSide(n){ tree =>
    val translations = new HashMap[TypeSymbol, (Map[List[Symbol], Symbol], StructType)]
    def tr(n: Node): Node = n match {
      case Pure(v, ts) =>
        val tsym = n.nodeType.asCollectionType.elementType.asInstanceOf[NominalType].sym
        logger.debug(s"Flattening projection $tsym")
        val (newV, newTranslations) = flattenProjection(tr(v))
        translations += tsym -> (newTranslations, newV.nodeType.asInstanceOf[StructType])
        Pure(newV, ts).nodeWithComputedType()
      case Path(path) =>
        logger.debug("Analyzing "+Path.toString(path)+" with symbols "+translations.keySet.mkString(", "))
        splitPath(n, translations.keySet) match {
          case Some((base, rest, tsym)) =>
            logger.debug("Found "+Path.toString(path)+" with local part "+Path.toString(rest)+" over "+tsym)
            val (paths, tpe) = translations(tsym)
            def retype(n: Node): Node = n.nodeMapChildren(retype, keepType = true).nodeTypedOrCopy(n.nodeType.replace {
              case t @ NominalType(tsym) if translations.contains(tsym) =>
                t.withStructuralView(tpe)
            })
            Select(retype(base), paths(rest)).nodeTyped(n.nodeType)
          case None => n
        }
      case n => n.nodeMapChildren(tr)
    }
    tr(tree).nodeWithComputedType(typeChildren = true)
  }}

  /** Split a path into the shortest part with a NominalType and the rest on top of it. */
  def splitPath(n: Node, candidates: scala.collection.Set[TypeSymbol]): Option[(Node, List[Symbol], TypeSymbol)] = {
    def checkType = n.nodeType match {
      case NominalType(tsym) if candidates contains tsym => Some((n, Nil, tsym))
      case _ => None
    }
    n match {
      case Select(in, field) => splitPath(in, candidates) match {
        case Some((n, p, tsym)) => Some((n, field :: p, tsym))
        case None => checkType
      }
      case _: Ref => checkType
    }
  }

  /** Flatten a projection into a StructNode. */
  def flattenProjection(n: Node): (StructNode, Map[List[Symbol], Symbol]) = {
    val defs = new ArrayBuffer[(Symbol, Node)]
    val paths = new HashMap[List[Symbol], Symbol]
    def flatten(n: Node, path: List[Symbol]) {
      logger.debug("Flattening node at "+Path.toString(path), n)
      n match {
        case StructNode(ch) =>
          ch.foreach { case (s, n) => flatten(n, s :: path) }
        case p: ProductNode =>
          p.numberedElements.foreach { case (s, n) => flatten(n, s :: path) }
        case n =>
          val sym = new AnonSymbol
          logger.debug(s"Adding definition: $sym -> $n")
          defs += sym -> n
          paths += path -> sym
      }
    }
    flatten(n, Nil)
    (StructNode(defs).nodeWithComputedType(), paths.toMap)
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
    case u @ Union(BindTarget(Pure(StructNode(ls), lts)), rb @ BindTarget(Pure(StructNode(rs), _)), _, _, _) =>
      val rs2 = (ls, rs).zipped.map { case ((s, _), (_, n)) => (s, n) }
      val u2 = u.copy(right = BindTarget.replace(rb, Pure(StructNode(rs2), lts)))
      u2.nodeMapChildren(relabelUnions).nodeWithComputedType()
    case n => n.nodeMapChildren(relabelUnions, keepType = true)
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

/** Remove unreferenced fields from StructNodes and convert unreferenced
  * StructNodes to single columns or ProductNodes (which is needed for
  * aggregation functions and at the top level). */
class PruneFields extends Phase {
  val name = "pruneFields"

  def apply(state: CompilerState) = state.map { n => ClientSideOp.mapServerSide(n){ n =>
    val top = n.nodeType.asCollectionType.elementType.asInstanceOf[NominalType].sym
    val referenced = n.collect[(TypeSymbol, Symbol)] { case Select(_ :@ NominalType(s), f) => (s, f) }.toSet
    val allTSyms = n.collect[TypeSymbol] { case Pure(_, _) :@ CollectionType(_, NominalType(ts)) => ts }.toSet
    val unrefTSyms = allTSyms -- referenced.map(_._1)
    logger.debug(s"Result tsym: $top; Unreferenced: ${unrefTSyms.mkString(", ")}; Field refs: ${referenced.mkString(", ")}")
    def tr(n: Node): Node =  n.replace {
      case (p @ Pure(s @ StructNode(ch), pts)) :@ CollectionType(_, NominalType(ts)) =>
        if(unrefTSyms contains ts) {
          val ch2 = s.nodeChildren.map(tr)
          val res = Pure(if(ch2.length == 1 && ts != top) ch2(0) else ProductNode(ch2), pts)
          res
        } else {
          val ch2 = ch.collect { case (sym, n) if referenced.contains((ts, sym)) => (sym, tr(n)) }
          if(ch2 == ch) p else Pure(StructNode(ch2), pts)
        }
    }
    tr(n)
  }}
}
