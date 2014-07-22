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
      tree.nodeType.collect { case NominalType(sym: TableIdentitySymbol, _) => sym }.toSet
    logger.debug("Tables for expansion in result type: " + tsyms.mkString(", "))
    if(tsyms.isEmpty) tree else {
      // Find the corresponding TableExpansions
      val tables: Map[TableIdentitySymbol, (Symbol, Node)] = tree.collect {
        case TableExpansion(s, TableNode(_, _, ts, _, _), ex) if tsyms contains ts => ts -> (s, ex)
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
    case NominalType(tsym: TableIdentitySymbol, _) if expansions contains tsym =>
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

/** Expand multi-column conditional expressions and SilentCasts created by expandSums.
  * Single-column conditionals involving NULL values are optimized away where possible. */
class ExpandConditionals extends Phase {
  val name = "expandConditionals"

  def apply(state: CompilerState) = state.map { n => ClientSideOp.mapServerSide(n)(tr) }

  def tr(n: Node): Node = n.nodeMapChildren(tr, keepType = true) match {
    // Expand multi-column SilentCasts
    case Library.SilentCast(LiteralNode(None) :@ OptionType(ScalaBaseType.nullType)) :@ tpe =>
      buildMultiColumnNone(tpe)
    case cast @ Library.SilentCast(ch) :@ Type.Structural(ProductType(typeCh)) =>
      val elems = typeCh.zipWithIndex.map { case (t, idx) => tr(Library.SilentCast.typed(t, select(ch, ElementSymbol(idx+1)))) }
      ProductNode(elems).nodeWithComputedType()
    case Library.SilentCast(ch) :@ Type.Structural(StructType(typeCh)) =>
      val elems = typeCh.map { case (sym, t) => (sym, tr(Library.SilentCast.typed(t, select(ch, sym)))) }
      StructNode(elems).nodeWithComputedType()

    // Optimize trivial SilentCasts
    case Library.SilentCast(v :@ tpe) :@ tpe2 if tpe.structural == tpe2.structural => v
    case Library.SilentCast(Library.SilentCast(ch)) :@ tpe => tr(Library.SilentCast.typed(tpe, ch))
    case Library.SilentCast(LiteralNode(None)) :@ (tpe @ OptionType.Primitive(_)) => LiteralNode(tpe, None)

    // Expand multi-column ConditionalExprs
    case (cond @ IfThenElse(_)) :@ Type.Structural(ProductType(chTypes)) =>
      val ch = (1 to chTypes.length).map { idx =>
        val sym = ElementSymbol(idx)
        tr(cond.mapResultClauses(n => select(n, sym)).nodeWithComputedType())
      }
      ProductNode(ch).nodeWithComputedType()
    case (cond @ IfThenElse(_)) :@ Type.Structural(StructType(chTypes)) =>
      val ch = chTypes.map { case (sym, _) =>
        (sym, tr(cond.mapResultClauses(n => select(n, sym)).nodeWithComputedType()))
      }
      StructNode(ch).nodeWithComputedType()

    // Optimize null-propagating single-column ConditionalExprs
    case IfThenElse(Seq(Library.==(r, LiteralNode(null)), Library.SilentCast(LiteralNode(None)), c @ Library.SilentCast(r2))) if r == r2 => c

    // Fix Untyped nulls in else clauses
    case cond @ IfThenElse(clauses) if (clauses.last match { case LiteralNode(None) :@ OptionType(ScalaBaseType.nullType) => true; case _ => false }) =>
      cond.copy(clauses.init :+ LiteralNode(cond.nodeType, None))

    // Resolve Selects into ProductNodes and StructNodes
    case Select(ProductNode(ch), ElementSymbol(idx)) => ch(idx-1)
    case Select(StructNode(ch), sym) => ch.find(_._1 == sym).get._2

    case n => n
  }

  def select(n: Node, sym: Symbol): Node = n match {
    // Can occur in an Else clause generated by Rep[Option[_]].filter. It won't pass type checking,
    // so we have to resolve it here instead of relying on `tr` to optimize it away afterwards.
    case LiteralNode(None) :@ OptionType(ScalaBaseType.nullType) => n
    case n => n.select(sym)
  }

  def buildMultiColumnNone(tpe: Type): Node = (tpe.structural match {
    case ProductType(ch) => ProductNode(ch.map(buildMultiColumnNone))
    case StructType(ch) => StructNode(ch.map { case (sym, t) => (sym, buildMultiColumnNone(t)) })
    case OptionType(ch) => LiteralNode(tpe, None)
    case t => throw new SlickException("Unexpected non-Option type in multi-column None")
  }).nodeTypedOrCopy(tpe)
}

/** Flatten all Pure node contents into a single StructNode. */
class FlattenProjections extends Phase {
  val name = "flattenProjections"

  def apply(state: CompilerState) = state.map { n => ClientSideOp.mapServerSide(n){ tree =>
    val translations = new HashMap[TypeSymbol, (Map[List[Symbol], Symbol], StructType)]
    def tr(n: Node): Node = n match {
      case Pure(v, ts) =>
        logger.debug(s"Flattening projection $ts")
        val (newV, newTranslations) = flattenProjection(tr(v))
        translations += ts -> (newTranslations, newV.nodeType.asInstanceOf[StructType])
        val res = Pure(newV, ts).nodeWithComputedType()
        logger.debug("Flattened projection to", res)
        res
      case p @ Path(path) =>
        logger.debug("Analyzing "+Path.toString(path)+" with symbols "+translations.keySet.mkString(", "), p)
        splitPath(n, translations.keySet) match {
          case Some((base, rest, tsym)) =>
            logger.debug("Found "+Path.toString(path)+" with local part "+rest.map(Path.toString _)+" over "+tsym)
            val (paths, tpe) = translations(tsym)
            logger.debug(s"  Translation for $tsym: ($paths, $tpe)")
            def retype(n: Node): Node = n.nodeMapChildren(retype, keepType = true).nodeTypedOrCopy(n.nodeType.replace {
              case t @ NominalType(tsym, _) if translations.contains(tsym) => t.withStructuralView(tpe)
            })
            rest match {
              case Some(r) => Select(retype(base), paths(r)).nodeWithComputedType()
              case None => retype(base)
            }
          case None => n
        }
      case n => n.nodeMapChildren(tr)
    }
    tr(tree).nodeWithComputedType(typeChildren = true)
  }}

  /** Split a path into the shortest part with a NominalType and the rest on
    * top of it. Returns `None` if there is no NominalType with one of the
    * candidate TypeSymbols in the path, otherwise returns `Some(base, rest, tsym)`
    * where `tsym` is the symbol that was found and `base` is the Node in the path
    * which has a NominalType of that symbol. If the NominalType occurs inside a
    * CollectionType, `rest` is `None`, otherwise it is `Some(path)` with a
    * (possibly empty) path of symbols on top of `base`. */
  def splitPath(n: Node, candidates: scala.collection.Set[TypeSymbol]): Option[(Node, Option[List[Symbol]], TypeSymbol)] = {
    def checkType(tpe: Type): Option[(Node, Option[List[Symbol]], TypeSymbol)] = tpe match {
      case NominalType(tsym, _) if candidates contains tsym => Some((n, Some(Nil), tsym))
      case CollectionType(cons, el) => checkType(el).map { case (n, _, tsym) => (n, None, tsym) }
      case _ => None
    }
    n match {
      case Select(in, field) => splitPath(in, candidates) match {
        case Some((n, p, tsym)) => Some((n, Some(field :: p.get), tsym))
        case None => checkType(n.nodeType)
      }
      case _: Ref => checkType(n.nodeType)
    }
  }

  /** Flatten a projection into a StructNode. */
  def flattenProjection(n: Node): (StructNode, Map[List[Symbol], Symbol]) = {
    val defs = new ArrayBuffer[(Symbol, Node)]
    val paths = new HashMap[List[Symbol], Symbol]
    def flatten(n: Node, path: List[Symbol]) {
      logger.debug("Flattening node at "+Path.toString(path), n)
      n match {
        case StructNode(ch) => ch.foreach { case (s, n) => flatten(n, s :: path) }
        case p: ProductNode => p.numberedElements.foreach { case (s, n) => flatten(n, s :: path) }
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
    val referenced = n.collect[(TypeSymbol, Symbol)] { case Select(_ :@ NominalType(s, _), f) => (s, f) }.toSet
    val allTSyms = n.collect[TypeSymbol] { case Pure(_, _) :@ CollectionType(_, NominalType(ts, _)) => ts }.toSet
    val unrefTSyms = allTSyms -- referenced.map(_._1)
    logger.debug(s"Result tsym: $top; Unreferenced: ${unrefTSyms.mkString(", ")}; Field refs: ${referenced.mkString(", ")}")
    def tr(n: Node): Node =  n.replace {
      case (p @ Pure(s @ StructNode(ch), pts)) :@ CollectionType(_, NominalType(ts, _)) =>
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
