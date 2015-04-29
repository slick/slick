package slick.compiler

import scala.collection.mutable.{ArrayBuffer, HashMap}
import slick.ast._
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
    val tree2 = tree.replace({ case TableExpansion(_, t, _) => t }, keepType = true)
    if(tsyms.isEmpty) tree2 else {
      // Find the corresponding TableExpansions
      val tables: Map[TableIdentitySymbol, (Symbol, Node)] = tree.collect {
        case TableExpansion(s, TableNode(_, _, ts, _, _), ex) if tsyms contains ts => ts -> (s, ex)
      }.toMap
      logger.debug("Table expansions: " + tables.mkString(", "))
      // Create a mapping that expands the tables
      val sym = new AnonSymbol
      val mapping = createResult(tables, Ref(sym), tree.nodeType.asCollectionType.elementType)
        .nodeWithComputedType(SymbolScope.empty + (sym -> tree.nodeType.asCollectionType.elementType), typeChildren = true)
      Bind(sym, tree2, Pure(mapping)).nodeWithComputedType()
    }
  }}

  /** Create an expression that copies a structured value, expanding tables in it. */
  def createResult(expansions: Map[TableIdentitySymbol, (Symbol, Node)], path: Node, tpe: Type): Node = tpe match {
    case p: ProductType =>
      ProductNode(p.numberedElements.map { case (s, t) => createResult(expansions, Select(path, s), t) }.toVector)
    case NominalType(tsym: TableIdentitySymbol, _) if expansions contains tsym =>
      val (sym, exp) = expansions(tsym)
      exp.replace { case Ref(s) if s == sym => path }
    case tpe: NominalType => createResult(expansions, path, tpe.structuralView)
    case m: MappedScalaType =>
      TypeMapping(createResult(expansions, path, m.baseType), m.mapper, m.classTag)
    case OptionType(el) =>
      val gen = new AnonSymbol
      OptionFold(path, LiteralNode.nullOption, OptionApply(createResult(expansions, Ref(gen), el)), gen)
    case _ => path
  }
}

/** Expand paths of record types to reference all fields individually and
  * recreate the record structure at the call site. */
class ExpandRecords extends Phase {
  val name = "expandRecords"

  def apply(state: CompilerState) = state.map { tree =>
    tree.replace({ case n @ Path(_) => expandPath(n) }, keepType = true)
  }

  def expandPath(n: Node): Node = n.nodeType.structural match {
    case StructType(ch) =>
      StructNode(ch.map { case (s, t) =>
        (s, expandPath(n.select(s).nodeTypedOrCopy(t)))
      }(collection.breakOut)).nodeTyped(n.nodeType)
    case p: ProductType =>
      ProductNode(p.numberedElements.map { case (s, t) =>
        expandPath(n.select(s).nodeTypedOrCopy(t))
      }.toVector).nodeTyped(n.nodeType)
    case t => n
  }
}

/** Flatten all Pure node contents into a single StructNode. */
class FlattenProjections extends Phase {
  val name = "flattenProjections"

  def apply(state: CompilerState) = state.map { tree =>
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
  }

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
