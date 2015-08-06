package slick.compiler

import slick.ast._
import Util._
import TypeUtil._

/** Expand table-valued expressions in the result type to their star projection and compute the
  * missing structural expansions of table types. After this phase the AST should always be
  * well-typed. */
class ExpandTables extends Phase {
  val name = "expandTables"

  def apply(state: CompilerState) = state.map { n => ClientSideOp.mapServerSide(n) { tree =>
    // Find table fields
    val structs = tree.collect[(TypeSymbol, (TermSymbol, Type))] {
      case s @ Select(_ :@ (n: NominalType), sym) => n.sourceNominalType.sym -> (sym -> s.nodeType)
    }.groupBy(_._1).mapValues(v => StructType(v.map(_._2).toMap.toIndexedSeq))
    logger.debug("Found Selects for NominalTypes: "+structs.keySet.mkString(", "))

    // Check for table types
    val tsyms: Set[TableIdentitySymbol] =
      tree.nodeType.collect { case NominalType(sym: TableIdentitySymbol, _) => sym }.toSet
    logger.debug("Tables for expansion in result type: " + tsyms.mkString(", "))

    val tree2 = tree.replace({
      case TableExpansion(_, t, _) => t
      case n => n :@ n.nodeType.replace { case NominalType(tsym, UnassignedType) => NominalType(tsym, structs(tsym)) }
    }, keepType = true, bottomUp = true)
    logger.debug("With correct table types:", tree2)

    if(tsyms.isEmpty) tree2 else {
      // Find the corresponding TableExpansions
      val tables: Map[TableIdentitySymbol, (TermSymbol, Node)] = tree.collect {
        case TableExpansion(s, TableNode(_, _, ts, _, _), ex) if tsyms contains ts => ts -> (s, ex)
      }.toMap
      logger.debug("Table expansions: " + tables.mkString(", "))
      // Create a mapping that expands the tables
      val sym = new AnonSymbol
      val mapping = createResult(tables, Ref(sym), tree2.nodeType.asCollectionType.elementType)
        .infer(Type.Scope(sym -> tree2.nodeType.asCollectionType.elementType), typeChildren = true)
      Bind(sym, tree2, Pure(mapping)).infer()
    }
  }}.withWellTyped(true)

  /** Create an expression that copies a structured value, expanding tables in it. */
  def createResult(expansions: Map[TableIdentitySymbol, (TermSymbol, Node)], path: Node, tpe: Type): Node = tpe match {
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
