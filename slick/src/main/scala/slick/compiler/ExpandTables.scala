package slick.compiler

import slick.ast._
import Util._
import TypeUtil._
import slick.util.ConstArray

import scala.collection.mutable

/** Expand table-valued expressions in the result type to their star projection and compute the
  * missing structural expansions of table types. After this phase the AST should always be
  * well-typed. */
class ExpandTables extends Phase {
  val name = "expandTables"

  def apply(state: CompilerState) = {
    var createdOption = false

    /** Create an expression that copies a structured value, expanding tables in it. */
    def createResult(expansions: collection.Map[TableIdentitySymbol, (TermSymbol, Node)], path: Node, tpe: Type): Node = tpe match {
      case p: ProductType =>
        ProductNode(p.elements.zipWithIndex.map { case (t, i) => createResult(expansions, Select(path, ElementSymbol(i+1)), t) })
      case NominalType(tsym: TableIdentitySymbol, _) if expansions contains tsym =>
        val (sym, exp) = expansions(tsym)
        exp.replace { case Ref(s) if s == sym => path }
      case tpe: NominalType => createResult(expansions, path, tpe.structuralView)
      case m: MappedScalaType =>
        TypeMapping(createResult(expansions, path, m.baseType), m.mapper, m.classTag)
      case OptionType(el) =>
        val gen = new AnonSymbol
        createdOption = true
        OptionFold(path, LiteralNode.nullOption, OptionApply(createResult(expansions, Ref(gen), el)), gen)
      case _ => path
    }

    val s2 = state.map { n => ClientSideOp.mapServerSide(n) { tree =>
      // Create the map of table identities to their base identities.
     val baseIdentity: Map[TypeSymbol, TypeSymbol] = tree.collect[(TypeSymbol, TypeSymbol)] { case TableNode(_, _, i, b) => (i, b) }.toMap
      // Find table fields(StructType) grouped by baseIdentity, so that all tables with the same baseIdentity end up with the same
      // structural type under a different identity when we are traversing the tree to modify it.
      val structs: Map[TypeSymbol, Type] = tree.collect[(TypeSymbol, (FieldSymbol, Type))] {
        case s @ Select(_ :@ (n: NominalType), sym: FieldSymbol) => baseIdentity(n.sourceNominalType.sym) -> (sym -> s.nodeType)
      }.toSeq.groupBy(_._1).mapValues(v => StructType(ConstArray.from(v.map(_._2).toMap))).toMap
      logger.debug("Found Selects for NominalTypes: "+structs.keySet.mkString(", "))

      val tables = new mutable.HashMap[TableIdentitySymbol, (TermSymbol, Node)]
      var expandDistinct = false
      def tr(tree: Node): Node = tree.replace {
        case TableExpansion(gen, t @ TableNode(_, _, i, b), columns) :@ CollectionType(cons, _) =>
          tables += ((i, (gen, columns)))
          t :@ CollectionType(cons, NominalType(i, structs(b)))
        case r: Ref => r.untyped
        case d: Distinct =>
          expandDistinct = d.children.toSeq match {
            case Seq(n1, n2: Ref) =>
              logger.debug(s"expandDistinct true: ${n1}, ${n2}")
              true
            case a =>
              logger.debug(s"expandDistinct false: ${a.mkString(",")}")
              false
          }
          d.mapChildren(tr)
      }
      val tree2 = tr(tree).infer()
      logger.debug("With correct table types:", tree2)
      logger.debug("Table expansions: " + tables.mkString(", "))

      // Perform star expansion in Distinct
      val tree3 = if(!expandDistinct) tree2 else {
        logger.debug("Expanding tables in Distinct")
        tree2.replace({
          case Distinct(s, f, o) => Distinct(s, f, createResult(tables, Ref(s), o.nodeType))
        }, bottomUp = true).infer()
      }

      // Perform star expansion in query result
      if(!tree.nodeType.existsType { case NominalType(_: TableIdentitySymbol, _) => true; case _ => false }) tree3 else {
        logger.debug("Expanding tables in result type")
        // Create a mapping that expands the tables
        val sym = new AnonSymbol
        val mapping = createResult(tables, Ref(sym), tree3.nodeType.asCollectionType.elementType)
          .infer(Type.Scope(sym -> tree3.nodeType.asCollectionType.elementType))
        Bind(sym, tree3, Pure(mapping)).infer()
      }
    }}.withWellTyped(true)
    if(createdOption) s2 + (Phase.assignUniqueSymbols -> state.get(Phase.assignUniqueSymbols).get.copy(nonPrimitiveOption = true))
    else s2
  }
}
