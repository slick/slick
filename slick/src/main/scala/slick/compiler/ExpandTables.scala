package slick.compiler

import slick.ast._
import Util._
import TypeUtil._
import slick.compat.collection._
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
      }.toSeq.groupBy(_._1).transform((_, v) => StructType(ConstArray.from(v.map(_._2).distinctBy(_._1))))
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
            case Seq(n1: TableExpansion, n2: Ref) =>
              logger.debug(s"expandDistinct true: ${n1}, ${n2}")
              true
            case a =>
              logger.debug(s"expandDistinct false: ${a.mkString(",")}")
              false
          }
          d.mapChildren(tr)
      }
      val tree2a = tr(tree).infer()

      /* The result of a query is expanded with the star projections of the tables whose rows it contains. A
       * Union has the row type of its left side, so the left side's projections are used for the rows of both
       * sides. Columns line up by position, so that is correct for them, but anything else in a star projection
       * (literals, computed values, a column used twice) has to be computed by each side (#1559). Under a Union,
       * replace each table that has such elements with a struct that contains one field per element of its star
       * projection, followed by the table's other fields, under a new identity whose expansion reads the fields.
       * Both sides of the Union then have the elements of their star projections at the same positions. */
      def elements(n: Node): Vector[Node] = n match {
        case _: ProductNode | _: TypeMapping | _: OptionApply => n.children.toSeq.toVector.flatMap(elements)
        case _ => Vector(n)
      }
      def replaceElements(n: Node, replacements: IndexedSeq[Node]): Node = {
        var i = -1
        def f(n: Node): Node = n match {
          case _: ProductNode | _: TypeMapping | _: OptionApply => n.mapChildren(f)
          case _ => i += 1; replacements(i)
        }
        f(n)
      }
      def liftElements(t: TableNode): Option[Node] = for {
        (gen, columns) <- tables.get(t.identity)
        fields = t.nodeType.asCollectionType.elementType.structural match {
          case StructType(defs) => defs.toSeq.map(_._1)
          case _ => Nil
        }
        es = elements(columns)
        // The field of each element that is a column, unless an earlier element is the same column
        columnFields = es.zipWithIndex.collect { case (Select(Ref(s), f), i) if s == gen && fields.contains(f) => (f, i) }
          .distinctBy(_._1).map(_.swap).toMap
        if columnFields.size < es.length
      } yield {
        val syms = es.indices.map(i => columnFields.getOrElse(i, new AnonSymbol))
        val others = fields.filterNot(syms.contains)
        val g = new AnonSymbol
        val struct = StructNode(ConstArray.from(syms.zip(es) ++ others.map(f => (f, Select(Ref(gen), f)))).map {
          case (s, n) => (s, n.replace { case Ref(`gen`) => Ref(g) })
        })
        val ts = new AnonTableIdentitySymbol
        tables += ((ts, (gen, replaceElements(columns, syms.map(Select(Ref(gen), _))))))
        Bind(g, t, Pure(struct, ts))
      }
      def liftInUnions(n: Node, inUnion: Boolean): Node = n match {
        case t: TableNode if inUnion => liftElements(t).getOrElse(t)
        case u: Union => u.mapChildren(liftInUnions(_, true))
        case _ => n.mapChildren(liftInUnions(_, inUnion))
      }
      val tree2b = liftInUnions(tree2a, false)
      val tree2 = if (tree2b eq tree2a) tree2a else tree2b.replace { case r: Ref => r.untyped }.infer()
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
