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

      /* The result of a Union of two tables is expanded with the left table's star projection only. That is
       * correct when both projections consist of columns, which line up by position, but anything else
       * (literals, computed values) has to be computed by each side (#1559). For such a Union, turn both sides
       * into structs of the left table's fields plus one extra field per non-column element of the star
       * projections, and give the Union a new table identity whose expansion reads the extra fields. */
      def isColumn(gen: TermSymbol, n: Node) = n match {
        case Select(Ref(s), _) => s == gen
        case _ => false
      }
      // Pair up the elements of two star projections, or None if their structures differ
      def pairElements(l: Node, r: Node): Option[Vector[(Node, Node)]] = (l, r) match {
        case (ProductNode(ls), ProductNode(rs)) if ls.length == rs.length =>
          ls.toSeq.zip(rs.toSeq).foldLeft(Option(Vector.empty[(Node, Node)])) { case (acc, (a, b)) =>
            for (v <- acc; w <- pairElements(a, b)) yield v ++ w
          }
        case (TypeMapping(lc, _, _), TypeMapping(rc, _, _)) => pairElements(lc, rc)
        case (_: ProductNode | _: TypeMapping, _) | (_, _: ProductNode | _: TypeMapping) => None
        case _ => Some(Vector((l, r)))
      }
      def replaceElements(n: Node, elements: IndexedSeq[Node]): Node = {
        var i = -1
        def f(n: Node): Node = n match {
          case _: ProductNode | _: TypeMapping => n.mapChildren(f)
          case _ => i += 1; elements(i)
        }
        f(n)
      }
      def tableExpansion(n: Node) = n.nodeType match {
        case CollectionType(_, NominalType(ts: TableIdentitySymbol, StructType(fields))) =>
          tables.get(ts).map { case (gen, columns) => (gen, columns, fields.toSeq.map(_._1)) }
        case _ => None
      }
      def expandUnion(u: Union): Option[Node] = for {
        (lGen, lColumns, lFields) <- tableExpansion(u.left)
        (rGen, rColumns, rFields) <- tableExpansion(u.right)
        pairs <- pairElements(lColumns, rColumns)
        if pairs.exists { case (l, r) => !isColumn(lGen, l) || !isColumn(rGen, r) }
        // The first position of each left field in the star projection
        fieldPositions = pairs.zipWithIndex.collect {
          case ((Select(Ref(s), f), _), i) if s == lGen && lFields.contains(f) => (f, i)
        }.distinctBy(_._1).toMap
        extras = pairs.indices.filterNot(fieldPositions.values.toSet).map(i => (i, new AnonSymbol))
        // Left fields that are not in the star projection fall back to the right field at the same index
        rValues <- lFields.zipWithIndex.foldLeft(Option(Vector.empty[Node])) { case (acc, (f, i)) =>
          for {
            v <- acc
            n <- fieldPositions.get(f).map(pairs(_)._2).orElse(rFields.lift(i).map(Select(Ref(rGen), _)))
          } yield v :+ n
        }
      } yield {
        val ts = new AnonTableIdentitySymbol
        val syms = ConstArray.from(lFields ++ extras.map(_._2))
        def side(from: Node, gen: TermSymbol, values: Seq[Node], identity: TypeSymbol) = {
          val g = new AnonSymbol
          val struct = StructNode(syms.zip(ConstArray.from(values)).map { case (s, n) =>
            (s, n.replace { case Ref(`gen`) => Ref(g) })
          })
          Bind(g, from, Pure(struct, identity)).infer()
        }
        val left = side(u.left, lGen, lFields.map(Select(Ref(lGen), _)) ++ extras.map(e => pairs(e._1)._1), ts)
        val right = side(u.right, rGen, rValues ++ extras.map(e => pairs(e._1)._2), new AnonTypeSymbol)
        val extraSyms = extras.toMap
        tables += ((ts, (lGen, replaceElements(lColumns, pairs.indices.map { i =>
          extraSyms.get(i).fold(pairs(i)._1)(Select(Ref(lGen), _))
        }))))
        u.copy(left = left, right = right).infer()
      }
      var expandedUnion = false
      val tree2b = tree2a.replace({
        case u: Union => expandUnion(u).fold[Node](u) { n => expandedUnion = true; n }
      }, bottomUp = true)
      val tree2 = if (!expandedUnion) tree2a else tree2b.replace { case r: Ref => r.untyped }.infer()
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
