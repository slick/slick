package slick.compiler

import slick.ast.Library.AggregateFunctionSymbol
import slick.{SlickException, SlickTreeException}
import slick.ast._
import slick.ast.QueryParameter.constOp
import slick.ast.Util._
import slick.ast.TypeUtil._
import slick.util.{ConstArray, Ellipsis, ??}

/** This phase merges nested nodes of types Bind, Filter, GroupBy, SortBy, Take, Drop,
  * CollectionCast and Distinct to Comprehension nodes. Nodes can be merged if they occur in the
  * following order:
  *
  * [Source] -> Filter (where) -> GroupBy -> SortBy / (Distinct | Filter (having)) -> Take / Drop
  *
  * Aliasing Binds and CollectionCasts are allowed everywhere in the chain. Any out of order
  * operation starts a new chain with a subquery as the source.
  */
class MergeToComprehensions extends Phase {
  val name = "mergeToComprehensions"

  /** A map from a TypeSymbol and a field on top of it to a new Symbol */
  type Replacements = Map[(TypeSymbol, TermSymbol), TermSymbol]

  type Mappings = ConstArray[((TypeSymbol, TermSymbol), List[TermSymbol])]

  def apply(state: CompilerState) = state.map(n => ClientSideOp.mapResultSetMapping(n, keepType = false) { rsm =>
    rsm.copy(from = convert(rsm.from), map = rsm.map.replace { case r: Ref => r.untyped })
  }.infer())

  def convert(tree: Node): Node = {
    // Find all references into tables so we can convert TableNodes to Comprehensions
    val tableFields =
      tree.collect { case Select(_ :@ NominalType(t: TableIdentitySymbol, _), f) => (t, f) }
        .toSeq.groupBy(_._1).mapValues(_.map(_._2).distinct.toVector)
    logger.debug("Table fields: " + tableFields)

    /** Merge Take, Drop, Bind and CollectionCast into an existing Comprehension */
    def mergeTakeDrop(n: Node, buildBase: Boolean): (Comprehension, Replacements) = n match {
      case Take(f1, count1) =>
        val (c1, replacements1) = mergeTakeDrop(f1, true)
        logger.debug("Merging Take into Comprehension:", Ellipsis(n, List(0)))
        val count2 = applyReplacements(count1, replacements1, c1)
        val fetch2 = c1.fetch match {
          case Some(t) => Some(constOp[Long]("min")(math.min)(t, count2))
          case None => Some(count2)
        }
        val c2 = c1.copy(fetch = fetch2) :@ c1.nodeType
        logger.debug("Merged Take into Comprehension:", c2)
        (c2, replacements1)

      case Drop(f1, count1) =>
        val (c1, replacements1) = mergeTakeDrop(f1, true)
        logger.debug("Merging Drop into Comprehension:", Ellipsis(n, List(0)))
        val count2 = applyReplacements(count1, replacements1, c1)
        val (fetch2, offset2) = (c1.fetch, c1.offset) match {
          case (None,    None   ) => (None, Some(count2))
          case (Some(t), None   ) => (Some(constOp[Long]("max")(math.max)(LiteralNode(0L).infer(), constOp[Long]("-")(_ - _)(t, count2))), Some(count2))
          case (None,    Some(d)) => (None, Some(constOp[Long]("+")(_ + _)(d, count2)))
          case (Some(t), Some(d)) => (Some(constOp[Long]("max")(math.max)(LiteralNode(0L).infer(), constOp[Long]("-")(_ - _)(t, count2))), Some(constOp[Long]("+")(_ + _)(d, count2)))
        }
        val c2 = c1.copy(fetch = fetch2, offset = offset2) :@ c1.nodeType
        logger.debug("Merged Drop into Comprehension:", c2)
        (c2, replacements1)

      case n =>
        mergeCommon(mergeTakeDrop _, mergeSortBy _, n, buildBase, allowFilter = false)
    }

    /** Merge Bind, Filter (as WHERE or HAVING depending on presence of GROUP BY), CollectionCast,
      * SortBy and Distinct into an existing Comprehension. If Distinct is present, no other
      * Distinct or Filter (as HAVING) is allowed. A subquery is created if necessary to avoid
      * this situation. */
    def mergeSortBy(n: Node, buildBase: Boolean): (Comprehension, Replacements) = n match {
      case SortBy(s1, f1, b1) =>
        val (c1, replacements1) = mergeSortBy(f1, true)
        logger.debug("Merging SortBy into Comprehension:", Ellipsis(n, List(0)))
        val b2 = b1.map { case (n, o) => (applyReplacements(n, replacements1, c1), o) }
        val c2 = c1.copy(orderBy = b2 ++ c1.orderBy) :@ c1.nodeType
        logger.debug("Merged SortBy into Comprehension:", c2)
        (c2, replacements1)

      case ForUpdate(s1, f1) =>
        val (c1, replacements1) = mergeSortBy(f1, true)
        logger.debug("Merging ForUpdate into Comprehension:", Ellipsis(n, List(0)))
        val c2 = c1.copy(forUpdate = true) :@ c1.nodeType
        logger.debug("Merged ForUpdate into Comprehension:", c2)
        (c2, replacements1)

      case Distinct(s1, f1, o1) =>
        val (c1, replacements1) = mergeSortBy(f1, true)
        val (c1a, replacements1a) =
          if(c1.distinct.isDefined || c1.having.isDefined) toSubquery(c1, replacements1)
          else (c1, replacements1)
        logger.debug("Merging Distinct into Comprehension:", Ellipsis(n, List(0)))
        val o2 = applyReplacements(o1, replacements1a, c1a)
        val c2 = c1a.copy(distinct = Some(ProductNode(ConstArray(o2)).flatten.infer())) :@ c1a.nodeType
        logger.debug("Merged Distinct into Comprehension:", c2)
        (c2, replacements1a)

      case n =>
        mergeCommon(mergeSortBy _, mergeGroupBy _, n, buildBase)
    }

    /** Merge GroupBy into an existing Comprehension or create new Comprehension from non-grouping Aggregation */
    def mergeGroupBy(n: Node, buildBase: Boolean): (Comprehension, Replacements) = n match {
      case Bind(s1, GroupBy(s2, f1, b1, ts1), Pure(str1, ts2)) =>
        val (c1, replacements1) = mergeFilterWhere(f1, true)
        logger.debug("Merging GroupBy into Comprehension:", Ellipsis(n, List(0, 0)))
        val (c1a, replacements1a, b2a) = {
          val b2 = applyReplacements(b1, replacements1, c1)
          // Check whether groupBy keys containing bind variables are returned for further use
          // and push the current Comprehension into a subquery if this is the case.
          val leakedPaths =
            str1.collect({ case FwdPath(s :: ElementSymbol(1) :: rest) if s == s1 => rest }, stopOnMatch = true)
          val isParam = leakedPaths.nonEmpty && ({
            logger.debug("Leaked paths to GroupBy keys: " + leakedPaths.map(l => ("_" :: l).mkString(".")).mkString(", "))
            val targets = leakedPaths.map(_.foldLeft(b2)(_ select _))
            targets.indexWhere(_.findNode {
              case _: QueryParameter => true
              case n: LiteralNode => n.volatileHint
              case _ => false
            }.isDefined) >= 0
          })
          if(isParam) {
            logger.debug("Pushing GroupBy source into subquery to avoid repeated parameter")
            val (c1a, replacements1a) = toSubquery(c1, replacements1)
            val b2a = applyReplacements(b1, replacements1a, c1a)
            (c1a, replacements1a, b2a)
          } else (c1, replacements1, b2)
        }
        val str2 = str1.replace {
          case Aggregate(_, FwdPath(s :: ElementSymbol(2) :: Nil), v) if s == s1 =>
            applyReplacements(v, replacements1a, c1a).replace {
              case Apply(f: AggregateFunctionSymbol, ConstArray(ch)) :@ tpe =>
                Apply(f, ConstArray(ch match {
                  case StructNode(ConstArray(ch, _*)) => ch._2
                  case n => n
                }))(tpe)
            }
          case FwdPath(s :: ElementSymbol(1) :: rest) if s == s1 =>
            rest.foldLeft(b2a) { case (n, s) => n.select(s) }.infer()
        }
        val c2 = c1a.copy(groupBy = Some(ProductNode(ConstArray(b2a)).flatten), select = Pure(str2, ts2)).infer()
        logger.debug("Merged GroupBy into Comprehension:", c2)
        val StructNode(defs2) = str2
        val replacements = defs2.iterator.map { case (f, _) => (ts2, f) -> f }.toMap
        logger.debug("Replacements are: "+replacements)
        (c2, replacements)

      case n @ Pure(Aggregate(s1, f1, str1), ts) =>
        val (c1, replacements1) = mergeFilterWhere(f1, true)
        logger.debug("Merging Aggregate source into Comprehension:", Ellipsis(n, List(0, 0)))
        val str2 = applyReplacements(str1, replacements1, c1)
        val c2 = c1.copy(select = Pure(str2, ts)).infer()
        logger.debug("Merged Aggregate source into Comprehension:", c2)
        val StructNode(defs) = str2
        val repl = defs.iterator.map { case (f, _) => ((ts, f), f) }.toMap
        logger.debug("Replacements are: "+repl)
        (c2, repl)

      case n => mergeFilterWhere(n, buildBase)
    }

    /** Merge Bind, Filter (as WHERE), CollectionCast into an existing Comprehension */
    def mergeFilterWhere(n: Node, buildBase: Boolean): (Comprehension, Replacements) =
      mergeCommon(mergeFilterWhere _, convertBase _, n, buildBase)

    /** Build a base Comprehension from a non-Comprehension base (e.g. Join) or a sub-Comprehension */
    def convertBase(n: Node, buildBase: Boolean): (Comprehension, Replacements) = {
      val (n2, mappings) = {
        if(buildBase) createSourceOrTopLevel(n)
        else createSource(n).getOrElse(throw new SlickTreeException("Cannot convert node to SQL Comprehension", n))
      }
      buildSubquery(n2, mappings)
    }

    /** Convert a Node for use as a source in a Join. Joins and TableNodes are not converted to
      * Comprehensions. Instead of returning regular replacements, the method returns identity
      * mappings for all fields in the source. */
    def createSource(n: Node): Option[(Node, Mappings)] = n match {
      case t: TableNode =>
        logger.debug("Creating source from TableNode:", t)
        val mappings = ConstArray.from(tableFields.getOrElse(t.identity, Seq.empty).map(f => ((t.identity: TypeSymbol, f), f :: Nil)))
        logger.debug("Mappings are: "+mappings)
        Some((t, mappings))
      case p @ Pure(StructNode(defs), ts) =>
        logger.debug("Creating source from Pure:", p)
        val mappings = defs.map { case (f, _) => ((ts, f), f :: Nil) }
        logger.debug("Mappings are: "+mappings)
        Some((p, mappings))
      case j @ Join(ls, rs, l1, r1, jt, on1) =>
        logger.debug(s"Creating source from Join $ls/$rs:", j)
        val (l2 @ (_ :@ CollectionType(_, ltpe)), lmap) = dealias(l1)(createSourceOrTopLevel)
        val (r2 @ (_ :@ CollectionType(_, rtpe)), rmap) = dealias(r1)(createSourceOrTopLevel)
        logger.debug(s"Converted left side of Join $ls/$rs:", l2)
        logger.debug(s"Converted right side of Join $ls/$rs:", r2)
        // Detect and remove empty join sides
        val noCondition = on1 == LiteralNode(true).infer()
        val noLeft = l2 match {
          case Pure(StructNode(ConstArray()), _) => true
          case _ => false
        }
        val noRight = r2 match {
          case Pure(StructNode(ConstArray()), _) => true
          case _ => false
        }
        if(noLeft && noCondition) {
          Some((r2, rmap))
        } else if(noRight && noCondition) {
          Some((l2, lmap))
        } else {
          val mappings =
            lmap.map { case (key, ss) => (key, ElementSymbol(1) :: ss )} ++
            rmap.map { case (key, ss) => (key, ElementSymbol(2) :: ss )}
          val mappingsM = mappings.iterator.toMap
          logger.debug(s"Mappings for `on` clause in Join $ls/$rs: "+mappingsM)
          val on2 = on1.replace({
            case p @ FwdPathOnTypeSymbol(ts, _ :: s :: Nil) =>
              //logger.debug(s"Finding ($ts, $s)")
              mappingsM.get((ts, s)) match {
                case Some(ElementSymbol(idx) :: ss) =>
                  //logger.debug(s"Found $idx :: $ss")
                  FwdPath((if(idx == 1) ls else rs) :: ss)
                case _ => p
              }
          }, bottomUp = true).infer(
              scope = Type.Scope(j.leftGen -> l2.nodeType.asCollectionType.elementType) +
                (j.rightGen -> r2.nodeType.asCollectionType.elementType))
          logger.debug(s"Transformed `on` clause in Join $ls/$rs:", on2)
          val j2 = j.copy(left = l2, right = r2, on = on2).infer()
          logger.debug(s"Created source from Join $ls/$rs:", j2)
          Some((j2, mappings))
        }
      case n => None
    }

    /** Create a source node, or alternatively a top-level node (possibly lifting the node into a
      * subquery) if it is not a valid source. */
    def createSourceOrTopLevel(n: Node): (Node, Mappings) = createSource(n).getOrElse {
      logger.debug("Creating subquery from top-level:", n)
      createTopLevel(n)
    }

    /** Create a Union or Comprehension (suitable for the top level of a query). */
    def createTopLevel(n: Node): (Node, Mappings) = n match {
      case u @ Union(l1, r1, all) =>
        logger.debug("Converting Union:", Ellipsis(u, List(0), List(1)))
        val (l2, rep1) = createTopLevel(l1)
        val (r2, rep2) = createTopLevel(r1)
        val u2 = u.copy(left = l2, right = r2).infer()
        logger.debug("Converted Union:", u2)
        (u2, rep1)

      case Subquery(n, _) =>
        createTopLevel(n)

      case n =>
        val (c, rep) = mergeTakeDrop(n, false)
        val mappings = ConstArray.from(rep.mapValues(_ :: Nil))
        logger.debug("Mappings are: "+mappings)
        val c2 = c.select match {
          // Ensure that the select clause is non-empty
          case Pure(StructNode(ConstArray.empty), _) =>
            c.copy(select = Pure(StructNode(ConstArray((new AnonSymbol, LiteralNode(1)))))).infer()
          case _ => c
        }
        (c2, mappings)
    }

    def convert1(n: Node): Node = n match {
      case CollectionCast(_, _) =>
        n.mapChildren(convert1, keepType = true)
      case n :@ Type.Structural(CollectionType(cons, el)) =>
        convertOnlyInScalar(createTopLevel(n)._1)
      case a: Aggregate =>
        logger.debug("Merging Aggregate into Comprehension:", Ellipsis(a, List(0)))
        val (c1, rep) = mergeFilterWhere(a.from, true)
        val sel2 = applyReplacements(a.select, rep, c1)
        val c2 = c1.copy(select = Pure(sel2)).infer()
        val c3 = convertOnlyInScalar(c2)
        val res = Library.SilentCast.typed(a.nodeType, c3).infer()
        logger.debug("Merged Aggregate into Comprehension as:", res)
        res
      case n =>
        n.mapChildren(convert1, keepType = true)
    }

    def convertOnlyInScalar(n: Node): Node = n match {
      case n :@ Type.Structural(CollectionType(_, _)) =>
        n.mapChildren(convertOnlyInScalar, keepType = true)
      case n => convert1(n)
    }

    val tree2 :@ CollectionType(cons2, _) = convert1(tree)
    val cons1 = tree.nodeType.asCollectionType.cons
    if(cons2 != cons1) CollectionCast(tree2, cons1).infer()
    else tree2
  }

  /** Lift a valid top-level or source Node into a subquery */
  def buildSubquery(n: Node, mappings: Mappings): (Comprehension, Replacements) = {
    logger.debug("Building new Comprehension from:", n)
    val newSyms = mappings.map(x => (x, new AnonSymbol))
    val s = new AnonSymbol
    val struct = StructNode(newSyms.map { case ((_, ss), as) => (as, FwdPath(s :: ss)) })
    val pid = new AnonTypeSymbol
    val res = Comprehension(s, n, select = Pure(struct, pid)).infer()
    logger.debug("Built new Comprehension:", res)
    val replacements = newSyms.iterator.map { case (((ts, f), _), as) => ((ts, f), as) }.toMap
    logger.debug("Replacements are: "+replacements)
    (res, replacements)
  }

  def toSubquery(n: Comprehension, r: Replacements): (Comprehension, Replacements) =
    buildSubquery(n, ConstArray.from(r.mapValues(_ :: Nil)))

  /** Merge the common operations Bind, Filter and CollectionCast into an existing Comprehension.
    * This method is used at different stages of the pipeline. If the Comprehension already contains
    * a Distinct clause, it is pushed into a subquery. */
  def mergeCommon(rec: (Node, Boolean) => (Comprehension, Replacements), parent: (Node, Boolean) => (Comprehension, Replacements),
                  n: Node, buildBase: Boolean,
                  allowFilter: Boolean = true): (Comprehension, Replacements) = n match {
    case Bind(s1, f1, Pure(StructNode(defs1), ts1)) if !f1.isInstanceOf[GroupBy] =>
      val (c1, replacements1) = rec(f1, true)
      logger.debug("Merging Bind into Comprehension as 'select':", Ellipsis(n, List(0)))
      val defs2 = defs1.map { case (s, d) => (s, applyReplacements(d, replacements1, c1)) }
      val c2 = c1.copy(select = Pure(StructNode(defs2), ts1)).infer()
      logger.debug("Merged Bind into Comprehension as 'select':", c2)
      val replacements = defs2.iterator.map { case (f, _) => (ts1, f) -> f }.toMap
      logger.debug("Replacements are: "+replacements)
      (c2, replacements)

    case Filter(s1, f1, p1) if allowFilter =>
      val (c1, replacements1) = rec(f1, true)
      val (c1a, replacements1a) =
        if(c1.distinct.isDefined) toSubquery(c1, replacements1)
        else (c1, replacements1)
      logger.debug("Merging Filter into Comprehension:", Ellipsis(n, List(0)))
      val p2 = applyReplacements(p1, replacements1a, c1a)
      val c2 =
        if(c1a.groupBy.isEmpty) c1a.copy(where = Some(c1a.where.fold(p2)(and(_, p2)).infer())) :@ c1a.nodeType
        else c1a.copy(having = Some(c1a.having.fold(p2)(and(p2, _)).infer())) :@ c1a.nodeType
      logger.debug("Merged Filter into Comprehension:", c2)
      (c2, replacements1a)

    case CollectionCast(ch, _) =>
      rec(ch, buildBase)

    case n => parent(n, buildBase)
  }

  def and(p1: Node, p2: Node): Node = {
    val t1 = p1.nodeType.structural
    Library.And.typed(if(t1.isInstanceOf[OptionType]) t1 else p2.nodeType.structural, p1, p2)
  }

  /** Remove purely aliasing `Bind` mappings, apply the conversion to the source, then inject the
    * mappings back into the source's mappings. */
  def dealias(n: Node)(f: Node => (Node, Mappings)): (Node, Mappings) = {
    def isAliasing(base: TermSymbol, defs: ConstArray[(TermSymbol, Node)]) = {
      val r = defs.forall {
        case (_, FwdPath(s :: _)) if s == base => true
        case _ => false
      }
      logger.debug("Bind from "+base+" is aliasing: "+r)
      r
    }
    n match {
      case Bind(_, _: GroupBy, _) => f(n) // we need this Bind for the GroupBy transformation
      case Bind(s, from, Pure(StructNode(defs), ts1)) if isAliasing(s, defs) =>
        val (n2, map1) = dealias(from)(f)
        logger.debug("Recombining aliasing Bind mappings "+defs)
        val map1M = map1.iterator.toMap
        val map2 = defs.map { case (f1, p) =>
          val sel = p.findNode {
            case Select(_ :@ NominalType(_, _), _) => true
            case _ => false
          }.getOrElse(throw new SlickTreeException("Missing path on top of TypeSymbol in:", p))
          val Select(_ :@ NominalType(ts2, _), f2) = sel
          (ts1, f1) -> map1M((ts2, f2))
        }
        (n2, map2)
      case n => f(n)
    }
  }

  /** Apply the replacements and current selection of a Comprehension to a new Node that
    * will be merged into the Comprehension. */
  def applyReplacements(n1: Node, r: Replacements, c: Comprehension): Node = {
    val Pure(StructNode(base), _) = c.select
    val baseM = base.iterator.toMap
    n1.replace({ case n @ Select(_ :@ NominalType(ts, _), s) =>
      r.get((ts, s)) match {
        case Some(s2) => baseM(s2)
        case None => n
      }
    }, bottomUp = true, keepType = true)
  }

  object FwdPathOnTypeSymbol {
    def unapply(n: Node): Option[(TypeSymbol, List[TermSymbol])] = n match {
      case (n: PathElement) :@ NominalType(ts, _) => Some((ts, List(n.sym)))
      case Select(in, s) => unapply(in).map { case (ts, l) => (ts, l :+ s) }
      case _ => None
    }
  }
}
