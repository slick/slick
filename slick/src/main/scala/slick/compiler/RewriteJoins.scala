package slick.compiler

import slick.util.Ellipsis
import slick.{SlickTreeException, SlickException}
import slick.ast._
import Util._
import TypeUtil._

import scala.collection.mutable.ArrayBuffer

/** Rewrite monadic joins to applicative joins. After this phase all `Bind` nodes are of the
  * form `Bind(_, _, Pure(_, _))` (i.e. `flatMap` has been reduced to `map`). */
class RewriteJoins extends Phase {
  val name = "rewriteJoins"

  def apply(state: CompilerState) = state.map(tr _)

  def tr(n: Node): Node = n.nodeMapChildren(tr, keepType = true) match {
    case Bind(s1, f1, Bind(s2, Filter(s3, f2, pred), select)) =>
      logger.debug("Hoisting flatMapped Filter from:", Ellipsis(n, List(0), List(1, 0, 0)))
      val sn, sj1, sj2 = new AnonSymbol
      val j = Join(sj1, sj2, f1, f2.replace {
        case Ref(s) if s == s1 => Ref(sj1) :@ f1.nodeType.asCollectionType.elementType
      }, JoinType.Inner, pred.replace {
        case Ref(s) if s == s1 => Ref(sj1) :@ f1.nodeType.asCollectionType.elementType
        case Ref(s) if s == s3 => Ref(sj2) :@ f2.nodeType.asCollectionType.elementType
      }).nodeWithComputedType()
      val refSn = Ref(sn) :@ j.nodeType.asCollectionType.elementType
      val ref1 = Select(refSn, ElementSymbol(1))
      val ref2 = Select(refSn, ElementSymbol(2))
      val sel2 = select.replace {
        case Ref(s) :@ tpe if s == s1 => ref1 :@ tpe
        case Ref(s) :@ tpe if s == s2 => ref2 :@ tpe
      }
      val res = Bind(sn, hoistFilters(j), sel2).nodeWithComputedType()
      logger.debug("Hoisted flatMapped Filter in:", Ellipsis(res, List(0, 0), List(0, 1)))
      flattenAliasingMap(res)

    case n @ Bind(s1, f1, Bind(s2, j: Join, select)) =>
      logger.debug("Hoisting flatMapped Join from:", Ellipsis(n, List(0), List(1, 0)))
      val sn, sj1, sj2 = new AnonSymbol
      val j2 = j.replace {
        case Ref(s) :@ tpe if s == s1 => Ref(sj1) :@ tpe
      }
      val oj = Join(sj1, sj2, f1, j2, JoinType.Inner, LiteralNode(true)).nodeWithComputedType()
      val refSn = Ref(sn) :@ oj.nodeType.asCollectionType.elementType
      val ref1 = Select(refSn, ElementSymbol(1))
      val ref2 = Select(refSn, ElementSymbol(2))
      val sel2 = select.replace {
        case Ref(s) :@ tpe if s == s1 => ref1 :@ tpe
        case Ref(s) :@ tpe if s == s2 => ref2 :@ tpe
      }
      val oj2 = hoistFilters(oj)
      val (oj3, m) = eliminateIllegalRefs(oj2, Set.empty, sn)
      val oj4 = rearrangeJoinConditions(oj3)
      val sel3 = if(m.isEmpty) sel2 else sel2.replace {
        case p @ FwdPath(r1 :: rest) if r1 == sn && m.contains(rest) => m(rest)
      }
      val res = Bind(sn, oj4, sel3).nodeWithComputedType()
      logger.debug("Hoisted flatMapped Join in:", Ellipsis(res, List(0, 0)))
      flattenAliasingMap(res)

    case n @ Bind(s1, f1, Bind(s2, f2, select)) =>
      logger.debug("Unnesting Bind from:", Ellipsis(n, List(0)))
      val sn, sj1, sj2 = new AnonSymbol
      val j = Join(sj1, sj2, f1, f2.replace {
        case Ref(s) if s == s1 => Ref(sj1) :@ f1.nodeType.asCollectionType.elementType
      }, JoinType.Inner, LiteralNode(true)).nodeWithComputedType()
      val refSn = Ref(sn) :@ j.nodeType.asCollectionType.elementType
      val ref1 = Select(refSn, ElementSymbol(1))
      val ref2 = Select(refSn, ElementSymbol(2))
      val sel2 = select.replace {
        case Ref(s) :@ tpe if s == s1 => ref1 :@ tpe
        case Ref(s) :@ tpe if s == s2 => ref2 :@ tpe
      }
      val res = Bind(sn, hoistFilters(j), sel2).nodeWithComputedType()
      logger.debug("Unnested Bind in:", Ellipsis(res, List(0, 0)))
      flattenAliasingMap(res)

    case n @ Bind(s1, p @ Pure(f1, _), sel1) =>
      logger.debug("Inlining Pure 'from' in:", n)
      val res = Bind(s1, Pure(StructNode(Vector.empty)).nodeWithComputedType(), sel1.replace({
        case FwdPath(s :: rest) if s == s1 => rest.foldLeft(f1) { case (n, s) => n.select(s) }
      }, keepType = true)) :@ n.nodeType
      logger.debug("Inlined Pure 'from' in:", res)
      res

    case b: Bind => flattenAliasingMap(b)

    case n => n
  }

  /** Hoist `Filter` nodes in `Join` generators into join predicates. */
  def hoistFilters(j: Join): Join = {
    def hoist(ts: Symbol, n: Node): (Node, Option[Node]) = (n match {
      case b: Bind => hoistFilterFromBind(b)._1
      case n => n
    }) match {
      case Filter(s, f, p) =>
        val p2 = p.replace({ case Ref(rs) :@ tpe if rs == s => Ref(ts) :@ tpe }, keepType = true)
        val (f2, pOpt) = hoist(ts, f)
        (f2, Some(and(pOpt, p2)))
      case n => (n, None)
    }
    val (l1, p1Opt) = hoist(j.leftGen, j.left)
    val (r1, p2Opt) = hoist(j.rightGen, j.right)
    if((l1 eq j.left) && (r1 eq j.right)) j
    else {
      val j2 = j.copy(left = l1, right = r1, on = and(p1Opt, and(p2Opt, j.on))) :@ j.nodeType
      logger.debug("Hoisting join filters from:", j)
      logger.debug("Hoisted join filters in:", j2)
      j2
    }
  }

  /** Recursively hoist `Filter` out of of `Bind(_, Filter, Pure(StructNode))`. Returns the possibly
    * modified tree plus a set of invalidated TypeSymbols (non-empty if additional columns
    * have to be added to the base projection for the filters). */
  def hoistFilterFromBind(b: Bind): (Node, Set[TypeSymbol]) = {
    (b.from match {
      case b2: Bind => hoistFilterFromBind(b2)
      case n => (n, Set.empty[TypeSymbol])
    }) match {
      case (Filter(fs1, from1, pred1), tss1) =>
        logger.debug("Hoisting Filter out of Bind from:", b)
        val sRefs = pred1.collect({ case p @ FwdPath(s :: rest) if s == fs1 => (p, FwdPath(b.generator :: rest)) }, stopOnMatch = true)
        val Bind(_, _, Pure(StructNode(struct1), pts)) = b
        val foundRefs = sRefs.map { case (p, pOnBGen) =>
          (p, (pOnBGen, /*None: Option[Symbol]*/ struct1.find { case (s, n) => pOnBGen == n }.map(_._1) ))
        }.toMap
        logger.debug("Found references in predicate: "+foundRefs.mkString(", "))
        val newDefs = foundRefs.filter(_._2._2.isEmpty).map { case (p, (pOnBGen, _)) => (p, (pOnBGen, new AnonSymbol)) }
        logger.debug("New references for predicate: "+newDefs.mkString(", "))
        val allRefs = foundRefs.collect { case (p, (_, Some(s))) => (p, s) } ++ newDefs.map { case (p, (_, s)) => (p, s) }
        logger.debug("All reference mappings for predicate: "+allRefs.mkString(", "))
        val (sel, tss) =
          if(newDefs.isEmpty) (b.select, tss1)
          else (Pure(StructNode(struct1 ++ newDefs.map { case (_, (pOnGen, s)) => (s, pOnGen) }), pts), tss1 + pts)
        val fs = new AnonSymbol
        val pred = pred1.replace {
          case p : Select => allRefs.get(p).map(s => Select(Ref(fs) :@ b.nodeType.asCollectionType.elementType, s) :@ p.nodeType).getOrElse(p)
        }
        val res = Filter(fs, Bind(b.generator, from1, sel), pred).nodeWithComputedType()
        logger.debug("Hoisted Filter out of Bind (invalidated: "+tss.mkString(", ")+") in:", res)
        (res, tss)
      case _ => (b, Set.empty)
    }
  }

  /** Recursively push refs from the right-hand side of a Join to the left-hand side out the join.
    * This is only possible when they occur in a a mapping `Bind(_, _, Pure(StructNode))` directly
    * at the RHS of a Join. Returns the (possibly transformed) Join and replacements for forward
    * paths into it.
    *
    * TODO: If the remainder of the mapping Bind is purely aliasing, eliminate it entirely. */
  def eliminateIllegalRefs(j: Join, illegal: Set[Symbol], outsideRef: Symbol): (Join, Map[List[Symbol], Node]) = {
    logger.debug("Trying to eliminate illegal refs ["+illegal.mkString(", ")+"] from:", j)
    // Pull defs to one of `illegal` out of `sn`, creating required refs to `ok` instead
    def pullOut(sn: StructNode, ok: Symbol, illegal: Set[Symbol]): (StructNode, Map[Symbol, Node]) = {
      val (illegalDefs, legalDefs) = sn.elements.partition(_._2.hasRefToOneOf(illegal))
      if(illegalDefs.isEmpty) (sn, Map.empty)
      else {
        logger.debug("Pulling refs to ["+illegal.mkString(", ")+"] with OK base "+ok+" out of:", sn)
        val requiredOkPaths = illegalDefs.flatMap(_._2.collect { case p @ FwdPath(s :: _) if s == ok => p }).toSet
        val existingOkDefs = legalDefs.collect { case (s, p @ FwdPath(s2 :: _)) if s2 == ok => (p, s) }.toMap
        val createDefs = (requiredOkPaths -- existingOkDefs.keySet).map(p => (new AnonSymbol, p)).toMap
        val sn2 = StructNode(legalDefs ++ createDefs)
        logger.debug("Pulled refs out of:", sn2)
        val replacements = (existingOkDefs ++ createDefs.map { case (s, n) => (n,s) }).toMap
        def rebase(n: Node): Node = n.replace({
          case p @ FwdPath(s :: _) :@ tpe if s == ok => Ref(replacements(p)) :@ tpe
        }, keepType = true)
        val rebasedIllegalDefs = illegalDefs.map { case (s, n) => (s, rebase(n)) }
        logger.debug("Rebased illegal defs are:", StructNode(rebasedIllegalDefs))
        (sn2, rebasedIllegalDefs.toMap)
      }
    }
    def trChild(n: Node, illegal: Set[Symbol], outsideRef: Symbol): (Node, Map[List[Symbol], Node]) = n match {
      case jch: Join => eliminateIllegalRefs(jch, illegal, outsideRef)
      case b @ Bind(s1, from, Pure(sn1 @ StructNode(defs), ts)) =>
        val (sn2, pulled) = pullOut(sn1, s1, illegal)
        if(sn2 eq sn1) (b, Map.empty)
        else {
          val b2 = b.copy(select = Pure(sn2, ts)).nodeWithComputedType()
          (b2, pulled.map { case (s, n) => (s :: Nil, n) })
        }
      case n => (n, Map.empty)
    }
    val (l1, l1m) = trChild(j.left, illegal, j.leftGen)
    val (r1, r1m) = trChild(j.right, illegal + j.leftGen, j.rightGen)
    if(l1m.isEmpty && r1m.isEmpty) (j, Map.empty)
    else {
      val on1 = j.on.replace({
        case p @ FwdPath(r1 :: rest) if r1 == j.leftGen && l1m.contains(rest) => l1m(rest)
        case p @ FwdPath(r1 :: rest) if r1 == j.rightGen && r1m.contains(rest) => r1m(rest)
      }, keepType = true, bottomUp = true)
      val j2 = j.copy(left = l1, right = r1, on = on1).nodeWithComputedType()
      logger.debug("Eliminated illegal refs ["+illegal.mkString(", ")+"] in:", j2)
      val m = l1m.map { case (p, n) => (ElementSymbol(1) :: p, n) } ++
        r1m.map { case (p, n) => (ElementSymbol(2) :: p, n) }
      val m2 = m.mapValues(_.replace({
        case Ref(s) :@ tpe if s == j.leftGen => Select(Ref(outsideRef) :@ j.nodeType, ElementSymbol(1)) :@ tpe
        case Ref(s) :@ tpe if s == j.rightGen => Select(Ref(outsideRef) :@ j.nodeType, ElementSymbol(2)) :@ tpe
      }, keepType = true))
      if(logger.isDebugEnabled) m2.foreach { case (p, n) =>
        logger.debug("Replacement for "+FwdPath.toString(p)+":", n)
      }
      (j2, m2)
    }
  }

  /** In a `Join(s1, _, _, Join(_, _, _, _, JoinType.Inner, on2), JoinType.Inner, on1)` where parts
    * of `on2` refer to `s1`, merge them into `on1`. Nested joins are processed recursively. The
    * same is done in the opposite direction, pushing predicates down into sub-joins if they only
    * reference one side of the join. */
  def rearrangeJoinConditions(j: Join): Join = j match {
    case Join(s1, s2, _, j2a @ Join(_, _, _, _, JoinType.Inner, _), JoinType.Inner, on1) =>
      val j2b = rearrangeJoinConditions(j2a)
      val (on1Down, on1Keep) = splitConjunctions(on1).partition(p => p.hasRefTo(s2) && !p.hasRefTo(s1))
      val (on2Up, on2Keep) = splitConjunctions(j2b.on).partition(_.hasRefTo(s1))
      if(on1Down.nonEmpty || on2Up.nonEmpty) {
        val refS2 = Ref(s2) :@ j2b.nodeType.asCollectionType.elementType
        val on1b = and(on1Keep ++ on2Up.map(_.replace({
          case Ref(s) :@ tpe if s == j2b.leftGen => Select(refS2, ElementSymbol(1)) :@ tpe
          case Ref(s) :@ tpe if s == j2b.rightGen => Select(refS2, ElementSymbol(2)) :@ tpe
        }, keepType = true)))
        val on2b = and(on1Down.map(_.replace({
          case Select(Ref(s), ElementSymbol(i)) :@ tpe if s == s2 =>
            Ref(if(i == 0) j2b.leftGen else j2b.rightGen) :@ tpe
        }, keepType = true)) ++ on2Keep)
        val j2c = j2b.copy(on = on2b) :@ j2b.nodeType
        val res = j.copy(right = j2c, on = on1b) :@ j.nodeType
        logger.debug("Rearranged join conditions in:", res)
        res
      } else if(j2b eq j2a) j
      else j.copy(right = j2b) :@ j.nodeType
    case j => j
  }

  /** Merge nested mapping operations of the form `Bind(_, Bind(_, _, Pure(StructNode(p1), _)), Pure(StructNode(p2), _))`
    * into a single Bind, provided that each element of either p1 or p2 contains not more than one path.
    * This transformation is not required for the correctness of join rewriting but it keeps the
    * tree smaller to speed up subsequent phases. */
  def flattenAliasingMap(b: Bind): Bind = b match {
    case Bind(s1, Bind(s2, f, Pure(StructNode(p1), ts1)), Pure(StructNode(p2), ts2)) =>
      def isAliasing(s: Seq[(Symbol, Node)]) = s.forall { case (_, n) =>
        n.collect({ case Path(_) => true }, stopOnMatch = true).length <= 1
      }
      val a1 = isAliasing(p1)
      if(a1 || isAliasing(p2)) {
        logger.debug(s"Bind(${if(a1) s1 else s2}) is aliasing. Merging Bind($s1, Bind($s2)) to Bind($s2)")
        val m = p1.toMap
        Bind(s2, f, Pure(StructNode(p2.map {
          case (f1, n) => (f1, n.replace({
            case Select(Ref(s), f2) if s == s1 => m(f2)
          }, keepType = true))
        }), ts2)).nodeWithComputedType()
      } else b
    case b => b
  }

  def splitConjunctions(n: Node): IndexedSeq[Node] = {
    val b = new ArrayBuffer[Node]
    def f(n: Node): Unit = n match {
      case Library.And(l, r) => f(l); f(r)
      case LiteralNode(t) if t == true =>
      case n => b += n
    }
    f(n)
    b
  }

  def and(ns: IndexedSeq[Node]): Node = ns.length match {
    case 0 => LiteralNode(true)
    case 1 => ns.head
    case _ => ns.reduceLeft { (p1, p2) =>
      val tpe1 = p1.nodeType.structural
      val tpe2 = p2.nodeType.structural
      val tpe = if(tpe1.isInstanceOf[OptionType]) tpe1 else tpe2
      Library.And.typed(tpe, p1, p2)
    }
  }

  def and(p1Opt: Option[Node], p2: Node): Node = p1Opt match {
    case Some(p1) =>
      val tpe1 = p1.nodeType.structural
      val tpe2 = p2.nodeType.structural
      val tpe = if(tpe1.isInstanceOf[OptionType]) tpe1 else tpe2
      Library.And.typed(tpe, p1, p2)
    case None => p2
  }
}
