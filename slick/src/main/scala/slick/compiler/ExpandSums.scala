package slick.compiler

import slick.util.{ConstArrayOp, ConstArray}
import slick.{SlickTreeException, SlickException}
import slick.ast._
import Util._
import TypeUtil._

import scala.collection.mutable

/** Expand sum types and their catamorphisms to equivalent product type operations. */
class ExpandSums extends Phase {
  val name = "expandSums"

  def apply(state: CompilerState) =
    if(state.get(Phase.assignUniqueSymbols).map(_.nonPrimitiveOption).getOrElse(true)) state.map(expandSums)
    else state

  val Disc1 = LiteralNode(ScalaBaseType.optionDiscType.optionType, Option(1))
  val DiscNone = LiteralNode(ScalaBaseType.optionDiscType.optionType, None)

  def expandSums(n: Node): Node = {
    var multi = false

    /** Perform the sum expansion on a Node */
    def tr(tree: Node, oldDiscCandidates: Set[(TypeSymbol, List[TermSymbol])]): Node = {
      val discCandidates = oldDiscCandidates ++ (tree match {
        case Filter(_, _, p) => collectDiscriminatorCandidates(p)
        case Bind(_, j: Join, _) => collectDiscriminatorCandidates(j.on)
        case _ => Set.empty
      })
      val tree2 = tree.mapChildren(tr(_, discCandidates), keepType = true)
      val tree3 = tree2 match {
        // Expand multi-column null values in ELSE branches (used by Rep[Option].filter) with correct type
        case IfThenElse(ConstArray(pred, then1 :@ tpe, LiteralNode(None) :@ OptionType(ScalaBaseType.nullType))) =>
          multi = true
          IfThenElse(ConstArray(pred, then1, buildMultiColumnNone(tpe))) :@ tpe

        // Identity OptionFold/OptionApply combination -> remove
        case OptionFold(from, LiteralNode(None) :@ OptionType(ScalaBaseType.nullType), oa @ OptionApply(Ref(s)), gen) if s == gen =>
          silentCast(oa.nodeType, from)

        // Primitive OptionFold representing GetOrElse -> translate to GetOrElse
        case OptionFold(from :@ OptionType.Primitive(_), LiteralNode(v), Ref(s), gen) if s == gen =>
          GetOrElse(from, () => v).infer()

        // Primitive OptionFold -> translate to null check
        case OptionFold(from :@ OptionType.Primitive(_), ifEmpty, map, gen) =>
          val pred = Library.==.typed[Boolean](from, LiteralNode(null))
          val n2 = (ifEmpty, map) match {
            case (LiteralNode(true), LiteralNode(false)) => pred
            case (LiteralNode(false), LiteralNode(true)) => Library.Not.typed[Boolean](pred)
            case _ =>
              val ifDefined = map.replace({
                case r @ Ref(s) if s == gen => silentCast(r.nodeType, from)
              }, keepType = true)
              val ifEmpty2 = silentCast(ifDefined.nodeType.structural, ifEmpty)
              IfThenElse(ConstArray(pred, ifEmpty2, ifDefined))
          }
          n2.infer()

        // Other OptionFold -> translate to discriminator check
        case OptionFold(from, ifEmpty, map, gen) =>
          multi = true
          val left = from.select(ElementSymbol(1)).infer()
          val pred = Library.==.typed[Boolean](left, LiteralNode(null))
          val n2 = (ifEmpty, map) match {
            case (LiteralNode(true), LiteralNode(false)) => pred
            case (LiteralNode(false), LiteralNode(true)) => Library.Not.typed[Boolean](pred)
            case _ =>
              val ifDefined = map.replace({
                case r @ Ref(s) if s == gen => silentCast(r.nodeType, from.select(ElementSymbol(2)).infer())
              }, keepType = true)
              val ifEmpty2 = silentCast(ifDefined.nodeType.structural, ifEmpty)
              if(left == Disc1) ifDefined else IfThenElse(ConstArray(Library.Not.typed[Boolean](pred), ifDefined, ifEmpty2))
          }
          n2.infer()

        // Primitive OptionApply -> leave unchanged
        case n @ OptionApply(_) :@ OptionType.Primitive(_) => n

        // Other OptionApply -> translate to product form
        case n @ OptionApply(ch) =>
          multi = true
          ProductNode(ConstArray(Disc1, silentCast(toOptionColumns(ch.nodeType), ch))).infer()

        // Non-primitive GetOrElse
        // (.get is only defined on primitive Options, but this can occur inside of HOFs like .map)
        case g @ GetOrElse(ch :@ tpe, _) =>
          tpe match {
            case OptionType.Primitive(_) => g
            case _ => throw new SlickException(".get may only be called on Options of top-level primitive types")
          }

        // Option-extended left outer, right outer or full outer join
        case bind @ Bind(bsym, Join(_, _, _, _, jt, _), _) if jt == JoinType.LeftOption || jt == JoinType.RightOption || jt == JoinType.OuterOption =>
          multi = true
          translateJoin(bind, discCandidates)

        case n => n
      }
      val tree4 = fuse(tree3)
      tree4 :@ trType(tree4.nodeType)
    }

    val n2 = tr(n, Set.empty)
    if(multi) expandConditionals(n2) else n2
  }

  /** Translate an Option-extended left outer, right outer or full outer join */
  def translateJoin(bind: Bind, discCandidates: Set[(TypeSymbol, List[TermSymbol])]): Bind = {
    logger.debug("translateJoin", bind)
    val Bind(bsym, (join @ Join(lsym, rsym, left :@ CollectionType(_, leftElemType), right :@ CollectionType(_, rightElemType), jt, on)) :@ CollectionType(cons, elemType), pure) = bind
    val lComplex = !leftElemType.structural.isInstanceOf[AtomicType]
    val rComplex = !rightElemType.structural.isInstanceOf[AtomicType]
    logger.debug(s"Translating join ($jt, complex: $lComplex, $rComplex):", bind)

    // Find an existing column that can serve as a discriminator
    def findDisc(t: Type): Option[List[TermSymbol]] = {
      val global: Set[List[TermSymbol]] = t match {
        case NominalType(ts, exp) =>
          val c = discCandidates.filter { case (t, ss) => t == ts && ss.nonEmpty }.map(_._2)
          logger.debug("Discriminator candidates from surrounding Filter and Join predicates: "+
            c.map(Path.toString).mkString(", "))
          c
        case _ => Set.empty
      }
      def find(t: Type, path: List[TermSymbol]): Vector[List[TermSymbol]] = t.structural match {
        case StructType(defs) => defs.toSeq.flatMap { case (s, t) => find(t, s :: path) }(collection.breakOut)
        case p: ProductType => p.elements.iterator.zipWithIndex.flatMap { case (t, i) => find(t, ElementSymbol(i+1) :: path) }.toVector
        case _: AtomicType => Vector(path)
        case _ => Vector.empty
      }
      val local = find(t, Nil).sortBy { ss =>
        (if(global contains ss) 3 else 1) * (ss.head match {
          case f: FieldSymbol =>
            if(f.options contains ColumnOption.PrimaryKey) -2 else -1
          case _ => 0
        })
      }
      logger.debug("Local candidates: "+local.map(Path.toString).mkString(", "))
      local.headOption
    }

    // Option-extend one side of the join with a discriminator column
    def extend(side: Node, sym: TermSymbol, on: Node): (Node, Node, Boolean) = {
      val extendGen = new AnonSymbol
      val elemType = side.nodeType.asCollectionType.elementType
      val (disc, createDisc) = findDisc(elemType) match {
        case Some(path) =>
          logger.debug("Using existing column "+Path(path)+" as discriminator in "+elemType)
          (FwdPath(extendGen :: path.reverse), true)
        case None =>
          logger.debug("No suitable discriminator column found in "+elemType)
          (Disc1, false)
      }
      val extend :@ CollectionType(_, extendedElementType) = Bind(extendGen, side, Pure(ProductNode(ConstArray(disc, Ref(extendGen))))).infer()
      val sideInCondition = Select(Ref(sym) :@ extendedElementType, ElementSymbol(2)).infer()
      val on2 = on.replace({
        case Ref(s) if s == sym => sideInCondition
      }, bottomUp = true).infer()
      (extend, on2, createDisc)
    }

    // Translate the join depending on JoinType and Option type
    val (left2, right2, on2, jt2, ldisc, rdisc) = jt match {
      case JoinType.LeftOption =>
        val (right2, on2, rdisc) = if(rComplex) extend(right, rsym, on) else (right, on, false)
        (left, right2, on2, JoinType.Left, false, rdisc)
      case JoinType.RightOption =>
        val (left2, on2, ldisc) = if(lComplex) extend(left, lsym, on) else (left, on, false)
        (left2, right, on2, JoinType.Right, ldisc, false)
      case JoinType.OuterOption =>
        val (left2, on2, ldisc) = if(lComplex) extend(left, lsym, on) else (left, on, false)
        val (right2, on3, rdisc) = if(rComplex) extend(right, rsym, on2) else (right, on2, false)
        (left2, right2, on3, JoinType.Outer, ldisc, rdisc)
    }

    // Cast to translated Option type in outer bind
    val join2 :@ CollectionType(_, elemType2) = Join(lsym, rsym, left2, right2, jt2, on2).infer()
    def optionCast(idx: Int, createDisc: Boolean): Node = {
      val ref = Select(Ref(bsym) :@ elemType2, ElementSymbol(idx+1))
      val v = if(createDisc) {
        val protoDisc = Select(ref, ElementSymbol(1)).infer()
        val rest = Select(ref, ElementSymbol(2))
        val disc = IfThenElse(ConstArray(Library.==.typed[Boolean](silentCast(OptionType(protoDisc.nodeType), protoDisc), LiteralNode(null)), DiscNone, Disc1))
        ProductNode(ConstArray(disc, rest))
      } else ref
      silentCast(trType(elemType.asInstanceOf[ProductType].children(idx)), v)
    }
    val ref = ProductNode(ConstArray(optionCast(0, ldisc), optionCast(1, rdisc))).infer()
    val pure2 = pure.replace({
      case Ref(s) if s == bsym => ref

      // Hoist SilentCasts and remove unnecessary ones
      case Library.SilentCast(Library.SilentCast(ch)) :@ tpe => silentCast(tpe, ch)
      case Select(Library.SilentCast(ch), s) :@ tpe => silentCast(tpe, ch.select(s).infer())
    }, bottomUp = true, keepType = true)
    val res = Bind(bsym, join2, pure2).infer()
    logger.debug("Translated join:", res)
    res
  }

  /** Create a SilentCast call unless the type already matches */
  def silentCast(tpe: Type, n: Node): Node = n match {
    case LiteralNode(None) :@ OptionType(ScalaBaseType.nullType) => buildMultiColumnNone(tpe)
    case n :@ tpe2 if tpe2 == tpe => n
    case n =>
      if(tpe == UnassignedType) throw new SlickTreeException("Unexpected UnassignedType for:", n)
      Library.SilentCast.typed(tpe, n).infer()
  }

  /** Create a Node representing a structure of null values of the given Type */
  def buildMultiColumnNone(tpe: Type): Node = (tpe.structural match {
    case ProductType(ch) => ProductNode(ch.map(buildMultiColumnNone))
    case StructType(ch) => StructNode(ch.map { case (sym, t) => (sym, buildMultiColumnNone(t)) })
    case OptionType(ch) => LiteralNode(tpe, None)
    case t => throw new SlickException("Unexpected non-Option type in multi-column None")
  }) :@ tpe

  /** Perform the sum expansion on a Type */
  def trType(tpe: Type): Type = {
    def f(tpe: Type): Type = tpe.mapChildren(f) match {
      case t @ OptionType.Primitive(_) => t
      case OptionType(ch) => ProductType(ConstArray(ScalaBaseType.optionDiscType.optionType, toOptionColumns(ch)))
      case t => t
    }
    val tpe2 = f(tpe)
    logger.debug(s"Translated type: $tpe -> $tpe2")
    tpe2
  }

  /** Strip nominal types and convert all atomic types to OptionTypes */
  def toOptionColumns(tpe: Type): Type = tpe match {
    case NominalType(_, str) => toOptionColumns(str)
    case o @ OptionType(ch) if ch.structural.isInstanceOf[AtomicType] => o
    case t: AtomicType => OptionType(t)
    case t => t.mapChildren(toOptionColumns)
  }

  /** Fuse unnecessary Option operations */
  def fuse(n: Node): Node = n match {
    // Option.map
    case IfThenElse(ConstArray(Library.Not(Library.==(disc, LiteralNode(null))), ProductNode(ConstArray(Disc1, map)), ProductNode(ConstArray(DiscNone, _)))) =>
      ProductNode(ConstArray(disc, map)).infer()
    case n => n
  }

  /** Collect discriminator candidate fields in a predicate. These are all paths below an
    * OptionApply, which indicates their future use under a discriminator guard. */
  def collectDiscriminatorCandidates(n: Node): Set[(TypeSymbol, List[TermSymbol])] = n.collectAll[(TypeSymbol, List[TermSymbol])] {
    case OptionApply(ch) =>
      ch.collect[(TypeSymbol, List[TermSymbol])] { case PathOnTypeSymbol(ts, ss) => (ts, ss) }
  }.toSet

  object PathOnTypeSymbol {
    def unapply(n: Node): Option[(TypeSymbol, List[TermSymbol])] = n match {
      case (n: PathElement) :@ NominalType(ts, _) => Some((ts, Nil))
      case Select(in, s) => unapply(in).map { case (ts, l) => (ts, s :: l) }
      case Library.SilentCast(ch) => unapply(ch)
      case _ => None
    }
  }

  /** Expand multi-column conditional expressions and SilentCasts.
    * Single-column conditionals involving NULL values are optimized away where possible. */
  def expandConditionals(n: Node): Node = {
    val invalid = mutable.HashSet.empty[TypeSymbol]
    def invalidate(n: Node): Unit = invalid ++= n.nodeType.collect { case NominalType(ts, _) => ts }.toSeq

    def tr(n: Node): Node = n.mapChildren(tr, keepType = true) match {
      // Expand multi-column SilentCasts
      case cast @ Library.SilentCast(ch) :@ Type.Structural(ProductType(typeCh)) =>
        invalidate(ch)
        val elems = typeCh.zipWithIndex.map { case (t, idx) => tr(Library.SilentCast.typed(t, ch.select(ElementSymbol(idx+1))).infer()) }
        ProductNode(elems).infer()
      case Library.SilentCast(ch) :@ Type.Structural(StructType(typeCh)) =>
        invalidate(ch)
        val elems = typeCh.map { case (sym, t) => (sym, tr(Library.SilentCast.typed(t, ch.select(sym)).infer())) }
        StructNode(elems).infer()

      // Optimize trivial SilentCasts
      case Library.SilentCast(v :@ tpe) :@ tpe2 if tpe.structural == tpe2.structural =>
        invalidate(v)
        v
      case Library.SilentCast(Library.SilentCast(ch)) :@ tpe => tr(Library.SilentCast.typed(tpe, ch).infer())
      case Library.SilentCast(LiteralNode(None)) :@ (tpe @ OptionType.Primitive(_)) => LiteralNode(tpe, None).infer()

      // Expand multi-column IfThenElse
      case (cond @ IfThenElse(_)) :@ Type.Structural(ProductType(chTypes)) =>
        val ch = ConstArrayOp.from(1 to chTypes.length).map { idx =>
          val sym = ElementSymbol(idx)
          tr(cond.mapResultClauses(n => n.select(sym)).infer())
        }
        ProductNode(ch).infer()
      case (cond @ IfThenElse(_)) :@ Type.Structural(StructType(chTypes)) =>
        val ch = chTypes.map { case (sym, _) =>
          (sym, tr(cond.mapResultClauses(n => n.select(sym)).infer()))
        }
        StructNode(ch).infer()

      // Optimize null-propagating single-column IfThenElse
      case IfThenElse(ConstArray(Library.==(r, LiteralNode(null)), Library.SilentCast(LiteralNode(None)), c @ Library.SilentCast(r2))) if r == r2 => c

      // Fix Untyped nulls in else clauses
      case cond @ IfThenElse(clauses) if (clauses.last match { case LiteralNode(None) :@ OptionType(ScalaBaseType.nullType) => true; case _ => false }) =>
        cond.copy(clauses.init :+ LiteralNode(cond.nodeType, None))

      // Resolve Selects into ProductNodes and StructNodes
      case Select(ProductNode(ch), ElementSymbol(idx)) => ch(idx-1)
      case Select(StructNode(ch), sym) => ch.find(_._1 == sym).get._2

      case n2 @ Pure(_, ts) if n2 ne n =>
        invalid += ts
        n2

      case n => n
    }

    val n2 = tr(n)
    logger.debug("Invalidated TypeSymbols: "+invalid.mkString(", "))
    n2.replace({
      case n: PathElement if n.nodeType.containsSymbol(invalid) => n.untyped
    }, bottomUp = true).infer()
  }
}
