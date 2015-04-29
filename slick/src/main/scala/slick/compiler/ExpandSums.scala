package slick.compiler

import slick.SlickException
import slick.ast._
import Util._
import TypeUtil._

/** Expand sum types and their catamorphisms to equivalent product type operations. */
class ExpandSums extends Phase {
  val name = "expandSums"

  def apply(state: CompilerState) = state.map(tr)

  val Disc1 = LiteralNode(ScalaBaseType.optionDiscType.optionType, Option(1))
  val DiscNone = LiteralNode(ScalaBaseType.optionDiscType.optionType, None)

  /** Perform the sum expansion on a Node */
  def tr(tree: Node): Node = {
    val tree2 = tree.nodeMapChildren(tr, keepType = true)
    val tree3 = tree2 match {
      // Expand multi-column null values in ELSE branches (used by Rep[Option].filter) with correct type
      case IfThenElse(IndexedSeq(pred, then1 :@ tpe, LiteralNode(None) :@ OptionType(ScalaBaseType.nullType))) =>
        IfThenElse(Vector(pred, then1, buildMultiColumnNone(tpe))).nodeTyped(tpe)

      // Primitive OptionFold representing GetOrElse -> translate to GetOrElse
      case OptionFold(from :@ OptionType.Primitive(_), LiteralNode(v), Ref(s), gen) if s == gen =>
        GetOrElse(from, () => v).nodeWithComputedType()

      // Primitive OptionFold -> translate to null check
      case OptionFold(from :@ OptionType.Primitive(_), ifEmpty, map, gen) =>
        val pred = Library.==.typed[Boolean](from, LiteralNode(null))
        val n2 = (ifEmpty, map) match {
          case (LiteralNode(true), LiteralNode(false)) => pred
          case (LiteralNode(false), LiteralNode(true)) => Library.Not.typed[Boolean](pred)
          case _ =>
            val ifDefined = map.replace({
              case Ref(s) :@ tpe if s == gen => silentCast(tpe, from)
            }, keepType = true).nodeWithComputedType()
            val ifEmpty2 = silentCast(ifDefined.nodeType.structural, ifEmpty)
            IfThenElse(Vector(pred, ifEmpty2, ifDefined))
        }
        n2.nodeWithComputedType()

      // Other OptionFold -> translate to discriminator check
      case OptionFold(from, ifEmpty, map, gen) =>
        val left = from.select(ElementSymbol(1)).nodeWithComputedType()
        val pred = Library.==.typed[Boolean](left, Disc1)
        val n2 = (ifEmpty, map) match {
          case (LiteralNode(true), LiteralNode(false)) => Library.Not.typed[Boolean](pred)
          case (LiteralNode(false), LiteralNode(true)) => pred
          case _ =>
            val ifDefined = map.replace({
              case Ref(s) :@ tpe if s == gen => silentCast(tpe, from.select(ElementSymbol(2)).nodeWithComputedType())
            }, keepType = true).nodeWithComputedType()
            val ifEmpty2 = silentCast(ifDefined.nodeType.structural, ifEmpty)
            if(left == Disc1) ifDefined else IfThenElse(IndexedSeq(pred, ifDefined, ifEmpty2))
        }
        n2.nodeWithComputedType()

      // Primitive OptionApply -> leave unchanged
      case n @ OptionApply(_) :@ OptionType.Primitive(_) => n

      // Other OptionApply -> translate to product form
      case n @ OptionApply(ch) => ProductNode(Vector(Disc1, silentCast(toOptionColumns(ch.nodeType), ch))).nodeWithComputedType()

      // Non-primitive GetOrElse
      // (.get is only defined on primitive Options, but this can occur inside of HOFs like .map)
      case g @ GetOrElse(ch :@ tpe, _) =>
        tpe match {
          case OptionType.Primitive(_) => g
          case _ => throw new SlickException(".get may only be called on Options of top-level primitive types")
        }

      // Option-extended left outer, right outer or full outer join
      case bind @ Bind(bsym, Join(_, _, _, _, jt, _), _) if jt == JoinType.LeftOption || jt == JoinType.RightOption || jt == JoinType.OuterOption =>
        translateJoin(bind)

      case n => n
    }
    val tree4 = fuse(tree3)
    tree4.nodeTypedOrCopy(trType(tree4.nodeType))
  }

  /** Translate an Option-extended left outer, right outer or full outer join */
  def translateJoin(bind: Bind): Bind = {
    logger.debug("translateJoin", bind)
    val Bind(bsym, (join @ Join(lsym, rsym, left :@ CollectionType(_, leftElemType), right :@ CollectionType(_, rightElemType), jt, on)) :@ CollectionType(cons, elemType), pure) = bind
    val lComplex = leftElemType.structural.children.nonEmpty
    val rComplex = rightElemType.structural.children.nonEmpty
    logger.debug(s"Translating join ($jt, complex: $lComplex, $rComplex):", bind)

    // Option-extend one side of the join with a discriminator column
    def extend(side: Node, sym: Symbol, on: Node): (Node, Node) = {
      val extendGen = new AnonSymbol
      val extend :@ CollectionType(_, extendedElementType) = Bind(extendGen, side, Pure(ProductNode(Vector(Disc1, Ref(extendGen))))).nodeWithComputedType()
      val sideInCondition = Select(Ref(sym).nodeTyped(extendedElementType), ElementSymbol(2)).nodeWithComputedType()
      val on2 = on.replace({
        case Ref(s) if s == sym => sideInCondition
        case n @ Select(in, _) => n.nodeWithComputedType(retype = true)  //if !in.nodeHasType => n.nodeUntypedOrCopy
      }, bottomUp = true)
      (extend, on2)
    }

    // Translate the join depending on JoinType and Option type
    val (left2, right2, on2, jt2) = jt match {
      case JoinType.LeftOption =>
        val (right2, on2) = if(rComplex) extend(right, rsym, on) else (right, on)
        (left, right2, on2, JoinType.Left)
      case JoinType.RightOption =>
        val (left2, on2) = if(lComplex) extend(left, lsym, on) else (left, on)
        (left2, right, on2, JoinType.Right)
      case JoinType.OuterOption =>
        val (left2, on2) = if(lComplex) extend(left, lsym, on) else (left, on)
        val (right2, on3) = if(rComplex) extend(right, rsym, on2) else (right, on2)
        (left2, right2, on3, JoinType.Outer)
    }

    // Cast to translated Option type in outer bind
    val join2 :@ CollectionType(_, elemType2) = Join(lsym, rsym, left2, right2, jt2, on2).nodeWithComputedType()
    val ref = silentCast(trType(elemType), Ref(bsym).nodeTyped(elemType2))
    val pure2 = pure.replace({
      case Ref(s) if s == bsym => ref
      case n @ Select(in, _) if !in.nodeHasType => n.nodeUntypedOrCopy

      // Hoist SilentCasts and remove unnecessary ones
      case Library.SilentCast(Library.SilentCast(ch)) :@ tpe => silentCast(tpe, ch.nodeWithComputedType())
      case Select(Library.SilentCast(ch), s) :@ tpe => silentCast(tpe, ch.select(s).nodeWithComputedType())

      // Ensure that the child is typed
      case Library.SilentCast(ch) :@ tpe => silentCast(tpe, ch.nodeWithComputedType())
    }, bottomUp = true)
    val res = Bind(bsym, join2, pure2).nodeWithComputedType()
    logger.debug("Translated join:", res)
    res
  }

  /** Create a SilentCast call unless the type already matches */
  def silentCast(tpe: Type, n: Node): Node = n match {
    case LiteralNode(None) :@ OptionType(ScalaBaseType.nullType) => buildMultiColumnNone(tpe)
    case n :@ tpe2 if tpe2 == tpe => n
    case n => Library.SilentCast.typed(tpe, n)
  }

  /** Create a Node representing a structure of null values of the given Type */
  def buildMultiColumnNone(tpe: Type): Node = (tpe.structural match {
    case ProductType(ch) => ProductNode(ch.map(buildMultiColumnNone))
    case StructType(ch) => StructNode(ch.map { case (sym, t) => (sym, buildMultiColumnNone(t)) })
    case OptionType(ch) => LiteralNode(tpe, None)
    case t => throw new SlickException("Unexpected non-Option type in multi-column None")
  }).nodeTypedOrCopy(tpe)

  /** Perform the sum expansion on a Type */
  def trType(tpe: Type): Type = {
    def f(tpe: Type): Type = tpe.mapChildren(f) match {
      case t @ OptionType.Primitive(_) => t
      case OptionType(ch) => ProductType(Vector(ScalaBaseType.optionDiscType.optionType, toOptionColumns(ch)))
      case t => t
    }
    val tpe2 = f(tpe)
    logger.debug(s"Translated type: $tpe -> $tpe2")
    tpe2
  }

  /** Strip nominal types and convert all atomic types to OptionTypes */
  def toOptionColumns(tpe: Type): Type = tpe match {
    case NominalType(_, str) => toOptionColumns(str)
    case o @ OptionType(ch) if ch.structural.children.isEmpty => o
    case t if t.children.isEmpty => OptionType(t)
    case t => t.mapChildren(toOptionColumns)
  }

  /** Fuse unnecessary Option operations */
  def fuse(n: Node): Node = n match {
    // Option.map
    case IfThenElse(IndexedSeq(Library.==(disc, Disc1), ProductNode(Seq(Disc1, map)), ProductNode(Seq(DiscNone, _)))) =>
      ProductNode(Vector(disc, map)).nodeWithComputedType()
    case n => n
  }
}
