package slick.compiler

import slick.{SlickTreeException, SlickException}
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
    val tree2 = tree.mapChildren(tr, keepType = true)
    val tree3 = tree2 match {
      // Expand multi-column null values in ELSE branches (used by Rep[Option].filter) with correct type
      case IfThenElse(IndexedSeq(pred, then1 :@ tpe, LiteralNode(None) :@ OptionType(ScalaBaseType.nullType))) =>
        IfThenElse(Vector(pred, then1, buildMultiColumnNone(tpe))) :@ tpe

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
            IfThenElse(Vector(pred, ifEmpty2, ifDefined))
        }
        n2.infer()

      // Other OptionFold -> translate to discriminator check
      case OptionFold(from, ifEmpty, map, gen) =>
        val left = from.select(ElementSymbol(1)).infer()
        val pred = Library.==.typed[Boolean](left, Disc1)
        val n2 = (ifEmpty, map) match {
          case (LiteralNode(true), LiteralNode(false)) => Library.Not.typed[Boolean](pred)
          case (LiteralNode(false), LiteralNode(true)) => pred
          case _ =>
            val ifDefined = map.replace({
              case r @ Ref(s) if s == gen => silentCast(r.nodeType, from.select(ElementSymbol(2)).infer())
            }, keepType = true)
            val ifEmpty2 = silentCast(ifDefined.nodeType.structural, ifEmpty)
            if(left == Disc1) ifDefined else IfThenElse(IndexedSeq(pred, ifDefined, ifEmpty2))
        }
        n2.infer()

      // Primitive OptionApply -> leave unchanged
      case n @ OptionApply(_) :@ OptionType.Primitive(_) => n

      // Other OptionApply -> translate to product form
      case n @ OptionApply(ch) => ProductNode(Vector(Disc1, silentCast(toOptionColumns(ch.nodeType), ch))).infer()

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
    tree4 :@ trType(tree4.nodeType)
  }

  /** Translate an Option-extended left outer, right outer or full outer join */
  def translateJoin(bind: Bind): Bind = {
    logger.debug("translateJoin", bind)
    val Bind(bsym, (join @ Join(lsym, rsym, left :@ CollectionType(_, leftElemType), right :@ CollectionType(_, rightElemType), jt, on)) :@ CollectionType(cons, elemType), pure) = bind
    val lComplex = leftElemType.structural.children.nonEmpty
    val rComplex = rightElemType.structural.children.nonEmpty
    logger.debug(s"Translating join ($jt, complex: $lComplex, $rComplex):", bind)

    // Find an existing column that can serve as a discriminator
    def findDisc(t: Type): Option[List[TermSymbol]] = {
      def find(t: Type, path: List[TermSymbol]): Vector[List[TermSymbol]] = t.structural match {
        case StructType(defs) => defs.flatMap { case (s, t) => find(t, s :: path) }(collection.breakOut)
        case p: ProductType => p.numberedElements.flatMap { case (s, t) => find(t, s :: path) }.toVector
        case _: AtomicType => Vector(path)
        case _ => Vector.empty
      }
      find(t, Nil).sortBy(ss => ss.head match {
        case f: FieldSymbol =>
          if(f.options contains ColumnOption.PrimaryKey) -2 else -1
        case _ => 0
      }).headOption
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
      val extend :@ CollectionType(_, extendedElementType) = Bind(extendGen, side, Pure(ProductNode(Vector(disc, Ref(extendGen))))).infer()
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
        val disc = IfThenElse(Vector(Library.==.typed[Boolean](silentCast(OptionType(protoDisc.nodeType), protoDisc), LiteralNode(null)), DiscNone, Disc1))
        ProductNode(Vector(disc, rest))
      } else ref
      silentCast(trType(elemType.asInstanceOf[ProductType].children(idx)), v)
    }
    val ref = ProductNode(Vector(optionCast(0, ldisc), optionCast(1, rdisc))).infer()
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
      ProductNode(Vector(disc, map)).infer()
    case n => n
  }
}
