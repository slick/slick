package slick.lifted


import scala.annotation.implicitNotFound
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.compat.*
import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.reflect.macros.blackbox

import slick.SlickException
import slick.ast.*
import slick.util.{ConstArray, ProductWrapper, TupleSupport}

/** A type class that encodes the unpacking `Mixed => Unpacked` of a
 * `Query[Mixed]` to its result element type `Unpacked` and the packing to a
 * fully packed type `Packed`, i.e. a type where everything which is not a
 * transparent container is wrapped in a `Column[_]`.
 *
 * =Example:=
 *  - Mixed: `(Column[Int], Column[(Int, String)], (Int, Option[Double]))`
 *  - Unpacked: `(Int, (Int, String), (Int, Option[Double]))`
 *  - Packed: `(Column[Int], Column[(Int, String)], (Column[Int], Column[Option[Double]]))`
 *  - Linearized: `(Int, Int, String, Int, Option[Double])`
 */
@implicitNotFound(
  msg =
    """No matching Shape found.
Slick does not know how to map the given types.
Possible causes: T in Table[T] does not match your * projection,
 you use an unsupported type in a Query (e.g. scala List),
 or you forgot to import a driver api into scope.
  Required level: ${Level}
     Source type: ${Mixed_}
   Unpacked type: ${Unpacked_}
     Packed type: ${Packed_}
""")
abstract class Shape[Level <: ShapeLevel, -Mixed_, Unpacked_, Packed_] {
  type Mixed = Mixed_ @uncheckedVariance
  type Unpacked = Unpacked_
  type Packed = Packed_

  /** Convert a value of this Shape's (mixed) type to the fully packed type */
  def pack(value: Mixed): Packed

  /** Return the fully packed Shape */
  def packedShape: Shape[Level, Packed, Unpacked, Packed]

  /** Build a packed representation containing QueryParameters that can extract
    * data from the unpacked representation later.
    * This method is not available for shapes where Mixed and Unpacked are
    * different types. */
  def buildParams(extract: Any => Unpacked): Packed

  /** Encode a reference into a value of this Shape.
    * This method may not be available for shapes where Mixed and Packed are
    * different types. */
  def encodeRef(value: Mixed, path: Node): Any

  /** Return an AST Node representing a mixed value. */
  def toNode(value: Mixed): Node
}

object Shape extends ConstColumnShapeImplicits with AbstractTableShapeImplicits with TupleShapeImplicits {
  implicit final def primitiveShape[T, Level <: ShapeLevel](implicit tm: TypedType[T]
                                                           ): Shape[Level, T, T, ConstColumn[T]] =
    new Shape[Level, T, T, ConstColumn[T]] {
      def pack(value: Mixed) = LiteralColumn(value)
      def packedShape = RepShape[Level, Packed, Unpacked]
      def buildParams(extract: Any => Unpacked): Packed = new ConstColumn[T](new QueryParameter(extract, tm))(tm)
      def encodeRef(value: Mixed, path: Node) =
        throw new SlickException("Shape does not have the same Mixed and Packed type")
      def toNode(value: Mixed): Node = pack(value).toNode
    }

  @inline implicit final def unitShape[Level <: ShapeLevel]: Shape[Level, Unit, Unit, Unit] =
    unitShapePrototype.asInstanceOf[Shape[Level, Unit, Unit, Unit]]

  // Need to be of higher priority than repColumnShape,
  // otherwise single-column ShapedValues and MappedProjections are ambiguous
  @inline implicit def shapedValueShape[T, U, Level <: ShapeLevel]: Shape[
    Level,
    ShapedValue[T, U],
    U,
    ShapedValue[T, U]
  ] =
    RepShape[Level, ShapedValue[T, U], U]
  @inline
  implicit def mappedProjectionShape[Level >: FlatShapeLevel <: ShapeLevel, T, P]: Shape[
    Level,
    MappedProjection[T, P],
    T,
    MappedProjection[T, P]
  ] =
    RepShape[Level, MappedProjection[T, P], T]

  val unitShapePrototype: Shape[FlatShapeLevel, Unit, Unit, Unit] = new Shape[FlatShapeLevel, Unit, Unit, Unit] {
    def pack(value: Mixed) = ()
    def packedShape: Shape[FlatShapeLevel, Packed, Unpacked, Packed] = this
    def buildParams(extract: Any => Unpacked) = ()
    def encodeRef(value: Mixed, path: Node) = ()
    def toNode(value: Mixed) = ProductNode(ConstArray.empty)
  }
}

trait AbstractTableShapeImplicits extends RepShapeImplicits {
  @inline implicit final def tableShape[
    Level >: FlatShapeLevel <: ShapeLevel,
    T,
    C <: AbstractTable[?]
  ](implicit ev: C <:< AbstractTable[T]): Shape[Level, C, T, C] =
    RepShape[Level, C, T]
}

trait ConstColumnShapeImplicits extends RepShapeImplicits {
  /** A Shape for ConstColumns. It is identical to `columnShape` but it
    * ensures that a `ConstColumn[T]` packs to itself, not just to
    * `Rep[T]`. This allows ConstColumns to be used as fully packed
    * types when compiling query functions. */
  @inline implicit def constColumnShape[T, Level <: ShapeLevel]: Shape[Level, ConstColumn[T], T, ConstColumn[T]] =
    RepShape[Level, ConstColumn[T], T]
}

trait RepShapeImplicits extends OptionShapeImplicits {
  /** A Shape for single-column Reps. */
  @inline implicit def repColumnShape[T : BaseTypedType, Level <: ShapeLevel]: Shape[Level, Rep[T], T, Rep[T]] =
    RepShape[Level, Rep[T], T]

  /** A Shape for Option-valued Reps. */
  @inline
  implicit def optionShape[M, U, P, Level <: ShapeLevel](implicit sh: Shape[? <: Level, Rep[M], U, Rep[P]]
                                                        ): Shape[Level, Rep[Option[M]], Option[U], Rep[Option[P]]] =
    RepShape.asInstanceOf[Shape[Level, Rep[Option[M]], Option[U], Rep[Option[P]]]]
}

trait OptionShapeImplicits {
  /** A Shape for Option-valued non-Reps. */
  @inline
  implicit def anyOptionShape[M, U, P, Level <: ShapeLevel](implicit sh: Shape[? <: Level, M, U, P]
                                                           ): Shape[Level, Rep[Option[M]], Option[U], Rep[Option[P]]] =
    RepShape.asInstanceOf[Shape[Level, Rep[Option[M]], Option[U], Rep[Option[P]]]]
}

/** Shape for Rep values (always fully packed) */
object RepShape extends Shape[FlatShapeLevel, Rep[?], Any, Rep[?]] {
  def apply[Level <: ShapeLevel, MP <: Rep[?], U]: Shape[Level, MP, U, MP] = this.asInstanceOf[Shape[Level, MP, U, MP]]

  def pack(value: Mixed): Packed = value
  def packedShape: Shape[FlatShapeLevel, Packed, Unpacked, Packed] = this
  def buildParams(extract: Any => Unpacked): Packed =
    throw new SlickException("Shape does not have the same Mixed and Unpacked type")
  override def encodeRef(value: Mixed, path: Node): Rep[?] = value.encodeRef(path)
  def toNode(value: Mixed): Node = value.toNode
}

/** Base class for Shapes of record values which are represented by
  * ProductNodes in the AST.
  *
  * @tparam C The supertype for the record values.
  * @tparam M The mixed type of the Shape (a subtype of C).
  * @tparam U The unpacked type of the Shape (a subtype of C).
  * @tparam P The fully packed type of the Shape (a subtype of C).
  */
abstract class ProductNodeShape[Level <: ShapeLevel, C, M <: C, U <: C, P <: C] extends Shape[Level, M, U, P] {
  /** The Shapes for the product elements. */
  val shapes: Seq[Shape[? <: ShapeLevel, ?, ?, ?]]

  /** Build a record value represented by this Shape from its element values. */
  def buildValue(elems: IndexedSeq[Any]): Any

  /** Create a copy of this Shape with new element Shapes. This is used for
    * packing Shapes recursively. */
  def copy(shapes: Seq[Shape[? <: ShapeLevel, ?, ?, ?]]): Shape[Level, ?, ?, ?]

  /** Get the element value from a record value at the specified index. */
  def getElement(value: C, idx: Int): Any

  /** Get an Iterator of a record value's element values. The default
    * implementation repeatedly calls `getElement`. */
  def getIterator(value: C): Iterator[Any] =
    shapes.iterator.zipWithIndex.map(t => getElement(value, t._2))

  private def shapesWithValues(value: Mixed) = shapes.iterator.zip(getIterator(value))
  def pack(value: Mixed) = {
    val elems = shapesWithValues(value).map{ case (p, f) => p.pack(f.asInstanceOf[p.Mixed]) }
    buildValue(elems.toIndexedSeq).asInstanceOf[Packed]
  }
  def packedShape: Shape[Level, Packed, Unpacked, Packed] =
    copy(shapes.map(_.packedShape)).asInstanceOf[Shape[Level, Packed, Unpacked, Packed]]
  def buildParams(extract: Any => Unpacked): Packed = {
    val elems = shapes.iterator.zipWithIndex.map { case (p, idx) =>
      def chExtract(u: C): p.Unpacked = getElement(u, idx).asInstanceOf[p.Unpacked]
      p.buildParams(extract.andThen(chExtract))
    }
    buildValue(elems.toIndexedSeq).asInstanceOf[Packed]
  }
  def encodeRef(value: Mixed, path: Node) = {
    val elems = shapesWithValues(value).zipWithIndex.map {
      case ((p, x), pos) => p.encodeRef(x.asInstanceOf[p.Mixed], Select(path, ElementSymbol(pos + 1)))
    }
    buildValue(elems.toIndexedSeq)
  }
  def toNode(value: Mixed): Node = ProductNode(ConstArray.from(shapesWithValues(value).map {
    case (p, f) => p.toNode(f.asInstanceOf[p.Mixed])
  }.to(Vector)))
}

/** Base class for ProductNodeShapes with a type mapping */
abstract class MappedProductShape[Level <: ShapeLevel, C, M <: C, U <: C, P <: C]
  extends ProductNodeShape[Level, C, M, U, P] {

  override def toNode(value: Mixed): TypeMapping =
    TypeMapping(super.toNode(value), MappedScalaType.Mapper(toBase, toMapped, None), classTag)
  def toBase(v: Any) = new ProductWrapper(getIterator(v.asInstanceOf[C]).toIndexedSeq)
  def toMapped(v: Any) = buildValue(TupleSupport.buildIndexedSeq(v.asInstanceOf[Product]))
  def classTag: ClassTag[U]
}

/** Base class for ProductNodeShapes with a type mapping to a type that extends scala.Product */
abstract class MappedScalaProductShape[
  Level <: ShapeLevel,
  C <: Product,
  M <: C,
  U <: C,
  P <: C
](implicit val classTag: ClassTag[U]) extends MappedProductShape[Level, C, M, U, P] {
  override def getIterator(value: C) = value.productIterator
  def getElement(value: C, idx: Int) = value.productElement(idx)
}

/** Shape for Scala tuples of all arities */
final class TupleShape[
  Level <: ShapeLevel,
  M <: Product,
  U <: Product,
  P <: Product
](val shapes: Shape[? <: ShapeLevel, ?, ?, ?]*)
  extends ProductNodeShape[Level, Product, M, U, P] {

  override def getIterator(value: Product) = value.productIterator
  def getElement(value: Product, idx: Int) = value.productElement(idx)
  override def buildValue(elems: IndexedSeq[Any]): Product = TupleSupport.buildTuple(elems)
  override def copy(shapes: Seq[Shape[? <: ShapeLevel, ?, ?, ?]]): TupleShape[Level, Nothing, Nothing, Nothing] =
    new TupleShape(shapes *)
}

/** A generic case class shape that can be used to lift a case class of
 * plain Scala types to a case class of lifted types. This allows the type
 * to be used as a record type (like tuples and HLists) in the Lifted
 * Embedding.
 *
 * Example:
 *
 * {{{
 *   case class C(a: Int, b: Option[String])
 *   case class LiftedC(a: Column[Int], b: Column[Option[String]])
 *   implicit object cShape extends CaseClassShape(LiftedC.tupled, C.tupled)
 * }}}
 */
class CaseClassShape[
  P <: Product,
  LiftedTuple,
  LiftedCaseClass <: P,
  PlainTuple,
  PlainCaseClass <: P
](mapLifted: LiftedTuple => LiftedCaseClass, mapPlain: PlainTuple => PlainCaseClass)
 (implicit columnShapes: Shape[FlatShapeLevel, LiftedTuple, PlainTuple, LiftedTuple],
  classTag: ClassTag[PlainCaseClass])
  extends MappedScalaProductShape[FlatShapeLevel, P, LiftedCaseClass, PlainCaseClass, LiftedCaseClass] {
  val shapes = columnShapes.asInstanceOf[TupleShape[?, ?, ?, ?]].shapes
  override def toMapped(v: Any): PlainCaseClass = mapPlain(v.asInstanceOf[PlainTuple])
  override def buildValue(elems: IndexedSeq[Any]): LiftedCaseClass =
    mapLifted(TupleSupport.buildTuple(elems).asInstanceOf[LiftedTuple])
  override def copy(s: Seq[Shape[? <: ShapeLevel, ?, ?, ?]]
                   ): CaseClassShape[P, LiftedTuple, LiftedCaseClass, PlainTuple, PlainCaseClass] =
    new CaseClassShape(mapLifted, mapPlain) {
      override val shapes = s
    }
}

/** A generic Product class shape that can be used to lift a class of
  * plain Scala types to a class of lifted types. This allows the type
  * to be used as a record type (like tuples and HLists) in the Lifted
  * Embedding.
  *
  * This can help with mapping tables >22 columns to classes, especially
  * when using code generation. This can be used for Scala 2.11 case classes >22 fields.
  *
  * Example:
  *
  * {{{
  *   def columnShape[T](implicit s: Shape[FlatShapeLevel, Column[T], T, Column[T]]) = s
  *   class C(val a: Int, val b: Option[String]) extends Product{
  *     def canEqual(that: Any): Boolean = that.isInstanceOf[C]
  *     def productArity: Int = 2
  *     def productElement(n: Int): Any = Seq(a, b)(n)
  *   }
  *   class LiftedC(val a: Column[Int], val b: Column[Option[String]]) extends Product{
  *     def canEqual(that: Any): Boolean = that.isInstanceOf[LiftedC]
  *     def productArity: Int = 2
  *     def productElement(n: Int): Any = Seq(a, b)(n)
  *   }
  *   implicit object cShape extends ProductClassShape(
  *     Seq(columnShape[Int], columnShape[Option[String]]),
  *     seq => new LiftedC(seq(0).asInstanceOf[Column[Int]], seq(1).asInstanceOf[Column[Option[String]]]),
  *     seq => new C(seq(0).asInstanceOf[Int], seq(1).asInstanceOf[Option[String]])
  *   )
  * }}}
  */
class ProductClassShape[E <: Product,C <: Product](
                                                    val shapes: Seq[Shape[? <: ShapeLevel, ?, ?, ?]],
                                                    mapLifted: Seq[Any] => C,
                                                    mapPlain:Seq[Any] => E
)(implicit classTag: ClassTag[E]) extends MappedScalaProductShape[
  FlatShapeLevel, Product, C, E, C
]{
  override def toMapped(v: Any): E = mapPlain(v.asInstanceOf[Product].productIterator.toSeq)
  override def buildValue(elems: IndexedSeq[Any]): C = mapLifted(elems)
  override def copy(s: Seq[Shape[? <: ShapeLevel, ?, ?, ?]]): ProductClassShape[E, C] =
    new ProductClassShape(s, mapLifted, mapPlain)
}

/** The level of a Shape, i.e. what kind of types it allows.
  * Subtypes of this trait are used as a phantom type for Shape resolution.
  * There are no instances of any ShapeLevel. */
trait ShapeLevel

/** ShapeLevel that allows nested collections. */
trait NestedShapeLevel extends ShapeLevel

/** ShapeLevel that does not allow nested collections.
  * This is the standard level for executable queries. */
trait FlatShapeLevel extends NestedShapeLevel

/** ShapeLevel that only allows records of individual columns.
  * This level is used for parameters of compiled queries. */
trait ColumnsShapeLevel extends FlatShapeLevel

/** A value together with its Shape */
case class ShapedValue[T, U](value: T, shape: Shape[? <: FlatShapeLevel, T, U, ?]) extends Rep[U] {
  def encodeRef(path: Node): ShapedValue[T, U] = {
    val fv = shape.encodeRef(value, path).asInstanceOf[T]
    if(fv.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) this else new ShapedValue(fv, shape)
  }
  def toNode = shape.toNode(value)
  def packedValue[R](implicit ev: Shape[? <: FlatShapeLevel, T, ?, R]): ShapedValue[R, U] =
    ShapedValue(shape.pack(value).asInstanceOf[R], shape.packedShape.asInstanceOf[Shape[FlatShapeLevel, R, U, ?]])
  def zip[T2, U2](s2: ShapedValue[T2, U2]) =
    new ShapedValue[(T, T2), (U, U2)]((value, s2.value), Shape.tuple2Shape(shape, s2.shape))
  def <>[R : ClassTag](f: U => R, g: R => Option[U]) =
    new MappedProjection[R, U](
      shape.toNode(value),
      MappedScalaType.Mapper(g.andThen(_.get).asInstanceOf[Any => Any], f.asInstanceOf[Any => Any], None),
      implicitly[ClassTag[R]]
    )
  @inline def shaped: ShapedValue[T, U] = this

  def mapTo[R <: Product & Serializable](implicit rCT: ClassTag[R]
                                        ): MappedProjection[R, U] = macro ShapedValue.mapToImpl[R, U]
}

object ShapedValue {
  def mapToImpl[R <: Product & Serializable, U](c: blackbox.Context { type PrefixType = ShapedValue[?, U] })
                                               (rCT: c.Expr[ClassTag[R]])
                                               (implicit rTag: c.WeakTypeTag[R], uTag: c.WeakTypeTag[U]): c.Tree = {
    import c.universe.*
    val rSym = symbolOf[R]
    if(!rSym.isClass || !rSym.asClass.isCaseClass)
      c.abort(c.enclosingPosition, s"${rSym.fullName} must be a case class")
    val rModule = rSym.companion match {
      case NoSymbol => q"${rSym.name.toTermName}" // This can happen for case classes defined inside of methods
      case s => q"$s"
    }
    val rHasTupled = rSym.companion match {
      case NoSymbol => true
      case s => s.info.member(TermName("tupled")) != NoSymbol
    }
    val fields =
      rTag.tpe.decls
        .collect {
          case s: TermSymbol if s.isVal && s.isCaseAccessor =>
            (TermName(s.name.toString.trim), s.typeSignature, TermName(c.freshName()))
        }
        .toIndexedSeq
    val (f, g) = if(uTag.tpe <:< c.typeOf[slick.collection.heterogeneous.HList]) { // Map from HList
      val rTypeAsHList = fields.foldRight[Tree](tq"_root_.slick.collection.heterogeneous.HNil.type") {
        case ((_, t, _), z) => tq"_root_.slick.collection.heterogeneous.HCons[$t, $z]"
      }
      val pat = fields.foldRight[Tree](pq"_root_.slick.collection.heterogeneous.HNil") {
        case ((_, _, n), z) => pq"_root_.slick.collection.heterogeneous.HCons($n, $z)"
      }
      val cons = fields.foldRight[Tree](q"_root_.slick.collection.heterogeneous.HNil") {
        case ((n, _, _), z) => q"v.$n :: $z"
      }
      (q"({ case $pat => new $rTag(..${fields.map(_._3)}) } : ($rTypeAsHList => $rTag)): ($uTag => $rTag)",
       q"{ case v => $cons }: ($rTag => $uTag)")
    } else if(fields.length == 1) { // Map from single value
      (q"($rModule.apply _) : ($uTag => $rTag)",
       q"(($rModule.unapply _) : $rTag => Option[$uTag]).andThen(_.get)")
    } else if (rHasTupled) { // Map from tuple with existing tupled
      (q"($rModule.tupled) : ($uTag => $rTag)",
        q"(($rModule.unapply _) : $rTag => Option[$uTag]).andThen(_.get)")
    } else { // Map from tuple with tupled apply
      (q"(($rModule.apply _).tupled) : ($uTag => $rTag)",
        q"(($rModule.unapply _) : $rTag => Option[$uTag]).andThen(_.get)")
    }

    val fpName = Constant("Fast Path of ("+fields.map(_._2).mkString(", ")+").mapTo["+rTag.tpe+"]")
    val fpChildren = fields.map { case (_, t, n) => q"val $n = next[$t]" }
    val fpReadChildren = fields.map { case (_, _, n) => q"$n.read(r)" }
    val fpSetChildren = fields.map { case (fn, _, n) => q"$n.set(value.$fn, pp, offset)" }
    val fpUpdateChildren = fields.map { case (fn, _, n) => q"$n.update(value.$fn, pr)" }

    q"""
      // Resolving f first creates more useful type errors
      val ff = $f.asInstanceOf[_root_.scala.Any => _root_.scala.Any]
      val gg = $g.asInstanceOf[_root_.scala.Any => _root_.scala.Any]
      val fpMatch: (_root_.scala.Any => _root_.scala.Any) = {
        case tm @ _root_.slick.relational.TypeMappingResultConverter(
          _: _root_.slick.relational.ProductResultConverter[_, _],
          _,
          _
        ) =>
          new _root_.slick.relational.SimpleFastPathResultConverter[
            _root_.slick.relational.ResultConverterDomain,
             $rTag
          ](
            tm.asInstanceOf[
              _root_.slick.relational.TypeMappingResultConverter[
                _root_.slick.relational.ResultConverterDomain,
                $rTag,
                _
              ]
            ]
          ) {
            ..$fpChildren
            override def read(r: Reader): $rTag = new $rTag(..$fpReadChildren)
            override def set(value: $rTag, pp: Writer, offset: Int): _root_.scala.Unit = {..$fpSetChildren}
            override def update(value: $rTag, pr: Updater): _root_.scala.Unit = {..$fpUpdateChildren}
            override def getDumpInfo = super.getDumpInfo.copy(name = $fpName)
          }
        case tm => tm
      }
      new _root_.slick.lifted.MappedProjection[$rTag, $uTag](${c.prefix}.toNode,
        _root_.slick.ast.MappedScalaType.Mapper(gg, ff, _root_.scala.Some(fpMatch)), $rCT)
    """
  }
}

/** A limited version of ShapedValue which can be constructed for every type
  * that has a valid shape. We use it to enforce that a table's * projection
  * has a valid shape. A ProvenShape has itself a Shape so it can be used in
  * place of the value that it wraps for purposes of packing and unpacking. */
trait ProvenShape[U] {
  def value: Any
  val shape: Shape[? <: FlatShapeLevel, ?, U, ?]
  def packedValue[R](implicit ev: Shape[? <: FlatShapeLevel, ?, U, R]): ShapedValue[R, U]
  def toNode = packedValue(shape).toNode
}

object ProvenShape {
  /** Convert an appropriately shaped value to a ProvenShape */
  implicit def proveShapeOf[T, U](v: T)(implicit sh: Shape[? <: FlatShapeLevel, T, U, ?]): ProvenShape[U] =
    new ProvenShape[U] {
      def value: T = v
      val shape: Shape[? <: FlatShapeLevel, ?, U, ?] = sh
      def packedValue[R](implicit ev: Shape[? <: FlatShapeLevel, ?, U, R]): ShapedValue[R, U] =
        ShapedValue(sh.pack(value).asInstanceOf[R], sh.packedShape.asInstanceOf[Shape[FlatShapeLevel, R, U, ?]])
    }

  /** The Shape for a ProvenShape */
  implicit def provenShapeShape[T, P](implicit shape: Shape[? <: FlatShapeLevel, T, T, P]
                                     ): Shape[FlatShapeLevel, ProvenShape[T], T, P] =
    new Shape[FlatShapeLevel, ProvenShape[T], T, P] {
      def pack(value: Mixed): Packed =
        value.shape.pack(value.value.asInstanceOf[value.shape.Mixed]).asInstanceOf[Packed]
      def packedShape: Shape[FlatShapeLevel, Packed, Unpacked, Packed] =
        shape.packedShape.asInstanceOf[Shape[FlatShapeLevel, Packed, Unpacked, Packed]]
      def buildParams(extract: Any => Unpacked): Packed =
        shape.buildParams(extract)
      def encodeRef(value: Mixed, path: Node) =
        value.shape.encodeRef(value.value.asInstanceOf[value.shape.Mixed], path)
      def toNode(value: Mixed): Node =
        value.shape.toNode(value.value.asInstanceOf[value.shape.Mixed])
    }
}

class MappedProjection[T, P](child: Node, mapper: MappedScalaType.Mapper, classTag: ClassTag[T]) extends Rep[T] {
  type Self = MappedProjection[?, ?]
  override def toString = "MappedProjection"
  override def toNode: Node = TypeMapping(child, mapper, classTag)
  def encodeRef(path: Node): MappedProjection[T, P] = new MappedProjection[T, P](child, mapper, classTag) {
    override def toNode = path
  }
  def genericFastPath(f: Function[Any, Any]) =
    new MappedProjection[T, P](child, mapper.copy(fastPath = Some(f)), classTag)
}
