package slick.lifted


import scala.annotation.implicitNotFound
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.compat.*
import scala.language.implicitConversions
import scala.reflect.ClassTag

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
  def pack(value: Any /* should be Mixed */): Packed

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
  def encodeRef(value: Any, path: Node): Any

  /** Return an AST Node representing a mixed value. */
  def toNode(value: Any): Node
}

object Shape extends ConstColumnShapeImplicits with AbstractTableShapeImplicits with TupleShapeImplicits {
  implicit final def primitiveShape[T, Level <: ShapeLevel](implicit tm: TypedType[T]
                                                           ): Shape[Level, T, T, ConstColumn[T]] =
    new Shape[Level, T, T, ConstColumn[T]] {
      def pack(value: Any) = LiteralColumn(value.asInstanceOf[T])
      def packedShape = RepShape[Level, Packed, Unpacked]
      def buildParams(extract: Any => Unpacked): Packed = new ConstColumn[T](new QueryParameter(extract, tm))(tm)
      def encodeRef(value: Any, path: Node) =
        throw new SlickException("Shape does not have the same Mixed and Packed type")
      def toNode(value: Any): Node = pack(value).toNode
    override def toString = s"PrimitiveShape($tm)"
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
    def pack(value: Any) = ()
    def packedShape: Shape[FlatShapeLevel, Packed, Unpacked, Packed] = this
    def buildParams(extract: Any => Unpacked) = ()
    def encodeRef(value: Any, path: Node) = ()
    def toNode(value: Any) = ProductNode(ConstArray.empty)
    override def toString = s"UnitShape"
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
  def pack(value: Any): Packed = value.asInstanceOf[Packed]
  def packedShape: Shape[FlatShapeLevel, Packed, Unpacked, Packed] = this
  def buildParams(extract: Any => Unpacked): Packed =
    throw new SlickException("Shape does not have the same Mixed and Unpacked type")
  override def encodeRef(value: Any, path: Node): Rep[?] = value.asInstanceOf[Rep[_]].encodeRef(path)
  def toNode(value: Any): Node = value.asInstanceOf[Rep[_]].toNode
  override def toString = "RepShape"
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
  def pack(value: Any) = {
    val elems = shapesWithValues(value.asInstanceOf[M]).map{ case (p, f) => p.pack(f) }
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
  def encodeRef(value: Any, path: Node) = {
    val elems = shapesWithValues(value.asInstanceOf[M]).zipWithIndex.map {
      case ((p, x), pos) => p.encodeRef(x, Select(path, ElementSymbol(pos + 1)))
    }
    buildValue(elems.toIndexedSeq)
  }
  def toNode(value: Any): Node = ProductNode(ConstArray.from(shapesWithValues(value.asInstanceOf[M]).map {
    case (p, f) => p.toNode(f)
  }.to(Vector)))

  override def toString = shapes.mkString("ProductNodeShape(", ", ", ")")
}

/** Base class for ProductNodeShapes with a type mapping */
abstract class MappedProductShape[Level <: ShapeLevel, C, M <: C, U <: C, P <: C]
  extends ProductNodeShape[Level, C, M, U, P] {

  override def toNode(value: Any): TypeMapping =
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
  override def copy(s: Seq[Shape[? <: ShapeLevel, ?, ?, ?]]) =
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
        ShapedValue(sh.pack(value.asInstanceOf[sh.Mixed]).asInstanceOf[R], sh.packedShape.asInstanceOf[Shape[FlatShapeLevel, R, U, ?]])
    }

  /** The Shape for a ProvenShape */
  implicit def provenShapeShape[T, P](implicit shape: Shape[? <: FlatShapeLevel, T, T, P]
                                     ): Shape[FlatShapeLevel, ProvenShape[T], T, P] =
    new Shape[FlatShapeLevel, ProvenShape[T], T, P] {
      def pack(value: Any): Packed = {
        val v = value.asInstanceOf[ProvenShape[T]]
      v.shape.pack(v.value).asInstanceOf[Packed]
    }

    def packedShape: Shape[FlatShapeLevel, Packed, Unpacked, Packed] =
      shape.packedShape.asInstanceOf[Shape[FlatShapeLevel, Packed, Unpacked, Packed]]
    def buildParams(extract: Any => Unpacked): Packed =
      shape.buildParams(extract)
      def encodeRef(value: Any, path: Node) = {
        val v = value.asInstanceOf[ProvenShape[T]]
      v.shape.encodeRef(v.value, path)
      }

    def toNode(value: Any): Node = {
        val v = value.asInstanceOf[ProvenShape[T]]
      v.shape.toNode(v.value)
    }

    override def toString = s"ProvenShapeShape($shape)"
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
