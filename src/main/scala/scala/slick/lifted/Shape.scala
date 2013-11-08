package scala.slick.lifted

import scala.language.{existentials, implicitConversions}
import scala.annotation.implicitNotFound
import scala.annotation.unchecked.uncheckedVariance
import scala.slick.SlickException
import scala.slick.util.{ProductWrapper, TupleSupport}
import scala.slick.ast._

/** A type class that encodes the unpacking `Mixed => Unpacked` of a
 * `Query[Mixed]` to its result element type `Unpacked` and the packing to a
 * fully packed type `Packed`, i.e. a type where everything which is not a
 * transparent container is wrapped in a `Column[_]`.
 *
 * =Example:=
 * - Mixed: (Column[Int], Column[(Int, String)], (Int, Option[Double]))
 * - Unpacked: (Int, (Int, String), (Int, Option[Double]))
 * - Packed: (Column[Int], Column[(Int, String)], (Column[Int], Column[Option[Double]]))
 * - Linearized: (Int, Int, String, Int, Option[Double])
 */
@implicitNotFound(msg = "No matching Shape found.\nSlick does not know how to map the given types.\nPossible causes: T in Table[T] does not match your * projection. Or you use an unsupported type in a Query (e.g. scala List).\n  Required level: ${Level}\n     Source type: ${Mixed_}\n   Unpacked type: ${Unpacked_}\n     Packed type: ${Packed_}\n")
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
  def encodeRef(value: Mixed, path: List[Symbol]): Any

  /** Return an AST Node representing a mixed value. */
  def toNode(value: Mixed): Node
}

object Shape extends ShapeLowPriority {
  /** A Shape for ConstColumns. It is identical to `columnShape` but it
    * ensures that a `ConstColumn[T]` packs to itself, not just to
    * `Column[T]`. This allows ConstColumns to be used as fully packed
    * types when compiling query functions. */
  @inline implicit def constColumnShape[T, Level <: ShapeLevel]: Shape[Level, ConstColumn[T], T, ConstColumn[T]] =
    repShape.asInstanceOf[Shape[Level, ConstColumn[T], T, ConstColumn[T]]]
}

trait ShapeLowPriority extends ShapeLowPriority1 {
  /** A Shape for Columns. */
  @inline implicit def columnShape[T, Level <: ShapeLevel]: Shape[Level, Column[T], T, Column[T]] =
    repShape.asInstanceOf[Shape[Level, Column[T], T, Column[T]]]

  /** Shape for Rep values (always fully packed) */
  val repShape: Shape[ShapeLevel.Flat, Rep[_], Any, Rep[_]] = new Shape[ShapeLevel.Flat, Rep[_], Any, Rep[_]] {
    def pack(value: Mixed): Packed = value
    def packedShape: Shape[ShapeLevel.Flat, Packed, Unpacked, Packed] = this
    def buildParams(extract: Any => Unpacked): Packed =
      throw new SlickException("Shape does not have the same Mixed and Unpacked type")
    def encodeRef(value: Mixed, path: List[Symbol]) = value.encodeRef(path)
    def toNode(value: Mixed): Node = value.toNode
  }

  implicit def provenShape[T, P](implicit shape: Shape[_ <: ShapeLevel.Flat, T, _, P]): Shape[ShapeLevel.Flat, ProvenShape[T], T, P] = new Shape[ShapeLevel.Flat, ProvenShape[T], T, P] {
    def pack(value: Mixed): Packed =
      value.shape.pack(value.value.asInstanceOf[value.shape.Mixed]).asInstanceOf[Packed]
    def packedShape: Shape[ShapeLevel.Flat, Packed, Unpacked, Packed] =
      shape.packedShape.asInstanceOf[Shape[ShapeLevel.Flat, Packed, Unpacked, Packed]]
    def buildParams(extract: Any => Unpacked): Packed =
      shape.buildParams(extract.asInstanceOf[Any => shape.Unpacked])
    def encodeRef(value: Mixed, path: List[Symbol]) =
      value.shape.encodeRef(value.value.asInstanceOf[value.shape.Mixed], path)
    def toNode(value: Mixed): Node =
      value.shape.toNode(value.value.asInstanceOf[value.shape.Mixed])
  }

  @inline implicit def queryShape[Level >: ShapeLevel.Nested <: ShapeLevel, M, U]: Shape[Level, Query[M, U], Seq[U], Query[M, U]] =
    repShape.asInstanceOf[Shape[Level, Query[M, U], Seq[U], Query[M, U]]]
}

class ShapeLowPriority1 extends ShapeLowPriority2 {
  @inline implicit final def columnBaseShape[Level >: ShapeLevel.Flat <: ShapeLevel, T, C <: ColumnBase[_]](implicit ev: C <:< ColumnBase[T]): Shape[Level, C, T, C] =
    Shape.repShape.asInstanceOf[Shape[Level, C, T, C]]

  implicit final def primitiveShape[T, Level <: ShapeLevel](implicit tm: TypedType[T]): Shape[Level, T, T, Column[T]] = new Shape[Level, T, T, Column[T]] {
    def pack(value: Mixed) = LiteralColumn(value)
    def packedShape = Shape.repShape.asInstanceOf[Shape[Level, Packed, Unpacked, Packed]]
    def buildParams(extract: Any => Unpacked): Packed = new ParameterColumn[T](new QueryParameter(extract, tm))(tm)
    def encodeRef(value: Mixed, path: List[Symbol]) =
      throw new SlickException("Shape does not have the same Mixed and Packed type")
    def toNode(value: Mixed): Node = pack(value).toNode
  }

  @inline implicit final def unitShape[Level <: ShapeLevel]: Shape[Level, Unit, Unit, Unit] =
    unitShapePrototype.asInstanceOf[Shape[Level, Unit, Unit, Unit]]

  val unitShapePrototype: Shape[ShapeLevel.Flat, Unit, Unit, Unit] = new Shape[ShapeLevel.Flat, Unit, Unit, Unit] {
    def pack(value: Mixed) = ()
    def packedShape: Shape[ShapeLevel.Flat, Packed, Unpacked, Packed] = this
    def buildParams(extract: Any => Unpacked) = ()
    def encodeRef(value: Mixed, path: List[Symbol]) = ()
    def toNode(value: Mixed) = ProductNode(Nil)
  }
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
  val shapes: Seq[Shape[_, _, _, _]]

  /** Build a record value represented by this Shape from its element values. */
  def buildValue(elems: IndexedSeq[Any]): Any

  /** Create a copy of this Shape with new element Shapes. This is used for
    * packing Shapes recursively. */
  def copy(shapes: Seq[Shape[_ <: ShapeLevel, _, _, _]]): Shape[Level, _, _, _]

  /** Get the element value from a record value at the specified index. */
  def getElement(value: C, idx: Int): Any

  /** Get an Iterator of a record value's element values. The default
    * implementation repeatedly calls `getElement`. */
  def getIterator(value: C): Iterator[Any] =
    shapes.iterator.zipWithIndex.map(t => getElement(value, t._2))

  def pack(value: Mixed) = {
    val elems = shapes.iterator.zip(getIterator(value)).map{ case (p, f) => p.pack(f.asInstanceOf[p.Mixed]) }
    buildValue(elems.toIndexedSeq).asInstanceOf[Packed]
  }
  def packedShape: Shape[Level, Packed, Unpacked, Packed] =
    copy(shapes.map(_.packedShape.asInstanceOf[Shape[_ <: ShapeLevel, _, _, _]])).asInstanceOf[Shape[Level, Packed, Unpacked, Packed]]
  def buildParams(extract: Any => Unpacked): Packed = {
    val elems = shapes.iterator.zipWithIndex.map { case (p, idx) =>
      def chExtract(u: C): p.Unpacked = getElement(u, idx).asInstanceOf[p.Unpacked]
      p.buildParams(extract.andThen(chExtract))
    }
    buildValue(elems.toIndexedSeq).asInstanceOf[Packed]
  }
  def encodeRef(value: Mixed, path: List[Symbol]) = {
    val elems = shapes.iterator.zip(getIterator(value)).zipWithIndex.map {
      case ((p, x), pos) => p.encodeRef(x.asInstanceOf[p.Mixed], ElementSymbol(pos + 1) :: path)
    }
    buildValue(elems.toIndexedSeq)
  }
  def toNode(value: Mixed): Node = ProductNode(shapes.iterator.zip(getIterator(value)).map {
    case (p, f) => p.toNode(f.asInstanceOf[p.Mixed])
  }.toSeq)
}

/** Base class for ProductNodeShapes with a type mapping */
abstract class MappedProductShape[Level <: ShapeLevel, C, M <: C, U <: C, P <: C] extends ProductNodeShape[Level, C, M, U, P] {
  lazy val unary = shapes.length == 1
  override def toNode(value: Mixed) = TypeMapping(super.toNode(value), toBase, toMapped)
  def toBase(v: Any) = {
    val it = getIterator(v.asInstanceOf[C])
    if(unary) it.next() else new ProductWrapper(it.toIndexedSeq)
  }
  def toMapped(v: Any) = buildValue(
    if(unary) Vector(v) else TupleSupport.buildIndexedSeq(v.asInstanceOf[Product])
  )
}

/** Base class for ProductNodeShapes with a type mapping to a type that extends scala.Product */
abstract class MappedScalaProductShape[Level <: ShapeLevel, C <: Product, M <: C, U <: C, P <: C] extends MappedProductShape[Level, C, M, U, P] {
  override def getIterator(value: C) = value.productIterator
  def getElement(value: C, idx: Int) = value.productElement(idx)
}

/** Shape for Scala tuples of all arities */
final class TupleShape[Level <: ShapeLevel, M <: Product, U <: Product, P <: Product](val shapes: Shape[_, _, _, _]*) extends ProductNodeShape[Level, Product, M, U, P] {
  override def getIterator(value: Product) = value.productIterator
  def getElement(value: Product, idx: Int) = value.productElement(idx)
  def buildValue(elems: IndexedSeq[Any]) = TupleSupport.buildTuple(elems)
  def copy(shapes: Seq[Shape[_ <: ShapeLevel, _, _, _]])  = new TupleShape(shapes: _*)
}

/** The level of a Shape, i.e. what kind of types it allows.
  * This is used as a phantom type for Shape resolution. There are no
  * instances of any ShapeLevel. */
trait ShapeLevel
object ShapeLevel {
  /** Allows nested collections but no computations. */
  trait Nested extends ShapeLevel
  /** Does not allow nested collections.
    * This is the standard level for executable queries. */
  trait Flat extends Nested
  /** Only records of individual columns.
    * This level is used for parameters of compiled queries. */
  trait Columns extends Flat
}

/** A value together with its Shape */
case class ShapedValue[T, U](value: T, shape: Shape[_ <: ShapeLevel.Flat, T, U, _]) {
  def encodeRef(path: List[Symbol]): ShapedValue[T, U] = {
    val fv = shape.encodeRef(value, path).asInstanceOf[T]
    if(fv.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) this else new ShapedValue(fv, shape)
  }
  def toNode = shape.toNode(value)
  def packedValue[R](implicit ev: Shape[_ <: ShapeLevel.Flat, T, _, R]): ShapedValue[R, U] = ShapedValue(shape.pack(value).asInstanceOf[R], shape.packedShape.asInstanceOf[Shape[ShapeLevel.Flat, R, U, _]])
  def zip[T2, U2](s2: ShapedValue[T2, U2]) = new ShapedValue[(T, T2), (U, U2)]((value, s2.value), Shape.tuple2Shape(shape, s2.shape))
  @inline def <>[R](f: (U => R), g: (R => Option[U])) = new MappedProjection[R, U](shape.toNode(value), f, g.andThen(_.get))
}

// Work-around for SI-3346
final class ToShapedValue[T](val value: T) extends AnyVal {
  @inline def shaped[U](implicit shape: Shape[_ <: ShapeLevel.Flat, T, U, _]) = new ShapedValue[T, U](value, shape)
  @inline def <>[R, U](f: (U => R), g: (R => Option[U]))(implicit shape: Shape[_ <: ShapeLevel.Flat, T, U, _]) = new MappedProjection[R, U](shape.toNode(value), f, g.andThen(_.get))
}

/** A limited version of ShapedValue which can be constructed for every type
  * that has a valid shape. We use it to enforce that a table's * projection
  * has a valid shape. A ProvenShape has itself a Shape so it can be used in
  * place of the value that it wraps for purposes of packing and unpacking. */
trait ProvenShape[U] {
  def value: Any
  val shape: Shape[_ <: ShapeLevel.Flat, _, U, _]
  def packedValue[R](implicit ev: Shape[_ <: ShapeLevel.Flat, _, U, R]): ShapedValue[R, U]
  def toNode = packedValue(shape).toNode
}

object ProvenShape {
  /** Convert an appropriately shaped value to a ProvenShape */
  implicit def proveShapeOf[T, U](v: T)(implicit sh: Shape[_ <: ShapeLevel.Flat, T, U, _]): ProvenShape[U] =
    new ProvenShape[U] {
      def value = v
      val shape: Shape[_ <: ShapeLevel.Flat, _, U, _] = sh.asInstanceOf[Shape[ShapeLevel.Flat, _, U, _]]
      def packedValue[R](implicit ev: Shape[_ <: ShapeLevel.Flat, _, U, R]): ShapedValue[R, U] = ShapedValue(sh.pack(value).asInstanceOf[R], sh.packedShape.asInstanceOf[Shape[ShapeLevel.Flat, R, U, _]])
    }
}

class MappedProjection[T, P](child: Node, f: (P => T), g: (T => P)) extends ColumnBase[T] {
  type Self = MappedProjection[_, _]
  override def toString = "MappedProjection"
  override def toNode: Node = TypeMapping(child, (v => g(v.asInstanceOf[T])), (v => f(v.asInstanceOf[P])))
  def encodeRef(path: List[Symbol]): MappedProjection[T, P] = new MappedProjection[T, P](child, f, g) {
    override def toNode = Path(path)
  }
}
