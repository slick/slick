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
@implicitNotFound(msg = "No Shape found for unpacking ${Mixed_} to ${Unpacked_} and packing it to ${Packed_}")
abstract class Shape[-Mixed_, Unpacked_, Packed_] {
  type Mixed = Mixed_ @uncheckedVariance
  type Unpacked = Unpacked_
  type Packed = Packed_

  /** Convert a value of this Shape's (mixed) type to the fully packed type */
  def pack(value: Mixed): Packed

  /** Return the fully packed Shape */
  def packedShape: Shape[Packed, Unpacked, Packed]

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
  @inline implicit def columnShape[T]: Shape[Column[T], T, Column[T]] =
    repShape.asInstanceOf[Shape[Column[T], T, Column[T]]]

  /** Shape for Rep values (always fully packed) */
  val repShape: Shape[Rep[_], Any, Rep[_]] = new Shape[Rep[_], Any, Rep[_]] {
    def pack(value: Mixed): Packed = value
    def packedShape: Shape[Packed, Unpacked, Packed] = this
    def buildParams(extract: Any => Unpacked): Packed =
      throw new SlickException("Shape does not have the same Mixed and Unpacked type")
    def encodeRef(value: Mixed, path: List[Symbol]) = value.encodeRef(path)
    def toNode(value: Mixed): Node = value.toNode
  }

  @inline implicit def provenShape[T, P](implicit shape: Shape[T, _, P]): Shape[ProvenShape[T], T, P] = new Shape[ProvenShape[T], T, P] {
    def pack(value: Mixed): Packed =
      value.shape.pack(value.value.asInstanceOf[value.shape.Mixed]).asInstanceOf[Packed]
    def packedShape: Shape[Packed, Unpacked, Packed] =
      shape.packedShape.asInstanceOf[Shape[Packed, Unpacked, Packed]]
    def buildParams(extract: Any => Unpacked): Packed =
      shape.buildParams(extract.asInstanceOf[Any => shape.Unpacked])
    def encodeRef(value: Mixed, path: List[Symbol]) =
      value.shape.encodeRef(value.value.asInstanceOf[value.shape.Mixed], path)
    def toNode(value: Mixed): Node =
      value.shape.toNode(value.value.asInstanceOf[value.shape.Mixed])
  }
}

class ShapeLowPriority extends ShapeLowPriority2 {
  @inline implicit final def columnBaseShape[T, C <: ColumnBase[_]](implicit ev: C <:< ColumnBase[T]): Shape[C, T, C] =
    Shape.repShape.asInstanceOf[Shape[C, T, C]]

  implicit final def primitiveShape[T](implicit tm: TypedType[T]): Shape[T, T, Column[T]] = new Shape[T, T, Column[T]] {
    def pack(value: Mixed) = ConstColumn(value)
    def packedShape: Shape[Packed, Unpacked, Packed] = columnBaseShape[T, Column[T]]
    def buildParams(extract: Any => Unpacked): Packed = Column.forNode[T](new QueryParameter(extract, tm))(tm)
    def encodeRef(value: Mixed, path: List[Symbol]) =
      throw new SlickException("Shape does not have the same Mixed and Packed type")
    def toNode(value: Mixed): Node = pack(value).toNode
  }
}

/** Base class for Shapes that are represented by ProductNodes in the AST. */
abstract class ProductNodeShape[C, M <: C, U <: C, P <: C] extends Shape[M, U, P] {
  val shapes: Seq[Shape[_, _, _]]
  def buildValue(elems: IndexedSeq[Any]): Any
  def copy(shapes: Seq[Shape[_, _, _]]): Shape[_, _, _]
  def getIterator(value: C): Iterator[Any]
  def getElement(value: C, idx: Int): Any

  def pack(value: Mixed) = {
    val elems = shapes.iterator.zip(getIterator(value)).map{ case (p, f) => p.pack(f.asInstanceOf[p.Mixed]) }
    buildValue(elems.toIndexedSeq).asInstanceOf[Packed]
  }
  def packedShape: Shape[Packed, Unpacked, Packed] =
    copy(shapes.map(_.packedShape)).asInstanceOf[Shape[Packed, Unpacked, Packed]]
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
abstract class MappedProductShape[C, M <: C, U <: C, P <: C] extends ProductNodeShape[C, M, U, P] {
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

/** Shape for Scala tuples of all arities */
final class TupleShape[M <: Product, U <: Product, P <: Product](val shapes: Shape[_, _, _]*) extends ProductNodeShape[Product, M, U, P] {
  def getIterator(value: Product) = value.productIterator
  def getElement(value: Product, idx: Int) = value.productElement(idx)
  def buildValue(elems: IndexedSeq[Any]) = TupleSupport.buildTuple(elems)
  def copy(shapes: Seq[Shape[_, _, _]])  = new TupleShape(shapes: _*)
}

/** A value together with its Shape */
case class ShapedValue[T, U](value: T, shape: Shape[T, U, _]) {
  def encodeRef(path: List[Symbol]): ShapedValue[T, U] = {
    val fv = shape.encodeRef(value, path).asInstanceOf[T]
    if(fv.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) this else new ShapedValue(fv, shape)
  }
  def toNode = shape.toNode(value)
  def packedValue[R](implicit ev: Shape[T, _, R]): ShapedValue[R, U] = ShapedValue(shape.pack(value).asInstanceOf[R], shape.packedShape.asInstanceOf[Shape[R, U, _]])
  def zip[T2, U2](s2: ShapedValue[T2, U2]) = new ShapedValue[(T, T2), (U, U2)]((value, s2.value), Shape.tuple2Shape(shape, s2.shape))
  @inline def <>[R](f: (U => R), g: (R => Option[U])) = new MappedProjection[R, U](shape.toNode(value), f, g.andThen(_.get))
}

// Work-around for SI-3346
final class ToShapedValue[T](val value: T) extends AnyVal {
  @inline def shaped[U](implicit shape: Shape[T, U, _]) = new ShapedValue[T, U](value, shape)
  @inline def <>[R, U](f: (U => R), g: (R => Option[U]))(implicit shape: Shape[T, U, _]) = new MappedProjection[R, U](shape.toNode(value), f, g.andThen(_.get))
}

/** A limited version of ShapedValue which can be constructed for every type
  * that has a valid shape. We use it to enforce that a table's * projection
  * has a valid shape. A ProvenShape has itself a Shape so it can be used in
  * place of the value that it wraps for purposes of packing and unpacking. */
trait ProvenShape[U] {
  def value: Any
  val shape: Shape[_, U, _]
  def packedValue[R](implicit ev: Shape[_, U, R]): ShapedValue[R, U]
  def toNode = packedValue.toNode
}

object ProvenShape {
  /** Convert an appropriately shaped value to a ProvenShape */
  implicit def proveShapeOf[T, U](v: T)(implicit sh: Shape[T, U, _]): ProvenShape[U] =
    new ProvenShape[U] {
      def value = v
      val shape: Shape[_, U, _] = sh
      def packedValue[R](implicit ev: Shape[_, U, R]): ShapedValue[R, U] = ShapedValue(sh.pack(value).asInstanceOf[R], sh.packedShape.asInstanceOf[Shape[R, U, _]])
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
