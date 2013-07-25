package scala.slick.lifted

import scala.language.{existentials, implicitConversions}
import scala.annotation.implicitNotFound
import scala.slick.SlickException
import scala.slick.util.TupleSupport
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
@implicitNotFound(msg = "Don't know how to unpack ${Mixed_} to ${Unpacked_} and pack to ${Packed_}")
abstract class Shape[-Mixed_, Unpacked_, Packed_] {
  type Mixed = Mixed_
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
  def encodeRef(value: Mixed, sym: Symbol, positions: List[Int] = Nil): Any

  /** Return an AST Node representing a mixed value. */
  def toNode(value: Mixed): Node
}

object Shape extends ShapeLowPriority {
  @inline implicit def columnShape[T]: Shape[Column[T], T, Column[T]] =
    encodeRefShape.asInstanceOf[Shape[Column[T], T, Column[T]]]

  val encodeRefShape: Shape[EncodeRef with NodeGenerator, Any, EncodeRef with NodeGenerator] = new Shape[EncodeRef with NodeGenerator, Any, EncodeRef with NodeGenerator] {
    def pack(value: Mixed): Packed = value
    def packedShape: Shape[Packed, Unpacked, Packed] = this
    def buildParams(extract: Any => Unpacked): Packed =
      throw new SlickException("Shape does not have the same Mixed and Unpacked type")
    def encodeRef(value: Mixed, sym: Symbol, positions: List[Int] = Nil) =
      value.encodeRef(sym, positions)
    def toNode(value: Mixed): Node = Node(value)
  }

  @inline implicit def provenShape[T, P](implicit shape: Shape[T, _, P]): Shape[ProvenShape[T], T, P] = new Shape[ProvenShape[T], T, P] {
    def pack(value: Mixed): Packed =
      value.shape.pack(value.value.asInstanceOf[value.shape.Mixed]).asInstanceOf[Packed]
    def packedShape: Shape[Packed, Unpacked, Packed] =
      shape.packedShape.asInstanceOf[Shape[Packed, Unpacked, Packed]]
    def buildParams(extract: Any => Unpacked): Packed =
      shape.buildParams(extract.asInstanceOf[Any => shape.Unpacked])
    def encodeRef(value: Mixed, sym: Symbol, positions: List[Int] = Nil) =
      value.shape.encodeRef(value.value.asInstanceOf[value.shape.Mixed], sym, positions)
    def toNode(value: Mixed): Node =
      value.shape.toNode(value.value.asInstanceOf[value.shape.Mixed])
  }
}

class ShapeLowPriority extends ShapeLowPriority2 {
  @inline implicit final def columnBaseShape[T, C <: ColumnBase[_]](implicit ev: C <:< ColumnBase[T]): Shape[C, T, C] =
    Shape.encodeRefShape.asInstanceOf[Shape[C, T, C]]

  implicit final def primitiveShape[T](implicit tm: TypedType[T]): Shape[T, T, Column[T]] = new Shape[T, T, Column[T]] {
    def pack(value: Mixed) = ConstColumn(value)
    def packedShape: Shape[Packed, Unpacked, Packed] = columnBaseShape[T, Column[T]]
    def buildParams(extract: Any => Unpacked): Packed = Column.forNode[T](new QueryParameter(extract, tm))(tm)
    def encodeRef(value: Mixed, sym: Symbol, positions: List[Int] = Nil) =
      throw new SlickException("Shape does not have the same Mixed and Packed type")
    def toNode(value: Mixed): Node = Node(pack(value))
  }
}

final class TupleShape[M <: Product, U <: Product, P <: Product](ps: Shape[_, _, _]*) extends Shape[M, U, P] {
  def pack(value: Mixed) = {
    val elems = ps.iterator.zip(value.productIterator).map{ case (p, f) => p.pack(f.asInstanceOf[p.Mixed]) }
    TupleSupport.buildTuple(elems.toIndexedSeq).asInstanceOf[Packed]
  }
  def packedShape: Shape[Packed, Unpacked, Packed] = new TupleShape(ps.map(_.packedShape): _*)
  def buildParams(extract: Any => Unpacked): Packed = {
    val elems = ps.iterator.zipWithIndex.map { case (p, idx) =>
      def chExtract(u: Unpacked): p.Unpacked = u.productElement(idx).asInstanceOf[p.Unpacked]
      p.buildParams(extract.andThen(chExtract))
    }
    TupleSupport.buildTuple(elems.toIndexedSeq).asInstanceOf[Packed]
  }
  def encodeRef(value: Mixed, sym: Symbol, positions: List[Int] = Nil) = {
    val elems = ps.iterator.zip(value.productIterator).zipWithIndex.map {
      case ((p, x), pos) => p.encodeRef(x.asInstanceOf[p.Mixed], sym, (pos + 1) :: positions)
    }
    TupleSupport.buildTuple(elems.toIndexedSeq)
  }
  def toNode(value: Mixed) = {
    val elems = ps.iterator.zip(value.productIterator).map{ case (p, f) => p.toNode(f.asInstanceOf[p.Mixed]) }
    ProductNode(elems.toSeq)
  }
}

/** A value together with its Shape
  */
case class ShapedValue[T, U](value: T, shape: Shape[T, U, _]) {
  def encodeRef(sym: Symbol, positions: List[Int] = Nil): ShapedValue[T, U] = {
    val fv = shape.encodeRef(value, sym, positions).asInstanceOf[T]
    if(fv.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) this else new ShapedValue(fv, shape)
  }
  def packedNode = shape.toNode(value)
  def packedValue[R](implicit ev: Shape[T, _, R]): ShapedValue[R, U] = ShapedValue(shape.pack(value).asInstanceOf[R], shape.packedShape.asInstanceOf[Shape[R, U, _]])
  def zip[T2, U2](s2: ShapedValue[T2, U2]) = new ShapedValue[(T, T2), (U, U2)]((value, s2.value), Shape.tuple2Shape(shape, s2.shape))
  @inline def <>[R](f: (U => R), g: (R => Option[U])) = new MappedProjection[R, U](shape.toNode(value), f, g.andThen(_.get))
}

object ShapedValue {
  // Should be implicit for using ShapedValue as a view bound, but SI-3346 prevents this use case
  @inline def createShapedValue[T, U](value: T)(implicit shape: Shape[T, U, _]) = ShapedValue(value, shape)
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
trait ProvenShape[U] extends NodeGenerator {
  def value: Any
  val shape: Shape[_, U, _]
  def packedValue[R](implicit ev: Shape[_, U, R]): ShapedValue[R, U]
  def nodeDelegate = packedValue.packedNode
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

class MappedProjection[T, P](child: Node, f: (P => T), g: (T => P)) extends ColumnBase[T] with NodeGenerator with EncodeRef {
  type Self = MappedProjection[_, _]
  override def toString = "MappedProjection"
  private def typeMapping = TypeMapping(child, (v => g(v.asInstanceOf[T])), (v => f(v.asInstanceOf[P])))
  override def nodeDelegate: Node = typeMapping
  def encodeRef(sym: Symbol, positions: List[Int] = Nil): MappedProjection[T, P] = new MappedProjection[T, P](child, f, g) {
    override def nodeDelegate = Path(positions.map(ElementSymbol) :+ sym)
  }
}

/** A trait for encoding refs directly into a value. This needs to be
  * implemented by values that should use Shape.encodeRefShape. */
trait EncodeRef {
  def encodeRef(sym: Symbol, positions: List[Int] = Nil): Any
}
