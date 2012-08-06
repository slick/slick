package scala.slick.lifted

import scala.language.existentials
import scala.annotation.implicitNotFound
import scala.slick.SlickException
import scala.slick.util.{ProductLinearizer, ValueLinearizer, NaturalTransformation2, RecordLinearizer}
import scala.slick.ast.{WithOp, TableNode, Node, Symbol}

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
  def pack(from: Mixed): Packed
  def packedShape: Shape[Packed, Unpacked, Packed]
  def linearizer(from: Mixed): ValueLinearizer[Unpacked]

  /** Build a packed representation from the shape and the TypeMappers alone.
   * This method is not available for shapes where Mixed and Unpacked are
   * different types.
   */
  def buildPacked(f: NaturalTransformation2[TypeMapper, ({ type L[X] = Unpacked => X })#L, Column]): Packed

  protected[this] def impureShape =
    throw new SlickException("Shape does not have the same Mixed and Unpacked type")
}

object Shape extends ShapeLowPriority {
  @inline def tableShape[T <: TableNode]: Shape[T, NothingContainer#TableNothing, T] =
    sharedTableShape.asInstanceOf[Shape[T, NothingContainer#TableNothing, T]]

  @inline implicit def columnShape[T]: Shape[Column[T], T, Column[T]] =
    selfLinearizingShape.asInstanceOf[Shape[Column[T], T, Column[T]]]

  val selfLinearizingShape: Shape[ValueLinearizer[_], Any, ValueLinearizer[_]] = new IdentityShape[ValueLinearizer[_], Any] {
    def linearizer(from: Mixed) = from.asInstanceOf[ValueLinearizer[Unpacked]]
    def buildPacked(f: NaturalTransformation2[TypeMapper, ({ type L[X] = Unpacked => X })#L, Column]) = impureShape
  }

  val sharedTableShape: Shape[TableNode, Any, TableNode] = new IdentityShape[TableNode, Any] {
    def linearizer(from: Mixed) = from.nodeShaped_*.value.asInstanceOf[ValueLinearizer[Unpacked]]
    def buildPacked(f: NaturalTransformation2[TypeMapper, ({ type L[X] = Unpacked => X })#L, Column]) = impureShape
  }
}

abstract class IdentityShape[Packed, Unpacked] extends Shape[Packed, Unpacked, Packed] {
  def pack(from: Mixed): Packed = from
  def packedShape: Shape[Packed, Unpacked, Packed] = this
}

class ShapeLowPriority extends ShapeLowPriority2 {
  @inline implicit final def unpackColumnBase[T, C <: ColumnBase[_]](implicit ev: C <:< ColumnBase[T]): Shape[C, T, C] =
    Shape.selfLinearizingShape.asInstanceOf[Shape[C, T, C]]

  implicit final def unpackPrimitive[T](implicit tm: TypeMapper[T]): Shape[T, T, Column[T]] = new Shape[T, T, Column[T]] {
    def pack(from: Mixed) = ConstColumn(from)
    def packedShape: Shape[Packed, Unpacked, Packed] = unpackColumnBase[T, Column[T]]
    def linearizer(from: Mixed) = ConstColumn(from)
    def buildPacked(f: NaturalTransformation2[TypeMapper, ({ type L[X] = Unpacked => X })#L, Column]): Packed =
      f(tm, identity)
  }
}

final class TupleShape[M <: Product, U <: Product, P <: Product](ps: Shape[_, _, _]*) extends Shape[M, U, P] {
  def pack(from: Mixed) =
    Shape.buildTuple(ps.iterator.zip(from.productIterator).map{case (p, f) => p.pack(f.asInstanceOf[p.Mixed])}.toIndexedSeq).asInstanceOf[Packed]
  def packedShape: Shape[Packed, Unpacked, Packed] =
    new TupleShape(ps.map(_.packedShape): _*)
  def linearizer(from: Mixed) =
    new ProductLinearizer(ps.iterator.zip(from.productIterator).map{case (p, f) => p.linearizer(f.asInstanceOf[p.Mixed]).asInstanceOf[RecordLinearizer[_]]}.toIndexedSeq)
  def buildPacked(f: NaturalTransformation2[TypeMapper, ({ type L[X] = Unpacked => X })#L, Column]): Packed =
    Shape.buildTuple(ps.iterator.zipWithIndex.map{ case (p, i) => p.buildPacked(productTf(i, f)) }.toIndexedSeq).asInstanceOf[Packed]

  private[this] def productTf[Unpacked <: Product, U](idx: Int,
      f: NaturalTransformation2[TypeMapper, ({ type L[X] = Unpacked => X })#L, Column]): NaturalTransformation2[TypeMapper, ({ type L[X] = U => X })#L, Column] =
    new NaturalTransformation2[TypeMapper, ({ type L[X] = U => X })#L, Column] {
      def apply[T](p1: TypeMapper[T], p2: (U => T)) = f.apply[T](p1, (u => p2(u.productElement(idx).asInstanceOf[U])))
    }
}

/** A value together with its Shape
  */
case class ShapedValue[T, U](value: T, shape: Shape[T, U, _]) {
  def encodeRef(s: Symbol, positions: List[Int] = Nil): ShapedValue[T, U] = {
    val fv = WithOp.encodeRef(value, s, positions)
    if(fv.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) this else new ShapedValue(fv, shape)
  }
  def packedNode = Node(shape.pack(value))
  def packedValue[R](implicit ev: Shape[T, _, R]): ShapedValue[R, U] = ShapedValue(shape.pack(value).asInstanceOf[R], shape.packedShape.asInstanceOf[Shape[R, U, _]])
  def linearizer = shape.linearizer(value).asInstanceOf[ValueLinearizer[U]]
  def zip[T2, U2](s2: ShapedValue[T2, U2]) = new ShapedValue[(T, T2), (U, U2)]((value, s2.value), Shape.tuple2Shape(shape, s2.shape))
}

object ShapedValue {
  // Should be implicit for using ShapedValue as a view bound, but SI-3346 prevents this use case
  @inline def createShapedValue[T, U](value: T)(implicit shape: Shape[T, U, _]) = ShapedValue(value, shape)
}

// Work-around for SI-3346
final class ToShapedValue[T](val value: T) extends AnyVal {
  @inline def shaped[U](implicit shape: Shape[T, U, _]) = new ShapedValue[T, U](value, shape)
}
