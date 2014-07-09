package scala.slick.lifted

import scala.language.{existentials, implicitConversions, higherKinds}
import scala.annotation.implicitNotFound
import scala.annotation.unchecked.uncheckedVariance
import scala.slick.SlickException
import scala.slick.util.{ProductWrapper, TupleSupport}
import scala.slick.ast._
import scala.reflect.ClassTag

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

object Shape extends ShapeLowPriority2 {
  implicit final def primitiveShape[T, Level <: ShapeLevel](implicit tm: TypedType[T]): Shape[Level, T, T, ConstColumn[T]] = new Shape[Level, T, T, ConstColumn[T]] {
    def pack(value: Mixed) = LiteralColumn(value)
    def packedShape = RepShape[Level, Packed, Unpacked]
    def buildParams(extract: Any => Unpacked): Packed = new ConstColumn[T](new QueryParameter(extract, tm))(tm)
    def encodeRef(value: Mixed, path: List[Symbol]) =
      throw new SlickException("Shape does not have the same Mixed and Packed type")
    def toNode(value: Mixed): Node = pack(value).toNode
  }

  @inline implicit final def unitShape[Level <: ShapeLevel]: Shape[Level, Unit, Unit, Unit] =
    unitShapePrototype.asInstanceOf[Shape[Level, Unit, Unit, Unit]]

  val unitShapePrototype: Shape[FlatShapeLevel, Unit, Unit, Unit] = new Shape[FlatShapeLevel, Unit, Unit, Unit] {
    def pack(value: Mixed) = ()
    def packedShape: Shape[FlatShapeLevel, Packed, Unpacked, Packed] = this
    def buildParams(extract: Any => Unpacked) = ()
    def encodeRef(value: Mixed, path: List[Symbol]) = ()
    def toNode(value: Mixed) = ProductNode(Nil)
  }
}

trait RepColumnShapeImplicits {
  /** A Shape for single-column Reps. */
  @inline implicit def repColumnShape[T : TypedType, Level <: ShapeLevel] = RepShape[Level, Rep[T], T]
}

/** Shape for Rep values (always fully packed) */
object RepShape extends Shape[FlatShapeLevel, Rep[_], Any, Rep[_]] {
  def apply[Level <: ShapeLevel, MP <: Rep[_], U]: Shape[Level, MP, U, MP] = this.asInstanceOf[Shape[Level, MP, U, MP]]

  def pack(value: Mixed): Packed = value
  def packedShape: Shape[FlatShapeLevel, Packed, Unpacked, Packed] = this
  def buildParams(extract: Any => Unpacked): Packed =
    throw new SlickException("Shape does not have the same Mixed and Unpacked type")
  def encodeRef(value: Mixed, path: List[Symbol]) = value.encodeRef(path)
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
  override def toNode(value: Mixed) = TypeMapping(super.toNode(value), MappedScalaType.Mapper(toBase, toMapped, None), classTag)
  def toBase(v: Any) = new ProductWrapper(getIterator(v.asInstanceOf[C]).toIndexedSeq)
  def toMapped(v: Any) = buildValue(TupleSupport.buildIndexedSeq(v.asInstanceOf[Product]))
  def classTag: ClassTag[U]
}

/** Base class for ProductNodeShapes with a type mapping to a type that extends scala.Product */
abstract class MappedScalaProductShape[Level <: ShapeLevel, C <: Product, M <: C, U <: C, P <: C](implicit val classTag: ClassTag[U]) extends MappedProductShape[Level, C, M, U, P] {
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
class CaseClassShape[P <: Product, LiftedTuple, LiftedCaseClass <: P, PlainTuple, PlainCaseClass <: P](
   mapLifted: LiftedTuple => LiftedCaseClass, mapPlain: PlainTuple => PlainCaseClass)(
   implicit columnShapes: Shape[FlatShapeLevel, LiftedTuple, PlainTuple, LiftedTuple], classTag: ClassTag[PlainCaseClass])
extends MappedScalaProductShape[FlatShapeLevel, P, LiftedCaseClass, PlainCaseClass, LiftedCaseClass] {
  val shapes = columnShapes.asInstanceOf[TupleShape[_,_,_,_]].shapes
  override def toMapped(v: Any) = mapPlain(v.asInstanceOf[PlainTuple])
  def buildValue(elems: IndexedSeq[Any]) = mapLifted(TupleSupport.buildTuple(elems).asInstanceOf[LiftedTuple])
  def copy(s: Seq[Shape[_ <: ShapeLevel, _, _, _]]) = new CaseClassShape(mapLifted, mapPlain) { override val shapes = s }
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
  val shapes: Seq[Shape[_, _, _, _]],
  mapLifted: Seq[Any] => C,
  mapPlain:Seq[Any] => E
)(implicit classTag: ClassTag[E]) extends MappedScalaProductShape[
  FlatShapeLevel, Product, C, E, C
]{
  override def toMapped(v: Any) = mapPlain(v.asInstanceOf[Product].productIterator.toSeq)
  def buildValue(elems: IndexedSeq[Any]) = mapLifted(elems)
  def copy(s: Seq[Shape[_ <: ShapeLevel, _, _, _]]) = new ProductClassShape(s, mapLifted, mapPlain)
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
case class ShapedValue[T, U](value: T, shape: Shape[_ <: FlatShapeLevel, T, U, _]) extends Rep[U] {
  def encodeRef(path: List[Symbol]): ShapedValue[T, U] = {
    val fv = shape.encodeRef(value, path).asInstanceOf[T]
    if(fv.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) this else new ShapedValue(fv, shape)
  }
  def toNode = shape.toNode(value)
  def packedValue[R](implicit ev: Shape[_ <: FlatShapeLevel, T, _, R]): ShapedValue[R, U] = ShapedValue(shape.pack(value).asInstanceOf[R], shape.packedShape.asInstanceOf[Shape[FlatShapeLevel, R, U, _]])
  def zip[T2, U2](s2: ShapedValue[T2, U2]) = new ShapedValue[(T, T2), (U, U2)]((value, s2.value), Shape.tuple2Shape(shape, s2.shape))
  @inline def <>[R : ClassTag](f: (U => R), g: (R => Option[U])) = new MappedProjection[R, U](shape.toNode(value), MappedScalaType.Mapper(g.andThen(_.get).asInstanceOf[Any => Any], f.asInstanceOf[Any => Any], None), implicitly[ClassTag[R]])
}

object ShapedValue {
  @inline implicit def shapedValueShape[T, U, Level <: ShapeLevel] = RepShape[Level, ShapedValue[T, U], U]
}

// Work-around for SI-3346
final class ToShapedValue[T](val value: T) extends AnyVal {
  @inline def shaped[U](implicit shape: Shape[_ <: FlatShapeLevel, T, U, _]) = new ShapedValue[T, U](value, shape)
  @inline def <>[R : ClassTag, U](f: (U => R), g: (R => Option[U]))(implicit shape: Shape[_ <: FlatShapeLevel, T, U, _]) = new MappedProjection[R, U](shape.toNode(value), MappedScalaType.Mapper(g.andThen(_.get).asInstanceOf[Any => Any], f.asInstanceOf[Any => Any], None), implicitly[ClassTag[R]])
}

/** A limited version of ShapedValue which can be constructed for every type
  * that has a valid shape. We use it to enforce that a table's * projection
  * has a valid shape. A ProvenShape has itself a Shape so it can be used in
  * place of the value that it wraps for purposes of packing and unpacking. */
trait ProvenShape[U] {
  def value: Any
  val shape: Shape[_ <: FlatShapeLevel, _, U, _]
  def packedValue[R](implicit ev: Shape[_ <: FlatShapeLevel, _, U, R]): ShapedValue[R, U]
  def toNode = packedValue(shape).toNode
}

object ProvenShape {
  /** Convert an appropriately shaped value to a ProvenShape */
  implicit def proveShapeOf[T, U](v: T)(implicit sh: Shape[_ <: FlatShapeLevel, T, U, _]): ProvenShape[U] =
    new ProvenShape[U] {
      def value = v
      val shape: Shape[_ <: FlatShapeLevel, _, U, _] = sh.asInstanceOf[Shape[FlatShapeLevel, _, U, _]]
      def packedValue[R](implicit ev: Shape[_ <: FlatShapeLevel, _, U, R]): ShapedValue[R, U] = ShapedValue(sh.pack(value).asInstanceOf[R], sh.packedShape.asInstanceOf[Shape[FlatShapeLevel, R, U, _]])
    }

  /** The Shape for a ProvenShape */
  implicit def provenShapeShape[T, P](implicit shape: Shape[_ <: FlatShapeLevel, T, T, P]): Shape[FlatShapeLevel, ProvenShape[T], T, P] = new Shape[FlatShapeLevel, ProvenShape[T], T, P] {
    def pack(value: Mixed): Packed =
      value.shape.pack(value.value.asInstanceOf[value.shape.Mixed]).asInstanceOf[Packed]
    def packedShape: Shape[FlatShapeLevel, Packed, Unpacked, Packed] =
      shape.packedShape.asInstanceOf[Shape[FlatShapeLevel, Packed, Unpacked, Packed]]
    def buildParams(extract: Any => Unpacked): Packed =
      shape.buildParams(extract.asInstanceOf[Any => shape.Unpacked])
    def encodeRef(value: Mixed, path: List[Symbol]) =
      value.shape.encodeRef(value.value.asInstanceOf[value.shape.Mixed], path)
    def toNode(value: Mixed): Node =
      value.shape.toNode(value.value.asInstanceOf[value.shape.Mixed])
  }
}

class MappedProjection[T, P](child: Node, mapper: MappedScalaType.Mapper, classTag: ClassTag[T]) extends Rep[T] {
  type Self = MappedProjection[_, _]
  override def toString = "MappedProjection"
  override def toNode: Node = TypeMapping(child, mapper, classTag)
  def encodeRef(path: List[Symbol]): MappedProjection[T, P] = new MappedProjection[T, P](child, mapper, classTag) {
    override def toNode = Path(path)
  }
  def genericFastPath(pf: PartialFunction[Any, Any]) = new MappedProjection[T, P](child, mapper.copy(fastPath = Some(pf)), classTag)
}

object MappedProjection {
  /** The Shape for a MappedProjection */
  @inline implicit final def mappedProjectionShape[Level >: FlatShapeLevel <: ShapeLevel, T, P] = RepShape[Level, MappedProjection[T, P], T]
}
