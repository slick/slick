package slick.lifted

import slick.ast.{MappedScalaType, Node}

import scala.reflect.ClassTag

/** A value together with its Shape */
case class ShapedValue[T, U](value: T, shape: Shape[_ <: FlatShapeLevel, T, U, _]) extends Rep[U] {
  def encodeRef(path: Node): ShapedValue[T, U] = {
    val fv = shape.encodeRef(value, path).asInstanceOf[T]
    if(fv.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) this else new ShapedValue(fv, shape)
  }
  def toNode = shape.toNode(value)
  def packedValue[R](implicit ev: Shape[_ <: FlatShapeLevel, T, _, R]): ShapedValue[R, U] = ShapedValue(shape.pack(value).asInstanceOf[R], shape.packedShape.asInstanceOf[Shape[FlatShapeLevel, R, U, _]])
  def zip[T2, U2](s2: ShapedValue[T2, U2]) = new ShapedValue[(T, T2), (U, U2)]((value, s2.value), Shape.tuple2Shape(shape, s2.shape))
  def <>[R : ClassTag](f: (U => R), g: (R => Option[U])) = new MappedProjection[R, U](shape.toNode(value), MappedScalaType.Mapper(g.andThen(_.get).asInstanceOf[Any => Any], f.asInstanceOf[Any => Any], None), implicitly[ClassTag[R]])
  @inline def shaped: ShapedValue[T, U] = this
}
