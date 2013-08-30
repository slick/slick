package scala.slick.lifted

import scala.slick.SlickException
import scala.slick.ast.{QueryParameter, TypedType}
import scala.slick.profile.BasicProfile

final class Parameters[PU, PP](c: PP) {
  def flatMap[QU](f: PP => Query[_, QU])(implicit profile: BasicProfile): profile.ParameterizedQuery[PU, QU] =
    profile.compileParameterizedQuery[PU, QU](f(c))

  def map[QM, QU](f: PP => QM)(implicit profile: BasicProfile, shape: Shape[_ <: ShapeLevel.Flat, QM, QU, _]): profile.ParameterizedQuery[PU, QU] =
    profile.compileParameterizedQuery[PU, QU](Query(f(c)))

  def filter(f: PP => Boolean): Parameters[PU, PP] =
    if (!f(c)) throw new SlickException("Match failed when unpacking Parameters")
    else this

  def withFilter(f: PP => Boolean) = filter(f)
}

object Parameters {
  def apply[U](implicit shape: Shape[_ <: ShapeLevel.Flat, U, U, _]): Parameters[U, shape.Packed] =
    new Parameters[U, shape.Packed](shape.buildParams(_.asInstanceOf[U]))
}
