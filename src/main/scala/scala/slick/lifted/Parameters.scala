package scala.slick.lifted

import scala.slick.SlickException
import scala.slick.driver.{BasicProfile, BasicQueryTemplate}
import scala.slick.util.NaturalTransformation2

final class Parameters[PU, PP](c: PP) {
  def flatMap[QU](f: PP => Query[_, QU])(implicit profile: BasicProfile): BasicQueryTemplate[PU, QU] =
    profile.createQueryTemplate[PU, QU](f(c))

  def map[QM, QU](f: PP => QM)(implicit profile: BasicProfile, shape: Shape[QM, QU, _]): BasicQueryTemplate[PU, QU] =
    profile.createQueryTemplate[PU, QU](Query(f(c)))

  def filter(f: PP => Boolean): Parameters[PU, PP] =
    if (!f(c)) throw new SlickException("Match failed when unpacking Parameters")
    else this

  def withFilter(f: PP => Boolean) = filter(f)
}

object Parameters {
  def apply[U](implicit shape: Shape[U, U, _]): Parameters[U, shape.Packed] = {
    var idx = -1
    val params: shape.Packed = shape.buildPacked(new NaturalTransformation2[TypeMapper, ({ type L[X] = U => X })#L, Column] {
      def apply[T](tm: TypeMapper[T], f: U => T) =
        new ParameterColumn[T]({ idx += 1; idx }, f)(tm)
    })
    new Parameters[U, shape.Packed](params)
  }
}
