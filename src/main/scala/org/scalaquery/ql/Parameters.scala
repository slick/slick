package org.scalaquery.ql

import org.scalaquery.SQueryException
import org.scalaquery.ql.basic.{BasicProfile, BasicQueryTemplate}
import org.scalaquery.util.NaturalTransformation2

final class Parameters[P, C](c: C) {
  def flatMap[F](f: C => Query[_, F])(implicit profile: BasicProfile): BasicQueryTemplate[P, F] =
    profile.createQueryTemplate[P, F](f(c))

  def map[F](f: C => ColumnBase[F])(implicit profile: BasicProfile): BasicQueryTemplate[P, F] =
    profile.createQueryTemplate[P, F](Query(f(c)))

  def filter(f: C => Boolean): Parameters[P, C] =
    if (!f(c)) throw new SQueryException("Match failed when unpacking Parameters")
    else this

  def withFilter(f: C => Boolean) = filter(f)
}

object Parameters {
  def apply[U](implicit packing: Packing[U, U, _]): Parameters[U, packing.Packed] = {
    var idx = -1
    val params: packing.Packed = packing.buildPacked(new NaturalTransformation2[TypeMapper, ({ type L[X] = U => X })#L, Column] {
      def apply[T](tm: TypeMapper[T], f: U => T) =
        new ParameterColumn[T]({ idx += 1; idx }, f)(tm)
    })
    new Parameters[U, packing.Packed](params)
  }
}
