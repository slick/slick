package slick.util

import scala.collection.compat._
import scala.collection.mutable.Builder

private[slick] object VersionCompat {

  def partiallyApply[From, A, C](bf: BuildFrom[From, A, C])(from: From): Factory[A, C] = new Factory[A, C] {
    def apply(): Builder[A,C] = bf.newBuilder(from)
    def apply(from: Nothing): Builder[A,C] = apply()
  }
}
