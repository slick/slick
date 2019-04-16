package slick.util

import scala.collection.{BuildFrom, Factory}
import scala.collection.mutable.Builder

private[slick] object VersionCompat {

  // This should have been a method on `BuildFrom`. Maybe in Scala 2.14?
  // See https://github.com/scala/scala-collection-compat/issues/196 for details
  def partiallyApply[From, A, C](bf: BuildFrom[From, A, C])(from: From): Factory[A, C] = new Factory[A, C] {
    def fromSpecific(it: IterableOnce[A]): C = bf.fromSpecific(from)(it)
    def newBuilder: Builder[A, C] = bf.newBuilder(from)
  }
}
