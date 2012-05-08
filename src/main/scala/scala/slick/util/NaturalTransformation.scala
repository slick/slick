package scala.slick.util

import scala.language.higherKinds

/**
 * Natural Transformations.
 */

abstract class ~> [-P1[_], +R[_]] {
  def apply[T](v1: P1[T]): R[T]
}

abstract class NaturalTransformation2 [-P1[_], -P2[_], +R[_]] {
  def apply[T](v1: P1[T], v2: P2[T]): R[T]
}
