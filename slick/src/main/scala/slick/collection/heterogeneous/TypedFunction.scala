package slick.collection.heterogeneous

import scala.language.higherKinds

/** A function which operates at the type and value levels. */
trait TypedFunction

/** A binary function which operates at the type and value levels. */
trait TypedFunction2[-T1, -T2, +TR, F[_ <: T1, _ <: T2] <: TR] extends TypedFunction {
  def apply[P1 <: T1, P2 <: T2](p1: P1, p2: P2): F[P1, P2]
}
