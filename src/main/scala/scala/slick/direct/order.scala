package scala.slick.direct
import common._

/*
// DOES NOT WORK, because it fetches Ordering[Reversed[T]] for Ordering[T]
class ReverseOrder{
  type Reversed[T] = T
  def reversed[T](t:T) : Reversed[T] = t.asInstanceOf[Reversed[T]]
  implicit def reversedOrdering[T](implicit ordering:Ordering[T]) : Ordering[Reversed[T]] = ordering.reverse.asInstanceOf[Ordering[Reversed[T]]]
}
*/

class ReverseOrder{
  final case class Reversed[T](value:T)
  def reversed[T](t:T) : Reversed[T] = Reversed(t)  
  implicit def reversedOrdering[T:Ordering] : Ordering[Reversed[T]] = new Ordering[Reversed[T]]{
    def compare( a:Reversed[T], b:Reversed[T] ) = implicitly[Ordering[T]].reverse.compare( a.value, b.value )
  }
}

/**
 * null ordering dummies for slick translation
 */
class NullAndReverseOrder extends ReverseOrder{
  final case class NonesFirst[T](t:T)
  final case class NonesLast[T](t:T)
  final case class NonesFirstReversed[T](t:T)
  final case class NonesLastReversed[T](t:T)
  def nonesFirst  [T](t:Option[T]) = NonesFirst(t)
  def nonesLast   [T](t:Option[T]) = NonesLast(t)
  def nonesFirst  [T](t:Reversed[Option[T]]) = NonesFirstReversed(t)
  def nonesLast   [T](t:Reversed[Option[T]]) = NonesLastReversed(t)
}
object order extends NullAndReverseOrder