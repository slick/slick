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
  def nonesFirst  [T](t:Option[T]) : Option[T] = SLICK_ONLY
  def nonesLast   [T](t:Option[T]) = SLICK_ONLY
  def nonesFirst  [T](t:Reversed[Option[T]]) = SLICK_ONLY
  def nonesLast   [T](t:Reversed[Option[T]]) = SLICK_ONLY
}
object order extends NullAndReverseOrder