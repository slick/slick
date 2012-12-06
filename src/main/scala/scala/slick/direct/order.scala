package scala.slick.direct

/*
// DOES NOT WORK, because it fetches Ordering[Reversed[T]] for Ordering[T]
class ReverseOrder{
  type Reversed[T] = T
  def reversed[T](t:T) : Reversed[T] = t.asInstanceOf[Reversed[T]]
  
  //private def reversedOrdering[T](implicit ordering:Ordering[T]) : Ordering[Reversed[T]] = ordering.reverse
  implicit def reversedOrdering[T](implicit ordering:Ordering[T]) : Ordering[Reversed[T]] = ordering.reverse.asInstanceOf[Ordering[Reversed[T]]]
}
*/

class ReverseOrder{
  final case class Reversed[T](value:T)
  def reversed[T](t:T) : Reversed[T] = Reversed(t)  
  private def reversedOrdering[T](implicit ordering:Ordering[T]) : Ordering[Reversed[T]] = new Ordering[Reversed[T]]{
    def compare( a:Reversed[T], b:Reversed[T] ) = ordering.reverse.compare( a.value, b.value )
  }
  implicit val reversedIntOrdering : Ordering[Reversed[Int]] = reversedOrdering[Int]
  implicit val reversedStringOrdering : Ordering[Reversed[String]] = reversedOrdering[String]
}
object order extends ReverseOrder