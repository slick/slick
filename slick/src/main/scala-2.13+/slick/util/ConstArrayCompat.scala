package slick.util

trait ConstArrayCompat {
  def from[T](values: scala.collection.Iterable[T]): ConstArray[T] = {
    val a = new Array[Any](values.size)
    var i = 0
    values.foreach { v =>
      a(i) = v
      i += 1
    }
    new ConstArray[T](a)
  }
}