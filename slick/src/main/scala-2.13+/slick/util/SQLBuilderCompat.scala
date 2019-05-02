package slick.util

trait SQLBuilderCompat {

  def +=(s: String): SQLBuilder

  def sep[T](sequence: scala.collection.immutable.Iterable[T], separator: String)(f: T => Unit): Unit = {
    var first = true
    for(x <- sequence) {
      if(first) first = false else this += separator
      f(x)
    }
  }
}