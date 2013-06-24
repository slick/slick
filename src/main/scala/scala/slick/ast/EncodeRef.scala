package scala.slick.ast

import scala.slick.util.TupleSupport

trait EncodeRef {
  def encodeRef(sym: Symbol, positions: List[Int] = Nil): Any
}

object EncodeRef {
  def apply[T](x: T, sym: Symbol, positions: List[Int] = Nil): T = {
    (x match {
      case e: EncodeRef => e.encodeRef(sym, positions)
      case p: Product => // Only works for tuples but they have no common superclass
        TupleSupport.buildTuple(p.productIterator.zipWithIndex.map {
          case (x, pos) => apply(x, sym, (pos + 1) :: positions) }.toIndexedSeq)
    }).asInstanceOf[T]
  }
}
