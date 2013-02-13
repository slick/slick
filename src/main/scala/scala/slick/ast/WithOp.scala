package scala.slick.ast

import scala.slick.util.TupleSupport

trait WithOp extends Cloneable {
  self: NodeGenerator =>
  def mapOp(f: (Node, List[Int]) => Node, positions: List[Int] = Nil): this.type = {
    val tv = Node(this)
    val fv = f(tv, positions)
    if(fv eq tv) this
    else {
      val t = clone
      t._op = fv
      t
    }
  }

  private[WithOp] var _op: Node = _

  final def op: Node = _op

  override def clone(): this.type = super.clone.asInstanceOf[this.type]
}

object WithOp {
  def unapply(w: WithOp) = if(w.op == null) None else Some(w.op)

  def encodeRef[T](x: T, sym: Symbol, positions: List[Int] = Nil): T = {
    def f(n: Node, positions: List[Int]): Node = {
      positions.foldRight[Node](Ref(sym))((idx, node) => Select(node, ElementSymbol(idx)))
    }
    (x match {
      case e: EncodeRef => e.encodeRef(sym, positions)
      case w: WithOp => w.mapOp(f, positions)
      case p: Product => // Only works for tuples but they have no common superclass
        TupleSupport.buildTuple(p.productIterator.zipWithIndex.map {
          case (x, pos) => encodeRef(x, sym, (pos + 1) :: positions) }.toIndexedSeq)
    }).asInstanceOf[T]
  }
}

trait EncodeRef {
  def encodeRef(sym: Symbol, positions: List[Int] = Nil): Any
}
