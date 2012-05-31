package scala.slick.ast

import scala.slick.ql.Shape

trait WithOp extends Cloneable {
  self: NodeGenerator =>
  def mapOp(f: (Node, List[Int]) => Node, positions: List[Int] = Nil): this.type = {
    val tv = Node(this)
    val fv = f(tv, positions)
    if(fv eq tv) this
    else {
      val t = clone
      t._op = fv
      t.asInstanceOf[WithOp.this.type] // work around https://issues.scala-lang.org/browse/SI-5210
    }
  }

  private[WithOp] var _op: Node = _

  final def op: Node = _op

  override def clone(): this.type = super.clone.asInstanceOf[this.type]
}

object WithOp {
  def unapply(w: WithOp) = if(w.op == null) None else Some(w.op)

  def mapOp[T](x: T, f: (Node, List[Int]) => Node, positions: List[Int] = Nil): T = (x match {
    case w: WithOp => w.mapOp(f, positions)
    case p: Product => // Only works for tuples but they have no common superclass
      Shape.buildTuple(p.productIterator.zipWithIndex.map {
        case (x, pos) => mapOp(x, f, (pos + 1) :: positions) }.toIndexedSeq)
  }).asInstanceOf[T]

  def encodeRef[T](x: T, sym: Symbol): T = mapOp(x, { (n, positions) =>
    if(positions.isEmpty) Ref(sym)
    else positions.foldRight[Node](Ref(sym))((idx, node) => ProductElement(node, idx))
  })
}
