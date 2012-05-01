package scala.slick.ast

import scala.slick.ql.Shape

trait WithOp extends Cloneable {
  self: NodeGenerator =>
  def mapOp(f: Node => Node): this.type = {
    val tv = Node(this)
    val fv = f(tv)
    if (fv eq tv) this
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

  def mapOp[T](x: T, f: Node => Node): T = x match {
    case w: WithOp => w.mapOp(f).asInstanceOf[T]
    case p: Product => // Only works for tuples but they have no common superclass
      var changed = false
      val seq = p.productIterator.map { x =>
        val y = mapOp(x, f)
        if(x.asInstanceOf[AnyRef].ne(y.asInstanceOf[AnyRef])) changed = true
        y
      }.toIndexedSeq
      if(changed) Shape.buildTuple(seq).asInstanceOf[T]
      else x
  }
}
