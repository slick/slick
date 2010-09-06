package org.scalaquery.util

trait WithOp extends Cloneable { self: Node =>
  def mapOp(f: Node => Node): this.type = {
    val t = clone
    t._op = f(this)
    t
  }
  private[WithOp] var _op: Node = _
  final def op: Node = _op
  //protected[WithOp] def op_=(c:Node):Unit = _op = c
  override def clone(): this.type = super.clone.asInstanceOf[this.type]
}

object WithOp {
  def unapply(w: WithOp) = if(w.op == null) None else Some(w.op)
}
