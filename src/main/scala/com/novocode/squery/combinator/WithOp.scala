package com.novocode.squery.combinator

// Not generic to work around bug #1434

trait WithOp extends Cloneable {
  def withOp(op: Node): this.type = {
    val t = clone
    t._op = op
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

/*
trait WithOp[O >: Null] extends Cloneable {
  def withOp(op: O): this.type = {
    val t = clone
    t._op = op
    t
  }
  private[WithOp] var _op: O = _
  final def op: O = _op
  override def clone(): this.type = super.clone.asInstanceOf[this.type]
}

object WithOp {
  def unapply[O >: Null](w: WithOp[O]) = if(w.op == null) None else Some(w.op)
}
*/
