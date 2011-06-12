package org.scalaquery.util

import org.scalaquery.SQueryException

trait WithOp extends Cloneable { self: Node =>
  def mapOp(f: Node => Node): this.type = {
    val t = clone
    t._op = f(this)
    t
  }
  private[WithOp] var _op: Node = _
  final def op: Node = _op
  override def clone(): this.type = super.clone.asInstanceOf[this.type]
}

object WithOp {
  def unapply(w: WithOp) = if(w.op == null) None else Some(w.op)

  def mapOp[T](x: T, f: Node => Node): T = x match {
    case w: WithOp => w.mapOp(f).asInstanceOf[T]
    case (x1, x2) => (mapOp(x1, f), mapOp(x2, f)).asInstanceOf[T]
    case (x1, x2, x3) => (mapOp(x1, f), mapOp(x2, f), mapOp(x3, f)).asInstanceOf[T]
    case (x1, x2, x3, x4) => (mapOp(x1, f), mapOp(x2, f), mapOp(x3, f), mapOp(x4, f)).asInstanceOf[T]
    case (x1, x2, x3, x4, x5) => (mapOp(x1, f), mapOp(x2, f), mapOp(x3, f), mapOp(x4, f), mapOp(x5, f)).asInstanceOf[T]
    case (x1, x2, x3, x4, x5, x6) => (mapOp(x1, f), mapOp(x2, f), mapOp(x3, f), mapOp(x4, f), mapOp(x5, f), mapOp(x6, f)).asInstanceOf[T]
    case (x1, x2, x3, x4, x5, x6, x7) => (mapOp(x1, f), mapOp(x2, f), mapOp(x3, f), mapOp(x4, f), mapOp(x5, f), mapOp(x6, f), mapOp(x7, f)).asInstanceOf[T]
    case (x1, x2, x3, x4, x5, x6, x7, x8) => (mapOp(x1, f), mapOp(x2, f), mapOp(x3, f), mapOp(x4, f), mapOp(x5, f), mapOp(x6, f), mapOp(x7, f), mapOp(x8, f)).asInstanceOf[T]
    case (x1, x2, x3, x4, x5, x6, x7, x8, x9) => (mapOp(x1, f), mapOp(x2, f), mapOp(x3, f), mapOp(x4, f), mapOp(x5, f), mapOp(x6, f), mapOp(x7, f), mapOp(x8, f), mapOp(x9, f)).asInstanceOf[T]
    case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) => (mapOp(x1, f), mapOp(x2, f), mapOp(x3, f), mapOp(x4, f), mapOp(x5, f), mapOp(x6, f), mapOp(x7, f), mapOp(x8, f), mapOp(x9, f), mapOp(x10, f)).asInstanceOf[T]
    case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) => (mapOp(x1, f), mapOp(x2, f), mapOp(x3, f), mapOp(x4, f), mapOp(x5, f), mapOp(x6, f), mapOp(x7, f), mapOp(x8, f), mapOp(x9, f), mapOp(x10, f), mapOp(x11, f)).asInstanceOf[T]
    case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) => (mapOp(x1, f), mapOp(x2, f), mapOp(x3, f), mapOp(x4, f), mapOp(x5, f), mapOp(x6, f), mapOp(x7, f), mapOp(x8, f), mapOp(x9, f), mapOp(x10, f), mapOp(x11, f), mapOp(x12, f)).asInstanceOf[T]
    case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) => (mapOp(x1, f), mapOp(x2, f), mapOp(x3, f), mapOp(x4, f), mapOp(x5, f), mapOp(x6, f), mapOp(x7, f), mapOp(x8, f), mapOp(x9, f), mapOp(x10, f), mapOp(x11, f), mapOp(x12, f), mapOp(x13, f)).asInstanceOf[T]
    case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) => (mapOp(x1, f), mapOp(x2, f), mapOp(x3, f), mapOp(x4, f), mapOp(x5, f), mapOp(x6, f), mapOp(x7, f), mapOp(x8, f), mapOp(x9, f), mapOp(x10, f), mapOp(x11, f), mapOp(x12, f), mapOp(x13, f), mapOp(x14, f)).asInstanceOf[T]
    case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) => (mapOp(x1, f), mapOp(x2, f), mapOp(x3, f), mapOp(x4, f), mapOp(x5, f), mapOp(x6, f), mapOp(x7, f), mapOp(x8, f), mapOp(x9, f), mapOp(x10, f), mapOp(x11, f), mapOp(x12, f), mapOp(x13, f), mapOp(x14, f), mapOp(x15, f)).asInstanceOf[T]
    case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16) => (mapOp(x1, f), mapOp(x2, f), mapOp(x3, f), mapOp(x4, f), mapOp(x5, f), mapOp(x6, f), mapOp(x7, f), mapOp(x8, f), mapOp(x9, f), mapOp(x10, f), mapOp(x11, f), mapOp(x12, f), mapOp(x13, f), mapOp(x14, f), mapOp(x15, f), mapOp(x16, f)).asInstanceOf[T]
    case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17) => (mapOp(x1, f), mapOp(x2, f), mapOp(x3, f), mapOp(x4, f), mapOp(x5, f), mapOp(x6, f), mapOp(x7, f), mapOp(x8, f), mapOp(x9, f), mapOp(x10, f), mapOp(x11, f), mapOp(x12, f), mapOp(x13, f), mapOp(x14, f), mapOp(x15, f), mapOp(x16, f), mapOp(x17, f)).asInstanceOf[T]
    case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18) => (mapOp(x1, f), mapOp(x2, f), mapOp(x3, f), mapOp(x4, f), mapOp(x5, f), mapOp(x6, f), mapOp(x7, f), mapOp(x8, f), mapOp(x9, f), mapOp(x10, f), mapOp(x11, f), mapOp(x12, f), mapOp(x13, f), mapOp(x14, f), mapOp(x15, f), mapOp(x16, f), mapOp(x17, f), mapOp(x18, f)).asInstanceOf[T]
    case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19) => (mapOp(x1, f), mapOp(x2, f), mapOp(x3, f), mapOp(x4, f), mapOp(x5, f), mapOp(x6, f), mapOp(x7, f), mapOp(x8, f), mapOp(x9, f), mapOp(x10, f), mapOp(x11, f), mapOp(x12, f), mapOp(x13, f), mapOp(x14, f), mapOp(x15, f), mapOp(x16, f), mapOp(x17, f), mapOp(x18, f), mapOp(x19, f)).asInstanceOf[T]
    case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20) => (mapOp(x1, f), mapOp(x2, f), mapOp(x3, f), mapOp(x4, f), mapOp(x5, f), mapOp(x6, f), mapOp(x7, f), mapOp(x8, f), mapOp(x9, f), mapOp(x10, f), mapOp(x11, f), mapOp(x12, f), mapOp(x13, f), mapOp(x14, f), mapOp(x15, f), mapOp(x16, f), mapOp(x17, f), mapOp(x18, f), mapOp(x19, f), mapOp(x20, f)).asInstanceOf[T]
    case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21) => (mapOp(x1, f), mapOp(x2, f), mapOp(x3, f), mapOp(x4, f), mapOp(x5, f), mapOp(x6, f), mapOp(x7, f), mapOp(x8, f), mapOp(x9, f), mapOp(x10, f), mapOp(x11, f), mapOp(x12, f), mapOp(x13, f), mapOp(x14, f), mapOp(x15, f), mapOp(x16, f), mapOp(x17, f), mapOp(x18, f), mapOp(x19, f), mapOp(x20, f), mapOp(x21, f)).asInstanceOf[T]
    case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22) => (mapOp(x1, f), mapOp(x2, f), mapOp(x3, f), mapOp(x4, f), mapOp(x5, f), mapOp(x6, f), mapOp(x7, f), mapOp(x8, f), mapOp(x9, f), mapOp(x10, f), mapOp(x11, f), mapOp(x12, f), mapOp(x13, f), mapOp(x14, f), mapOp(x15, f), mapOp(x16, f), mapOp(x17, f), mapOp(x18, f), mapOp(x19, f), mapOp(x20, f), mapOp(x21, f), mapOp(x22, f)).asInstanceOf[T]
    case _ => throw new SQueryException("Cannot apply an operation to "+x)
  }
}
