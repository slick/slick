package scala.slick.util

import java.lang.ref.{ReferenceQueue, WeakReference}

final case class RefId[E <: AnyRef](e: E) {
  override def hashCode = System.identityHashCode(e)
  override def equals(o: Any) = o match {
    case RefId(e2) => e eq e2
    case _ => false
  }
  override def toString = "RefId("+e.toString+")@" + hashCode
  def apply() = e
}

final class WeakRefId[E](_e: E, queue: ReferenceQueue[_ >: E] = null) extends WeakReference[E](_e, queue) {
  override val hashCode = System.identityHashCode(_e)
  override def equals(o: Any) = {
    if(this eq o.asInstanceOf[AnyRef]) true
    else if(!o.isInstanceOf[WeakRefId[_]]) false
    else {
      val got = get().asInstanceOf[AnyRef]
      (got ne null) && (got eq o.asInstanceOf[WeakRefId[_ <: AnyRef]].get())
    }
  }
  override def toString = "WeakRefId("+String.valueOf(get())+")@" + hashCode
  def apply() = get()
}
