package slick.basic

import scala.annotation.implicitNotFound

/** Describes a feature that can be supported by a profile. */
class Capability(val name: String) {
  override def toString = name
  override def hashCode(): Int = name.hashCode
  override def equals(obj: Any): Boolean = obj match {
    case c: Capability => c.name.equals(name)
    case _ => false
  }
}

object Capability {
  def apply(name: String) = new Capability(name)

  /** Indicates that the profile supports the capability. */
  @implicitNotFound("${P} does not support ${C}")
  class ForProfile[P <: BasicProfile, C <: Capability](val cap: C)
  implicit def ForProfile[P <: BasicProfile, C <: Capability](implicit cap: Capabilities#Registration[C]): ForProfile[P, C] = new ForProfile[P, C](cap.cap)

  /** Indicates that the currently imported profile supports the capability. */
  @implicitNotFound("Current profile does not support ${C}")
  class UnknownProfile[C <: Capability](val cap: C)
  implicit def UnknownProfile[C <: Capability](implicit cap: Capabilities#Registration[C]): UnknownProfile[C] = new UnknownProfile[C](cap.cap)
}
