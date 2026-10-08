package slick.basic

import scala.annotation.implicitNotFound

/** Describes a feature that can be supported by a profile. */
class Capability(name: String) {
  override def toString = name
}

object Capability {
  def apply(name: String) = new Capability(name)

  // this one is used if current profile is in scope
  @implicitNotFound("${P} does not support ${C}")
  class ForProfile[P <: BasicProfile, C <: Capability](val cap: C)
  implicit def ForProfile[P <: BasicProfile, C <: Capability](implicit cap: Capabilities#Registered[C]): ForProfile[P, C] = new ForProfile[P, C](cap.c)

  // this one has a less useful error message
  @implicitNotFound("Current profile does not support ${C}")
  class UnknownProfile[C <: Capability](val cap: C)
  implicit def UnknownProfile[C <: Capability](implicit cap: Capabilities#Registered[C]): UnknownProfile[C] = new UnknownProfile[C](cap.c)
  // todo perhaps it's not worth it to have both, ie either abandon mentioning exact profile in error, or add current profile to Query etc
}
