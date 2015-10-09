package slick.basic

/** Describes a feature that can be supported by a profile. */
class Capability(name: String) {
  override def toString = name
}

object Capability {
  def apply(name: String) = new Capability(name)
}
