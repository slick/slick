package slick.basic

// contains enabled capabilities as implicits
// to remove capability override with non-implicit value and call deregister
trait Capabilities {
  case class Registration[+C <: Capability] protected(cap: C)
  private var registeredCapabilities: Set[Registration[Capability]] = Set()
  def capabilities: Set[Capability] = this.registeredCapabilities.map(_.cap)

  protected def register[C <: Capability](c: C): Registration[C] = {
    val r = Registration(c)
    registeredCapabilities += r
    r
  }
  protected def deregister[C <: Capability](c: C): Registration[C] = {
    val r = Registration(c)
    registeredCapabilities -= r
    r
  }
}
