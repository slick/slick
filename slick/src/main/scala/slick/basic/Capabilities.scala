package slick.basic

import slick.jdbc.JdbcCapabilities
import slick.relational.{RelationalCapabilities}

// contains enabled capabilities as implicits
// to remove capability override with non-implicit calling deregister
// todo perhaps use a macros to collect all the implicits instead of mutable state
trait Capabilities {
  case class Registered[+C <: Capability] protected(c: C)
  private var registeredCapabilities: Set[Registered[Capability]] = Set()
  def capabilities = this.registeredCapabilities.map(_.c)
  protected def register[C <: Capability](c: C): Registered[C] = {
    val r = Registered(c)
    registeredCapabilities += r
    r
  }
  protected def deregister[C <: Capability](c: C) = {
    val r = Registered(c)
    registeredCapabilities -= r
    r
  }
  // todo these will go to subtypes for each kind of Capabilities, replacing *Capabilities.all methods
  implicit val insertOrUpdate: Registered[JdbcCapabilities.insertOrUpdate.type] = register(JdbcCapabilities.insertOrUpdate)
  implicit val repeat: Registered[RelationalCapabilities.repeat.type] = register(RelationalCapabilities.repeat)
  implicit val replace: Registered[RelationalCapabilities.replace.type] = register(RelationalCapabilities.replace)
  implicit val zip: Registered[RelationalCapabilities.zip.type] = register(RelationalCapabilities.zip)
}
object CapabilitiesExample {
  def main(args: Array[String]): Unit = {
    // remove capability
    class C1 extends Capabilities {
      override val insertOrUpdate = deregister(JdbcCapabilities.insertOrUpdate)
    }
    // add back capability
    class C2 extends C1 {
      override implicit val insertOrUpdate = register(JdbcCapabilities.insertOrUpdate)
    }
    println(new C1().capabilities)
    println(new C2().capabilities)
  }
}