package scala.slick.profile

import scala.slick.compiler.QueryCompiler
import scala.slick.backend.DatabaseComponent
import scala.slick.ast.Type

/**
 * The basic functionality that has to be implemented by all drivers.
 */
trait BasicProfile { driver: BasicDriver =>

  /** The back-end type required by this profile */
  type Backend <: DatabaseComponent
  val backend: Backend

  val compiler: QueryCompiler

  final val capabilities: Set[Capability] = computeCapabilities
  protected def computeCapabilities: Set[Capability] = Set.empty
}

trait BasicDriver extends BasicProfile {
  val profile: BasicProfile = this

  /** The driver-specific representation of types */
  type TypeInfo
  def typeInfoFor(t: Type): TypeInfo
}
