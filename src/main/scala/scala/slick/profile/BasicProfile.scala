package scala.slick.profile

import scala.slick.compiler.QueryCompiler
import scala.slick.backend.DatabaseComponent

/**
 * The basic functionality that has to be implemented by all drivers.
 */
trait BasicProfile {

  type Backend <: DatabaseComponent
  val backend: Backend
  val compiler: QueryCompiler
  final val capabilities: Set[Capability] = computeCapabilities

  protected def computeCapabilities: Set[Capability] = Set.empty
}
