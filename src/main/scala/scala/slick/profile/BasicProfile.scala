package scala.slick.profile

import scala.language.higherKinds
import scala.slick.compiler.QueryCompiler
import scala.slick.backend.DatabaseComponent
import scala.slick.ast.{Node, Type}
import slick.lifted.Query

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

  /* The type of a pre-compiled parameterized query */
  type ParameterizedQuery[P, R] <: (P => AppliedQuery[R])

  /* The type of a pre-compiled applied query */
  type AppliedQuery[R]

  def compileParameterizedQuery[P, R](q: Query[_, R]): ParameterizedQuery[P, R]
}

trait BasicDriver extends BasicProfile {
  val profile: BasicProfile = this

  /** The driver-specific representation of types */
  type TypeInfo
  def typeInfoFor(t: Type): TypeInfo
}

/** Standard implementations of parameterized queries */
trait StandardParameterizedQueries { driver: BasicDriver =>
  type ParameterizedQuery[P, R] = ParameterizedQueryDef[P, R]
  type AppliedQuery[R] = AppliedQueryDef[R]

  class ParameterizedQueryDef[P, R](val tree: Node) extends (P => AppliedQueryDef[R]) {
    def apply(param: P) = new AppliedQueryDef[R](tree, param)
  }

  class AppliedQueryDef[R](val tree: Node, val param: Any)
}
