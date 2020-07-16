package slick.lifted

import scala.annotation.implicitNotFound

import slick.ast.Node
import slick.basic.BasicProfile

/** A possibly parameterized query that will be cached for repeated efficient
  * execution without having to recompile it every time. The compiled state
  * is computed on demand the first time a `Cached` value is executed. It is
  * always tied to a specific profile.
  *
  * `Cached` forms a limited monad which ensures that it can only contain
  * values that are `Compilable`. */
sealed trait Compiled[T] {
  /** The profile which is used for compiling the query. */
  def profile: BasicProfile

  /** Perform a transformation of the underlying value. The computed value must
    * be `Compilable`. The resulting `Compiled` instance will be recompiled when
    * needed. It does not benefit from this instance already containing the
    * compiled state. */
  def map[U, C <: Compiled[U]](f: T => U)(implicit uCompilable: Compilable[U, C]): C =
    uCompilable.compiled(f(extract), profile)

  /** Perform a transformation of the underlying value. The computed `Compiled`
    * value is returned unmodified. */
  def flatMap[U <: Compiled[?]](f: T => U): U =
    f(extract)

  /** Return the underlying query or query function. It can be safely
    * extracted for reuse without caching the compiled representation. */
  def extract: T
}

object Compiled {
  /** Create a new `Compiled` value for a raw value that is `Compilable`. */
  @inline def apply[V, C <: Compiled[V]](raw: V)(implicit compilable: Compilable[V, C], profile: BasicProfile): C =
    compilable.compiled(raw, profile)
}

trait CompilersMixin { this: Compiled[?] =>
  def toNode: Node
  lazy val compiledQuery = profile.queryCompiler.run(toNode).tree
  lazy val compiledUpdate = profile.updateCompiler.run(toNode).tree
  lazy val compiledDelete = profile.deleteCompiler.run(toNode).tree
  lazy val compiledInsert = profile.compileInsert(toNode)
}

class CompiledFunction[F, PT, PU, R <: Rep[?], RU](val extract: F,
                                                   val tuple: F => PT => R,
                                                   val pShape: Shape[ColumnsShapeLevel, PU, PU, PT],
                                                   val profile: BasicProfile) extends Compiled[F] with CompilersMixin {
  /** Create an applied `Compiled` value for this compiled function. All applied
    * values share their compilation state with the original compiled function. */
  def apply(p: PU) = new AppliedCompiledFunction[PU, R, RU](p, this, profile)

  def toNode: Node = {
    val params: PT = pShape.buildParams(_.asInstanceOf[PU])
    val result: R = tuple(extract).apply(params)
    result.toNode
  }

  def applied(param: PU): R = tuple(extract).apply(pShape.pack(param))
}

/** A compiled value that can be executed to obtain its result. */
trait RunnableCompiled[R, RU] extends Compiled[R] {
  def param: Any
  def compiledQuery: Node
  def compiledUpdate: Node
  def compiledDelete: Node
  def compiledInsert: Any // Actually of the profile's CompiledInsert type
}

/** A compiled value that can be executed to obtain its result as a stream of data. */
trait StreamableCompiled[R, RU, EU] extends RunnableCompiled[R, RU]

class AppliedCompiledFunction[PU, R <: Rep[?], RU](val param: PU,
                                                   function: CompiledFunction[?, ?, PU, R, RU],
                                                   val profile: BasicProfile) extends RunnableCompiled[R, RU] {
  lazy val extract: R = function.applied(param)
  def compiledQuery = function.compiledQuery
  def compiledUpdate = function.compiledUpdate
  def compiledDelete = function.compiledDelete
  def compiledInsert = function.compiledInsert
}

abstract class CompiledExecutable[R, RU](val extract: R, val profile: BasicProfile)
  extends RunnableCompiled[R, RU]
    with CompilersMixin {
  override def param: Unit = ()
  def toNode: Node
}

abstract class CompiledStreamingExecutable[R, RU, EU](extract: R, profile: BasicProfile)
  extends CompiledExecutable[R, RU](extract, profile)
    with StreamableCompiled[R, RU, EU]

/** Typeclass for types that can be executed as queries. This encompasses
  * collection-valued (`Query[_, _, _[_] ]`), scalar and record types. */
@implicitNotFound("Computation of type ${T} cannot be executed (with result type ${TU})")
trait Executable[T, TU] {
  def toNode(value: T): Node
}

object Executable {
  @inline implicit def queryIsExecutable[B, BU, C[_]]: StreamingExecutable[Query[B, BU, C], C[BU], BU] =
    StreamingExecutable[Query[B, BU, C], C[BU], BU]
  @inline implicit def tableQueryIsExecutable[B <: AbstractTable[?], BU, C[_]]: StreamingExecutable[
    Query[B, BU, C] & TableQuery[B],
    C[BU],
    BU
  ] =
    StreamingExecutable[Query[B, BU, C] & TableQuery[B], C[BU], BU]
  @inline implicit def baseJoinQueryIsExecutable[B1, B2, BU1, BU2, C[_], Ba1, Ba2]: StreamingExecutable[
    BaseJoinQuery[B1, B2, BU1, BU2, C, Ba1, Ba2],
    C[(BU1, BU2)],
    (BU1, BU2)
  ] =
    StreamingExecutable[BaseJoinQuery[B1, B2, BU1, BU2, C, Ba1, Ba2], C[(BU1, BU2)], (BU1, BU2)]
  @inline implicit def scalarIsExecutable[R, U](implicit shape: Shape[? <: FlatShapeLevel, R, U, ?]): Executable[R, U] =
    (value: R) => shape.toNode(value)
}

/** Typeclass for types that can be executed as streaming queries, i.e. only
  * collection-valued (`Query[_, _, _[_] ]`) types. This is used
  * as a phantom type for computing the required types. The actual value is
  * always `null`. */
@implicitNotFound(
  "Computation of type ${T} cannot be executed (with sequence result type ${TU} and base result type ${EU})"
)
trait StreamingExecutable[T, TU, EU] extends Executable[T, TU]

/** A prototype `StreamingExecutable` instance for `Rep` types. */
object StreamingExecutable extends StreamingExecutable[Rep[Any], Any, Any] {
  def apply[T <: Rep[?], TU, EU] = this.asInstanceOf[StreamingExecutable[T, TU, EU]]
  def toNode(value: Rep[Any]) = value.toNode
}

/** Typeclass for types that can be contained in a `Compiled` container. This
  * includes all `Executable` types as well as functions (of any arity) from
  * flat, fully packed parameter types to an `Executable` result type. */
@implicitNotFound("Computation of type ${T} cannot be compiled (as type ${C})")
trait Compilable[T, C <: Compiled[T]] {
  def compiled(raw: T, profile: BasicProfile): C
}

object Compilable extends CompilableFunctions {
  implicit def function1IsCompilable[A , B <: Rep[?], P, U](implicit aShape: Shape[ColumnsShapeLevel, A, P, A],
                                                            pShape: Shape[ColumnsShapeLevel, P, P, ?],
                                                            bExe: Executable[B, U]): Compilable[
    A => B,
    CompiledFunction[A => B, A , P, B, U]
  ] =
    (raw: A => B, profile: BasicProfile) =>
      new CompiledFunction[A => B, A, P, B, U](
        raw,
        identity[A => B],
        pShape.asInstanceOf[Shape[ColumnsShapeLevel, P, P, A]],
        profile
      )
  implicit def streamingExecutableIsCompilable[T, U, EU](implicit e: StreamingExecutable[T, U, EU]
                                                        ): Compilable[T, CompiledStreamingExecutable[T, U, EU]] =
    (raw: T, profile: BasicProfile) =>
      new CompiledStreamingExecutable[T, U, EU](raw, profile) {
        def toNode = e.toNode(raw)
      }
}

trait CompilableLowPriority {
  implicit def executableIsCompilable[T, U](implicit e: Executable[T, U]): Compilable[T, CompiledExecutable[T, U]] =
    (raw: T, profile: BasicProfile) =>
      new CompiledExecutable[T, U](raw, profile) {
        def toNode = e.toNode(raw)
      }
}

final class Parameters[PU, PP](pShape: Shape[ColumnsShapeLevel, PU, PU, ?]) {
  def flatMap[R <: Rep[?], RU](f: PP => R)(implicit rExe: Executable[R, RU],
                                           profile: BasicProfile): CompiledFunction[PP => R, PP, PU, R, RU] =
    new CompiledFunction[PP => R, PP, PU, R, RU](
      f,
      identity[PP => R],
      pShape.asInstanceOf[Shape[ColumnsShapeLevel, PU, PU, PP]],
      profile
    )

  @inline def withFilter(f: PP => Boolean): Parameters[PU, PP] = this
}

object Parameters {
  @inline def apply[U](implicit pShape: Shape[ColumnsShapeLevel, U, U, ?]): Parameters[U, pShape.Packed] =
    new Parameters[U, pShape.Packed](pShape)
}
