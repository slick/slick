package scala.slick.lifted

import scala.language.implicitConversions
import scala.annotation.implicitNotFound
import scala.slick.ast.Node
import scala.slick.profile.BasicProfile
import scala.slick.compiler.QueryCompiler

/** A possibly parameterized query that will be cached for repeated efficient
  * execution without having to recompile it every time. The compiled state
  * is computed on demand the first time a `Cached` value is executed. It is
  * always tied to a specific driver.
  *
  * `Cached` forms a limited monad which ensures that it can only contain
  * values that are `Compilable`. */
sealed trait Compiled[T] {
  /** The driver which is used for compiling the query. */
  def driver: BasicProfile

  /** Perform a transformation of the underlying value. The computed value must
    * be `Compilable`. The resulting `Compiled` instance will be recompiled when
    * needed. It does not benefit from this instance already containing the
    * compiled state. */
  def map[U, C <: Compiled[U]](f: T => U)(implicit ucompilable: Compilable[U, C]): C =
    ucompilable.compiled(f(extract), driver)

  /** Perform a transformation of the underlying value. The computed `Compiled`
    * value is returned unmodified. */
  def flatMap[U <: Compiled[_]](f: T => U): U =
    f(extract)

  /** Return the underlying query or query function. It can be safely
    * extracted for reuse without caching the compiled representation. */
  def extract: T
}

object Compiled {
  /** Create a new `Compiled` value for a raw value that is `Compilable`. */
  @inline def apply[V, C <: Compiled[V]](raw: V)(implicit compilable: Compilable[V, C], driver: BasicProfile): C =
    compilable.compiled(raw, driver)
}

trait CompilersMixin { this: Compiled[_] =>
  def compile(qc: QueryCompiler): Node
  lazy val compiledQuery = compile(driver.queryCompiler)
  lazy val compiledUpdate = compile(driver.updateCompiler)
  lazy val compiledDelete = compile(driver.deleteCompiler)
}

class CompiledFunction[F, PT, PU, R <: Rep[_], RU](val extract: F, val tuple: F => PT => R, val pshape: Shape[ColumnsShapeLevel, PU, PU, PT], val driver: BasicProfile) extends Compiled[F] with CompilersMixin {
  /** Create an applied `Compiled` value for this compiled function. All applied
    * values share their compilation state with the original compiled function. */
  def apply(p: PU) = new AppliedCompiledFunction[PU, R, RU](p, this, driver)

  def compile(qc: QueryCompiler): Node = {
    val params: PT = pshape.buildParams(_.asInstanceOf[PU])
    val result: R = tuple(extract).apply(params)
    qc.run(result.toNode).tree
  }

  def applied(param: PU): R = tuple(extract).apply(pshape.pack(param))
}

/** A compiled value that can be executed to obtain its result. */
trait RunnableCompiled[R, RU] extends Compiled[R] {
  def param: Any
  def compiledQuery: Node
  def compiledUpdate: Node
  def compiledDelete: Node
}

class AppliedCompiledFunction[PU, R <: Rep[_], RU](val param: PU, function: CompiledFunction[_, _, PU, R, RU], val driver: BasicProfile) extends RunnableCompiled[R, RU] {
  lazy val extract: R = function.applied(param)
  def compiledQuery = function.compiledQuery
  def compiledUpdate = function.compiledUpdate
  def compiledDelete = function.compiledDelete
}

class CompiledExecutable[R <: Rep[_], RU](val extract: R, val driver: BasicProfile) extends RunnableCompiled[R, RU] with CompilersMixin {
  def param = ()
  def compile(qc: QueryCompiler): Node = qc.run(extract.toNode).tree
}

/** Typeclass for types that can be executed as queries. This encompasses
  * collection-valued (`Query[_, _]`), scalar and record types. This is used
  * as a phantom type for computing the required types. The actual value is
  * always `null`. */
@implicitNotFound("Computation of type ${T} cannot be executed (with result type ${TU})")
trait Executable[T, TU]

object Executable {
  @inline implicit def queryIsExecutable[B, BU]: Executable[Query[B, BU], Seq[BU]] = null
  @inline implicit def scalarIsExecutable[A, AU](implicit shape: Shape[FlatShapeLevel, A, AU, A]): Executable[A, AU] = null
}

/** Typeclass for types that can be contained in a `Compiled` container. This
  * includes all `Executable` types as well as functions (of any arity) from
  * flat, fully packed parameter types to an `Executable` result type. */
@implicitNotFound("Computation of type ${T} cannot be compiled (as type ${C})")
trait Compilable[T, C <: Compiled[T]] {
  def compiled(raw: T, driver: BasicProfile): C
}

object Compilable extends CompilableFunctions {
  implicit def function1IsCompilable[A , B <: Rep[_], P, U](implicit ashape: Shape[ColumnsShapeLevel, A, P, A], pshape: Shape[ColumnsShapeLevel, P, P, _], bexe: Executable[B, U]): Compilable[A => B, CompiledFunction[A => B, A , P, B, U]] = new Compilable[A => B, CompiledFunction[A => B, A, P, B, U]] {
    def compiled(raw: A => B, driver: BasicProfile) =
      new CompiledFunction[A => B, A, P, B, U](raw, identity[A => B], pshape.asInstanceOf[Shape[ColumnsShapeLevel, P, P, A]], driver)
  }
}

trait CompilableLowPriority {
  implicit def executableIsCompilable[T <: Rep[_], U](implicit e: Executable[T, U]): Compilable[T, CompiledExecutable[T, U]] = new Compilable[T, CompiledExecutable[T, U]] {
    def compiled(raw: T, driver: BasicProfile) = new CompiledExecutable[T, U](raw, driver)
  }
}

final class Parameters[PU, PP](pshape: Shape[ColumnsShapeLevel, PU, PU, _]) {
  def flatMap[R <: Rep[_], RU](f: PP => R)(implicit rexe: Executable[R, RU], driver: BasicProfile): CompiledFunction[PP => R, PP, PU, R, RU] =
    new CompiledFunction[PP => R, PP, PU, R, RU](f, identity[PP => R], pshape.asInstanceOf[Shape[ColumnsShapeLevel, PU, PU, PP]], driver)

  @inline def withFilter(f: PP => Boolean): Parameters[PU, PP] = this
}

object Parameters {
  @inline def apply[U](implicit pshape: Shape[ColumnsShapeLevel, U, U, _]): Parameters[U, pshape.Packed] =
    new Parameters[U, pshape.Packed](pshape)
}
