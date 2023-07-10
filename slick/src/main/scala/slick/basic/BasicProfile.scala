package slick.basic

import scala.language.{implicitConversions, existentials}

import slick.ast._
import slick.compiler.QueryCompiler
import slick.dbio._
import slick.lifted._
import slick.util.GlobalConfig

import com.typesafe.config.Config

/** The basic functionality that has to be implemented by all profiles. */
trait BasicProfile extends BasicActionComponent { self: BasicProfile =>

  /** The back-end type required by this profile */
  type Backend <: BasicBackend
  /** The back-end implementation for this profile */
  val backend: Backend

  /** The capabilities supported by this profile. This can be used to query at
    * runtime whether a specific feature is supported. */
  final val capabilities: Set[Capability] = computeCapabilities
  /** Compute the capabilities. This should be overridden in subclasses as needed. */
  protected def computeCapabilities: Set[Capability] = Set.empty

  /** The type of a schema description (DDL) */
  type SchemaDescription <: SchemaDescriptionDef

  /** A schema description contains the SQL statements for creating and
    * dropping database entities. Schema descriptions can be combined for
    * creating or dropping multiple entities together, even if they have
    * circular dependencies. */
  trait SchemaDescriptionDef {
    def ++(other: SchemaDescription): SchemaDescription
  }

  trait BasicAPI extends Aliases with ExtensionMethodConversions {
    type Database = backend.Database
    val Database = backend.Database
    type Session = backend.Session
    type SlickException = slick.SlickException

    implicit val slickProfile: self.type = self

    implicit final def anyToShapedValue[T, U](value: T)(implicit shape: Shape[_ <: FlatShapeLevel, T, U, _]): ShapedValue[T, U] =
      new ShapedValue[T, U](value, shape)

    implicit def streamableQueryActionExtensionMethods[U, C[_]](q: Query[_,U, C]): StreamingQueryActionExtensionMethods[C[U], U] =
      createStreamingQueryActionExtensionMethods[C[U], U](queryCompiler.run(q.toNode).tree, ())
    implicit def runnableCompiledQueryActionExtensionMethods[RU](c: RunnableCompiled[_, RU]): QueryActionExtensionMethods[RU, NoStream] =
      createQueryActionExtensionMethods[RU, NoStream](c.compiledQuery, c.param)
    implicit def streamableCompiledQueryActionExtensionMethods[RU, EU](c: StreamableCompiled[_, RU, EU]): StreamingQueryActionExtensionMethods[RU, EU] =
      createStreamingQueryActionExtensionMethods[RU, EU](c.compiledQuery, c.param)
    // Applying a CompiledFunction always results in only a RunnableCompiled, not a StreamableCompiled, so we need this:
    implicit def streamableAppliedCompiledFunctionActionExtensionMethods[R, RU, EU, C[_]](c: AppliedCompiledFunction[_, Query[R, EU, C], RU]): StreamingQueryActionExtensionMethods[RU, EU] =
      createStreamingQueryActionExtensionMethods[RU, EU](c.compiledQuery, c.param)
    implicit def recordQueryActionExtensionMethods[M, R](q: M)(implicit shape: Shape[_ <: FlatShapeLevel, M, R, _]): QueryActionExtensionMethods[R, NoStream] =
      createQueryActionExtensionMethods[R, NoStream](queryCompiler.run(shape.toNode(q)).tree, ())
  }

  /** The API for using the query language with a single import
    * statement. This provides the profile's implicits, the Database API
    * and commonly used query language types and objects. */
  val api: BasicAPI

  /** The compiler used for queries */
  def queryCompiler: QueryCompiler

  /** The compiler used for updates */
  def updateCompiler: QueryCompiler

  /** The compiler used for deleting data */
  def deleteCompiler: QueryCompiler

  /** The compiler used for inserting data */
  def insertCompiler: QueryCompiler

  /** The type of a (partially) compiled AST for Insert operations. Unlike
    * querying or deleting, inserts may require different compilation results
    * which should be computed lazily. */
  type CompiledInsert

  /** (Partially) compile an AST for insert operations */
  def compileInsert(n: Node): CompiledInsert

  /* internal: */

  /** The configuration for this profile, loaded via [[loadProfileConfig]]. */
  protected[this] final lazy val profileConfig: Config = loadProfileConfig

  /** Load the configuration for this profile. This can be overridden in user-defined
    * subclasses to load different configurations.
    *
    * The default implementation does a breadth-first search in the supertype hierarchy of the
    * runtime class until it finds a class or trait with a name matching "slick.[...]Profile"
    * and then returns uses this name as a path in the application config. If no configuration
    * exists at this path, an empty Config object is returned. */
  protected[this] def loadProfileConfig: Config = {
    def findConfigName(classes: Vector[Class[_]]): Option[String] =
      classes.iterator.map { cl =>
        val n = cl.getName
        if(n.startsWith("slick.") && n.endsWith("Profile")) Some(n) else None
      }.find(_.isDefined).getOrElse {
        val parents = classes.flatMap { cl => Option(cl.getSuperclass) ++: cl.getInterfaces.toVector }
        if(parents.isEmpty) None else findConfigName(parents)
      }
    GlobalConfig.profileConfig(findConfigName(Vector(getClass)).get)
  }

  override def toString = {
    val n = getClass.getName
    if(n.startsWith("slick.") && n.endsWith("Profile$")) n
    else super.toString
  }
}

trait BasicActionComponent { self: BasicProfile =>

  type ProfileAction[+R, +S <: NoStream, -E <: Effect] <: BasicAction[R, S, E]
  type StreamingProfileAction[+R, +T, -E <: Effect] <: BasicStreamingAction[R, T, E] with ProfileAction[R, Streaming[T], E]

  //////////////////////////////////////////////////////////// Query Actions

  type QueryActionExtensionMethods[R, S <: NoStream] <: BasicQueryActionExtensionMethodsImpl[R, S]
  type StreamingQueryActionExtensionMethods[R, T] <: BasicStreamingQueryActionExtensionMethodsImpl[R, T]

  def createQueryActionExtensionMethods[R, S <: NoStream](tree: Node, param: Any): QueryActionExtensionMethods[R, S]
  def createStreamingQueryActionExtensionMethods[R, T](tree: Node, param: Any): StreamingQueryActionExtensionMethods[R, T]

  trait BasicQueryActionExtensionMethodsImpl[R, S <: NoStream] {
    /** An Action that runs this query. */
    def result: ProfileAction[R, S, Effect.Read]
  }

  trait BasicStreamingQueryActionExtensionMethodsImpl[R, T]
    extends BasicQueryActionExtensionMethodsImpl[R, Streaming[T]] {
    def result: StreamingProfileAction[R, T, Effect.Read]
  }
}

trait BasicAction[+R, +S <: NoStream, -E <: Effect] extends DatabaseAction[R, S, E] {
  type ResultAction[+R, +S <: NoStream, -E <: Effect] <: BasicAction[R, S, E]
}

trait BasicStreamingAction[+R, +T, -E <: Effect] extends BasicAction[R, Streaming[T], E] {
  /** Create an Action that returns only the first value of this stream of data. The Action will
    * fail if the stream is empty. Only available on streaming Actions. */
  def head: ResultAction[T, NoStream, E]

  /** Create an Action that returns only the first value of this stream of data as an `Option`.
    * Only available on streaming Actions. */
  def headOption: ResultAction[Option[T], NoStream, E]
}

trait FixedBasicAction[+R, +S <: NoStream, -E <: Effect] extends BasicAction[R, S, E] {
  type ResultAction[+R, +S <: NoStream, -E <: Effect] = BasicAction[R, S, E]
}

trait FixedBasicStreamingAction[+R, +T, -E <: Effect] extends BasicStreamingAction[R, T, E] with FixedBasicAction[R, Streaming[T], E]
