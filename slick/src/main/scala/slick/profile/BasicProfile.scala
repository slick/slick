package slick.profile

import scala.language.{higherKinds, implicitConversions, existentials}
import slick.SlickException
import slick.compiler.QueryCompiler
import slick.backend.DatabaseComponent
import slick.dbio._
import slick.ast._
import slick.lifted._
import com.typesafe.config.{ConfigFactory, Config}
import slick.util.GlobalConfig

/**
 * The basic functionality that has to be implemented by all drivers.
 */
trait BasicProfile extends BasicActionComponent { driver: BasicDriver =>

  /** The back-end type required by this profile */
  type Backend <: DatabaseComponent
  /** The back-end implementation for this profile */
  val backend: Backend

  /** The capabilities supported by this driver. This can be used to query at
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

  trait API extends Aliases with ExtensionMethodConversions {
    type Database = Backend#Database
    val Database = backend.Database
    type Session = Backend#Session
    type SlickException = slick.SlickException

    implicit val slickDriver: driver.type = driver

    // Work-around for SI-3346
    @inline implicit final def anyToToShapedValue[T](value: T) = new ToShapedValue[T](value)

    implicit def repQueryActionExtensionMethods[U](rep: Rep[U]): QueryActionExtensionMethods[U, NoStream] =
      createQueryActionExtensionMethods[U, NoStream](queryCompiler.run(rep.toNode).tree, ())
    implicit def streamableQueryActionExtensionMethods[U, C[_]](q: Query[_,U, C]): StreamingQueryActionExtensionMethods[C[U], U] =
      createStreamingQueryActionExtensionMethods[C[U], U](queryCompiler.run(q.toNode).tree, ())
    implicit def runnableCompiledQueryActionExtensionMethods[RU](c: RunnableCompiled[_, RU]): QueryActionExtensionMethods[RU, NoStream] =
      createQueryActionExtensionMethods[RU, NoStream](c.compiledQuery, c.param)
    implicit def streamableCompiledQueryActionExtensionMethods[RU, EU](c: StreamableCompiled[_, RU, EU]): StreamingQueryActionExtensionMethods[RU, EU] =
      createStreamingQueryActionExtensionMethods[RU, EU](c.compiledQuery, c.param)
    // Applying a CompiledFunction always results in only a RunnableCompiled, not a StreamableCompiled, so we need this:
    implicit def streamableAppliedCompiledFunctionActionExtensionMethods[R, RU, EU, C[_]](c: AppliedCompiledFunction[_, Query[R, EU, C], RU]): StreamingQueryActionExtensionMethods[RU, EU] =
      createStreamingQueryActionExtensionMethods[RU, EU](c.compiledQuery, c.param)
    // This only works on Scala 2.11 due to SI-3346:
    implicit def recordQueryActionExtensionMethods[M, R](q: M)(implicit shape: Shape[_ <: FlatShapeLevel, M, R, _]): QueryActionExtensionMethods[R, NoStream] =
      createQueryActionExtensionMethods[R, NoStream](queryCompiler.run(shape.toNode(q)).tree, ())
  }

  /** The API for using the query language with a single import
    * statement. This provides the driver's implicits, the Database and
    * Session objects for DB connections, and commonly used query language
    * types and objects. */
  val api: API

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

  /** (Partially) ompile an AST for insert operations */
  def compileInsert(n: Node): CompiledInsert
}

trait BasicDriver extends BasicProfile {
  /** The external interface of this driver which defines the API. */
  val profile: BasicProfile = this

  override def toString = {
    val cl = getClass.getName
    if(cl.startsWith("slick.driver.") && cl.endsWith("$"))
      cl.substring(13, cl.length-1)
    else super.toString
  }

  /** The configuration for this driver, loaded via [[loadDriverConfig]]. */
  final lazy val driverConfig: Config = loadDriverConfig

  /** Load the configuration for this driver. This can be overridden in user-defined driver
    * subclasses to load different configurations.
    *
    * The default implementation does a breadth-first search in the supertype hierarchy of the
    * runtime class until it finds a class or trait matching "slick.driver.XXXDriver"
    * where XXX is an arbitrary name, and then returns the path "slick.driver.XXX" from the
    * application config, if it exists, otherwise an empty Config object. */
  protected[this] def loadDriverConfig: Config = {
    def findConfigName(classes: Vector[Class[_]]): Option[String] =
      classes.iterator.map { cl =>
        val n = cl.getName
        if(n.startsWith("slick.driver.") && n.endsWith("Driver")) Some(n.substring(13, n.length-6))
        else None
      }.find(_.isDefined).getOrElse {
        val parents = classes.flatMap { cl => Option(cl.getSuperclass) ++: cl.getInterfaces.toVector }
        if(parents.isEmpty) None else findConfigName(parents)
      }
    GlobalConfig.driverConfig(findConfigName(Vector(getClass)).get)
  }
}

trait BasicActionComponent { driver: BasicDriver =>

  type DriverAction[+R, +S <: NoStream, -E <: Effect] <: BasicAction[R, S, E]
  type StreamingDriverAction[+R, +T, -E <: Effect] <: BasicStreamingAction[R, T, E] with DriverAction[R, Streaming[T], E]

  //////////////////////////////////////////////////////////// Query Actions

  type QueryActionExtensionMethods[R, S <: NoStream] <: QueryActionExtensionMethodsImpl[R, S]
  type StreamingQueryActionExtensionMethods[R, T] <: StreamingQueryActionExtensionMethodsImpl[R, T]

  def createQueryActionExtensionMethods[R, S <: NoStream](tree: Node, param: Any): QueryActionExtensionMethods[R, S]
  def createStreamingQueryActionExtensionMethods[R, T](tree: Node, param: Any): StreamingQueryActionExtensionMethods[R, T]

  trait QueryActionExtensionMethodsImpl[R, S <: NoStream] {
    /** An Action that runs this query. */
    def result: DriverAction[R, S, Effect.Read]
  }

  trait StreamingQueryActionExtensionMethodsImpl[R, T] extends QueryActionExtensionMethodsImpl[R, Streaming[T]] {
    def result: StreamingDriverAction[R, T, Effect.Read]
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
