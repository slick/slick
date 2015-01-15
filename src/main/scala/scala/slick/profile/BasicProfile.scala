package scala.slick.profile

import scala.language.{higherKinds, implicitConversions, existentials}
import scala.slick.SlickException
import scala.slick.compiler.QueryCompiler
import scala.slick.backend.DatabaseComponent
import scala.slick.action._
import scala.slick.ast._
import scala.slick.lifted._
import com.typesafe.config.{ConfigFactory, Config}
import scala.slick.util.GlobalConfig

/**
 * The basic functionality that has to be implemented by all drivers.
 */
trait BasicProfile extends BasicInvokerComponent with BasicInsertInvokerComponent with BasicExecutorComponent with BasicActionComponent { driver: BasicDriver =>

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

  protected trait CommonImplicits extends ExtensionMethodConversions {
    implicit val slickDriver: driver.type = driver

    // Work-around for SI-3346
    @inline implicit final def anyToToShapedValue[T](value: T) = new ToShapedValue[T](value)
  }

  protected trait CommonAPI extends Aliases {
    type Database = Backend#Database
    val Database = backend.Database
    type Session = Backend#Session
    type SlickException = scala.slick.SlickException
  }

  trait Implicits extends CommonImplicits {
    implicit def ddlToDDLInvoker(d: SchemaDescription): DDLInvoker

    implicit def repToQueryExecutor[U](rep: Rep[U]): QueryExecutor[U] = createQueryExecutor[U](queryCompiler.run(rep.toNode).tree, ())
    implicit def runnableCompiledToQueryExecutor[RU](c: RunnableCompiled[_, RU]): QueryExecutor[RU] = createQueryExecutor[RU](c.compiledQuery, c.param)
    implicit def streamableCompiledToInsertInvoker[EU](c: StreamableCompiled[_, _, EU]): InsertInvoker[EU] = createInsertInvoker[EU](c.compiledInsert.asInstanceOf[CompiledInsert])
    // This only works on Scala 2.11 due to SI-3346:
    implicit def recordToQueryExecutor[M, R](q: M)(implicit shape: Shape[_ <: FlatShapeLevel, M, R, _]): QueryExecutor[R] = createQueryExecutor[R](queryCompiler.run(shape.toNode(q)).tree, ())
    implicit def queryToInsertInvoker[U, C[_]](q: Query[_, U, C]) = createInsertInvoker[U](compileInsert(q.toNode))
  }

  trait SimpleQL extends CommonAPI with Implicits

  trait API extends CommonAPI with CommonImplicits {
    implicit def repQueryActionExtensionMethods[U](rep: Rep[U]): QueryActionExtensionMethods[U, NoStream] =
      createQueryActionExtensionMethods[U, NoStream](queryCompiler.run(rep.toNode).tree, ())
    implicit def streamableQueryActionExtensionMethods[U, C[_]](q: Query[_,U, C]): StreamingQueryActionExtensionMethods[C[U], U] =
      createStreamingQueryActionExtensionMethods[C[U], U](queryCompiler.run(q.toNode).tree, ())
    implicit def runnableCompiledQueryActionExtensionMethods[RU](c: RunnableCompiled[_, RU]): QueryActionExtensionMethods[RU, NoStream] =
      createQueryActionExtensionMethods[RU, NoStream](c.compiledQuery, c.param)
    implicit def streamableCompiledQueryActionExtensionMethods[RU, EU](c: StreamableCompiled[_, RU, EU]): StreamingQueryActionExtensionMethods[RU, EU] =
      createStreamingQueryActionExtensionMethods[RU, EU](c.compiledQuery, c.param)
    // Applying a CompiledFunction always results in only a RunnableCompiled, not a StreamableCompiled, so we need this:
    implicit def runnableStreamableCompiledQueryActionExtensionMethods[R, RU, EU, C[_]](c: RunnableCompiled[Query[R, EU, C], RU]): StreamingQueryActionExtensionMethods[RU, EU] =
      createStreamingQueryActionExtensionMethods[RU, EU](c.compiledQuery, c.param)
    // This only works on Scala 2.11 due to SI-3346:
    implicit def recordQueryActionExtensionMethods[M, R](q: M)(implicit shape: Shape[_ <: FlatShapeLevel, M, R, _]): QueryActionExtensionMethods[R, NoStream] =
      createQueryActionExtensionMethods[R, NoStream](queryCompiler.run(shape.toNode(q)).tree, ())
  }

  /** A collection of values for using the query language with a single import
    * statement. This provides the driver's implicits, the Database and
    * Session objects for DB connections, and commonly used query language
    * types and objects. */
  @deprecated("Use 'api' instead of 'simple' or 'Implicit' to import the new API", "3.0")
  val simple: SimpleQL

  /** The implicit values and conversions provided by this driver.
    * This is a subset of ``simple``. You usually want to import
    * `simple._` instead of using `Implicit`. */
  @deprecated("Use 'api' instead of 'simple' or 'Implicit' to import the new API", "3.0")
  val Implicit: Implicits

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

  /** (Partially) ompile an AST for insert operations */
  def compileInsert(n: Node): CompiledInsert
}

trait BasicDriver extends BasicProfile {
  /** The external interface of this driver which defines the API. */
  val profile: BasicProfile = this

  override def toString = {
    val cl = getClass.getName
    if(cl.startsWith("scala.slick.driver.") && cl.endsWith("$"))
      cl.substring(19, cl.length-1)
    else super.toString
  }

  /** The configuration for this driver, loaded via [[loadDriverConfig]]. */
  final lazy val driverConfig: Config = loadDriverConfig

  /** Load the configuration for this driver. This can be overridden in user-defined driver
    * subclasses to load different configurations.
    *
    * The default implementation does a breadth-first search in the supertype hierarchy of the
    * runtime class until it finds a class or trait matching "scala.slick.driver.XXXDriver"
    * where XXX is an arbitrary name, and then returns the path "slick.driver.XXX" from the
    * application config, if it exists, otherwise an empty Config object. */
  protected[this] def loadDriverConfig: Config = {
    def findConfigName(classes: Vector[Class[_]]): Option[String] =
      classes.iterator.map { cl =>
        val n = cl.getName
        if(n.startsWith("scala.slick.driver.") && n.endsWith("Driver")) Some(n.substring(19, n.length-6))
        else None
      }.find(_.isDefined).getOrElse {
        val parents = classes.flatMap { cl => Option(cl.getSuperclass) ++: cl.getInterfaces.toVector }
        if(parents.isEmpty) None else findConfigName(parents)
      }
    GlobalConfig.driverConfig(findConfigName(Vector(getClass)).get)
  }
}

trait BasicInvokerComponent { driver: BasicDriver =>

  /** Create a DDLInvoker -- this method should be implemented by drivers as needed */
  def createDDLInvoker(ddl: SchemaDescription): DDLInvoker

  /** Pseudo-invoker for running DDL statements. */
  trait DDLInvoker {
    /** Create the entities described by this DDL object */
    def create(implicit session: Backend#Session): Unit

    /** Drop the entities described by this DDL object */
    def drop(implicit session: Backend#Session): Unit

    def ddlInvoker: this.type = this
  }
}

trait BasicInsertInvokerComponent { driver: BasicDriver =>

  /** The type of insert invokers returned by the driver */
  type InsertInvoker[T] <: InsertInvokerDef[T]

  /** Create an InsertInvoker -- this method should be implemented by drivers as needed */
  def createInsertInvoker[T](compiled: CompiledInsert): InsertInvoker[T]

  /** The type of a (partially) compiled AST for Insert operations. Unlike
    * querying or deleting, inserts may require different compilation results
    * which should be computed lazily. */
  type CompiledInsert

  /** Defines the standard InsertInvoker methods for inserting data,
    * which are available at the level of BasicProfile. */
  trait InsertInvokerDef[T] {
    /** The result type when inserting a single value */
    type SingleInsertResult

    /** The result type when inserting a collection of values */
    type MultiInsertResult

    /** Insert a single value */
    def += (value: T)(implicit session: Backend#Session): SingleInsertResult

    /** Insert a collection of values */
    def ++= (values: Iterable[T])(implicit session: Backend#Session): MultiInsertResult

    /** Return a reference to this InsertInvoker */
    def insertInvoker: this.type = this
  }
}

trait BasicExecutorComponent { driver: BasicDriver =>

  /** The type of query executors returned by the driver */
  type QueryExecutor[T] <: QueryExecutorDef[T]

  /** Create an executor -- this method should be implemented by drivers as needed */
  def createQueryExecutor[R](tree: Node, param: Any): QueryExecutor[R]

  /** Base class for `QueryExecutor` implementations */
  trait QueryExecutorDef[R] {
    def run(implicit session: Backend#Session): R
    def executor: this.type = this
  }
}

trait BasicActionComponent { driver: BasicDriver =>

  type DriverAction[-E <: Effect, +R, +S <: NoStream] <: BasicAction[E, R, S]
  type StreamingDriverAction[-E <: Effect, +R, +T] <: BasicStreamingAction[E, R, T] with DriverAction[E, R, Streaming[T]]

  //////////////////////////////////////////////////////////// Query Actions

  type QueryActionExtensionMethods[R, S <: NoStream] <: QueryActionExtensionMethodsImpl[R, S]
  type StreamingQueryActionExtensionMethods[R, T] <: StreamingQueryActionExtensionMethodsImpl[R, T]

  def createQueryActionExtensionMethods[R, S <: NoStream](tree: Node, param: Any): QueryActionExtensionMethods[R, S]
  def createStreamingQueryActionExtensionMethods[R, T](tree: Node, param: Any): StreamingQueryActionExtensionMethods[R, T]

  trait QueryActionExtensionMethodsImpl[R, S <: NoStream] {
    /** An Action that runs this query. */
    def result: DriverAction[Effect.Read, R, S]
  }

  trait StreamingQueryActionExtensionMethodsImpl[R, T] extends QueryActionExtensionMethodsImpl[R, Streaming[T]] {
    def result: StreamingDriverAction[Effect.Read, R, T]
  }
}

trait BasicAction[-E <: Effect, +R, +S <: NoStream] extends DatabaseAction[E, R, S] {
  type ResultAction[-E <: Effect, +R, +S <: NoStream] <: BasicAction[E, R, S]
}

trait BasicStreamingAction[-E <: Effect, +R, +T] extends BasicAction[E, R, Streaming[T]] {
  /** Create an Action that returns only the first value of this stream of data. The Action will
    * fail if the stream is empty. Only available on streaming Actions. */
  def head: ResultAction[E, T, NoStream]

  /** Create an Action that returns only the first value of this stream of data as an `Option`.
    * Only available on streaming Actions. */
  def headOption: ResultAction[E, Option[T], NoStream]
}

trait FixedBasicAction[-E <: Effect, +R, +S <: NoStream] extends BasicAction[E, R, S] {
  type ResultAction[-E <: Effect, +R, +S <: NoStream] = BasicAction[E, R, S]
}

trait FixedBasicStreamingAction[-E <: Effect, +R, +T] extends BasicStreamingAction[E, R, T] with FixedBasicAction[E, R, Streaming[T]]
