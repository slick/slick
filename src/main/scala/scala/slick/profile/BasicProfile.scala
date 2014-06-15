package scala.slick.profile

import scala.language.{higherKinds, implicitConversions}
import scala.slick.compiler.QueryCompiler
import scala.slick.backend.DatabaseComponent
import scala.slick.ast._
import scala.slick.lifted._

/**
 * The basic functionality that has to be implemented by all drivers.
 */
trait BasicProfile extends BasicInvokerComponent with BasicInsertInvokerComponent with BasicExecutorComponent { driver: BasicDriver =>

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

  /** A collection of values for using the query language with a single import
    * statement. This provides the driver's implicits, the Database and
    * Session objects for DB connections, and commonly used query language
    * types and objects. */
  val simple: SimpleQL

  /** The implicit values and conversions provided by this driver.
    * This is a subset of ``simple``. You usually want to import
    * `simple._` instead of using `Implicit`. */
  val Implicit: Implicits

  trait Implicits extends ExtensionMethodConversions {
    implicit val slickDriver: driver.type = driver
    implicit def ddlToDDLInvoker(d: SchemaDescription): DDLInvoker

    implicit def repToQueryExecutor[U](rep: Rep[U]): QueryExecutor[U] = createQueryExecutor[U](queryCompiler.run(rep.toNode).tree, ())
    implicit def runnableCompiledToQueryExecutor[RU](c: RunnableCompiled[_, RU]): QueryExecutor[RU] = createQueryExecutor[RU](c.compiledQuery, c.param)
    implicit def streamableCompiledToInsertInvoker[EU](c: StreamableCompiled[_, _, EU]): InsertInvoker[EU] = createInsertInvoker[EU](c.compiledInsert.asInstanceOf[CompiledInsert])
    // This only works on Scala 2.11 due to SI-3346:
    implicit def recordToQueryExecutor[M, R](q: M)(implicit shape: Shape[_ <: FlatShapeLevel, M, R, _]): QueryExecutor[R] = createQueryExecutor[R](queryCompiler.run(shape.toNode(q)).tree, ())
    implicit def queryToInsertInvoker[U, C[_]](q: Query[_, U, C]) = createInsertInvoker[U](compileInsert(q.toNode))

    // Work-around for SI-3346
    @inline implicit final def anyToToShapedValue[T](value: T) = new ToShapedValue[T](value)
  }

  trait SimpleQL extends Implicits with scala.slick.lifted.Aliases {
    type Database = Backend#Database
    val Database = backend.Database
    type Session = Backend#Session
    type SlickException = scala.slick.SlickException
  }

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
