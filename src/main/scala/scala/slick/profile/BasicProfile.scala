package scala.slick.profile

import scala.language.{higherKinds, implicitConversions}
import scala.slick.compiler.QueryCompiler
import scala.slick.backend.DatabaseComponent
import scala.slick.ast._
import scala.slick.lifted._

/**
 * The basic functionality that has to be implemented by all drivers.
 */
trait BasicProfile extends BasicInvokerComponent with BasicExecutorComponent { driver: BasicDriver =>

  /** The back-end type required by this profile */
  type Backend <: DatabaseComponent
  val backend: Backend

  final val capabilities: Set[Capability] = computeCapabilities
  protected def computeCapabilities: Set[Capability] = Set.empty

  /** The type of a pre-compiled parameterized query */
  type ParameterizedQuery[P, R] <: (P => AppliedQuery[R])

  /** The type of a pre-compiled applied query */
  type AppliedQuery[R] <: AppliedQueryDef[R]

  trait AppliedQueryDef[R] {
    def tree: Node
    def param: Any
  }

  def compileParameterizedQuery[P, R](q: Query[_, R]): ParameterizedQuery[P, R]

  /** The type of a schema description (DDL) */
  type SchemaDescription <: SchemaDescriptionDef

  /** A schema description contains the SQL statements for creating and
    * dropping database entities. Schema descriptions can be combined for
    * creating or dropping multiple entities together, even if they have
    * circular dependencies. */
  trait SchemaDescriptionDef {
    def ++(other: SchemaDescription): SchemaDescription
  }

  val Implicit: Implicits

  /** A collection of values for using the query language with a single import
    * statement. This provides the driver's implicits, the Database and
    * Session objects for DB connections, and commonly used query language
    * types and objects. */
  val simple: SimpleQL

  trait Implicits extends ExtensionMethodConversions {
    implicit val slickDriver: driver.type = driver
    implicit def ddlToDDLInvoker(d: SchemaDescription): DDLInvoker

    implicit def queryToQueryExecutor[E, U](q: Query[E, U]): QueryExecutor[Seq[U]] = createQueryExecutor[Seq[U]](queryCompiler.run(q.toNode).tree, ())
    implicit def appliedQueryToQueryExecutor[R](q: AppliedQuery[R]): QueryExecutor[Seq[R]] = createQueryExecutor[Seq[R]](q.tree, q.param)
    implicit def shapedValueToQueryExecutor[T, U](u: ShapedValue[T, U]): QueryExecutor[Seq[U]] = createQueryExecutor[Seq[U]](queryCompiler.run(u.toNode).tree, ())
    // We can't use this direct way due to SI-3346
    def recordToQueryExecutor[M, R](q: M)(implicit shape: Shape[_ <: ShapeLevel.Flat, M, R, _]): QueryExecutor[R] = createQueryExecutor[R](queryCompiler.run(shape.toNode(q)).tree, ())
    implicit final def recordToUnshapedQueryExecutor[M <: Rep[_]](q: M): UnshapedQueryExecutor[M] = new UnshapedQueryExecutor[M](q)

    implicit def columnBaseToInsertInvoker[T](c: ColumnBase[T]) = createInsertInvoker[T](insertCompiler.run(c.toNode).tree)
    implicit def shapedValueToInsertInvoker[T, U](u: ShapedValue[T, U]) = createInsertInvoker[U](insertCompiler.run(u.toNode).tree)
    implicit def queryToInsertInvoker[U](q: Query[_, U]) = createInsertInvoker[U](insertCompiler.run(q.toNode).tree)

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

  /** The compiler used for inserting data */
  def insertCompiler: QueryCompiler
}

trait BasicDriver extends BasicProfile {
  val profile: BasicProfile = this

  /** The driver-specific representation of types */
  type TypeInfo
  def typeInfoFor(t: Type): TypeInfo

  override def toString = {
    val cl = getClass.getName
    if(cl.startsWith("scala.slick.driver.") && cl.endsWith("$"))
      cl.substring(19, cl.length-1)
    else super.toString
  }
}

/** Standard implementations of parameterized queries */
trait StandardParameterizedQueries { driver: BasicDriver =>
  type ParameterizedQuery[P, R] = ParameterizedQueryImpl[P, R]
  type AppliedQuery[R] = AppliedQueryImpl[R]

  class ParameterizedQueryImpl[P, R](val tree: Node) extends (P => AppliedQueryImpl[R]) {
    def apply(param: P) = new AppliedQueryImpl[R](tree, param)
  }

  class AppliedQueryImpl[R](val tree: Node, val param: Any) extends AppliedQueryDef[R]
}

trait BasicInvokerComponent { driver: BasicDriver =>

  /** Pseudo-invoker for running DDL statements. */
  trait DDLInvoker {
    /** Create the entities described by this DDL object */
    def create(implicit session: Backend#Session): Unit

    /** Drop the entities described by this DDL object */
    def drop(implicit session: Backend#Session): Unit

    def ddlInvoker: this.type = this
  }

  /** The type of insert invokers returned by the driver */
  type InsertInvoker[T] <: InsertInvokerDef[T]

  /** Create an InsertInvoker -- this method should be implemented by drivers as needed */
  def createInsertInvoker[T](tree: Node): InsertInvoker[T]

  trait InsertInvokerDef[T] {
    type SingleInsertResult
    type MultiInsertResult

    /** Insert a single value */
    def += (value: T)(implicit session: Backend#Session): SingleInsertResult

    /** Insert a collection of values */
    def ++= (values: Iterable[T])(implicit session: Backend#Session): MultiInsertResult

    def insertInvoker: this.type = this
  }
}

trait BasicExecutorComponent { driver: BasicDriver =>

  /** The type of query executors returned by the driver */
  type QueryExecutor[T] <: QueryExecutorDef[T]

  /** Create an executor -- this method should be implemented by drivers as needed */
  def createQueryExecutor[R](tree: Node, param: Any): QueryExecutor[R]

  trait QueryExecutorDef[R] {
    def run(implicit session: Backend#Session): R
    def executor: this.type = this
  }

  // Work-around for SI-3346
  final class UnshapedQueryExecutor[M](val value: M) {
    @inline def run[U](implicit shape: Shape[_ <: ShapeLevel.Flat, M, U, _], session: Backend#Session): U =
      createQueryExecutor[U](queryCompiler.run(shape.toNode(value)).tree, ()).run
  }
}
