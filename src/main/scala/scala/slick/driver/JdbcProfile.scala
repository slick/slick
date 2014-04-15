package scala.slick.driver

import scala.language.{implicitConversions, higherKinds}
import scala.slick.ast.BaseTypedType
import scala.slick.compiler.{Phase, QueryCompiler}
import scala.slick.lifted._
import scala.slick.jdbc.{JdbcMappingCompilerComponent, JdbcBackend, Invoker, JdbcType, JdbcFastPath}
import scala.slick.jdbc.meta.{MTable, createModel => jdbcCreateModel}
import scala.slick.profile.{SqlDriver, SqlProfile, Capability}
import scala.slick.model.Model

/** A profile for accessing SQL databases via JDBC. All drivers for JDBC-based databases
  * implement this profile. */
trait JdbcProfile extends SqlProfile with JdbcTableComponent
  with JdbcInvokerComponent with JdbcExecutorComponent with JdbcTypesComponent { driver: JdbcDriver =>

  type Backend = JdbcBackend
  val backend: Backend = JdbcBackend
  val simple: SimpleQL = new SimpleQL {}
  lazy val Implicit: Implicits = simple
  type ColumnType[T] = JdbcType[T]
  type BaseColumnType[T] = JdbcType[T] with BaseTypedType[T]
  val columnTypes = new JdbcTypes
  lazy val MappedColumnType = MappedJdbcType

  override protected def computeQueryCompiler = super.computeQueryCompiler ++ QueryCompiler.relationalPhases

  override protected def computeCapabilities = super.computeCapabilities ++ JdbcProfile.capabilities.all

  lazy val queryCompiler = compiler + new JdbcCodeGen(_.buildSelect)
  lazy val updateCompiler = compiler + new JdbcCodeGen(_.buildUpdate)
  lazy val deleteCompiler = compiler + new JdbcCodeGen(_.buildDelete)
  lazy val insertCompiler = QueryCompiler(Phase.assignUniqueSymbols, new JdbcInsertCompiler)

  final def buildTableSchemaDescription(table: Table[_]): DDL = createTableDDLBuilder(table).buildDDL
  final def buildSequenceSchemaDescription(seq: Sequence[_]): DDL = createSequenceDDLBuilder(seq).buildDDL

  trait LowPriorityImplicits {
    implicit def queryToAppliedQueryInvoker[U, C[_]](q: Query[_,U, C]): QueryInvoker[U] = createQueryInvoker[U](queryCompiler.run(q.toNode).tree, ())
    implicit def queryToUpdateInvoker[U, C[_]](q: Query[_, U, C]): UpdateInvoker[U] = createUpdateInvoker(updateCompiler.run(q.toNode).tree, ())
  }

  trait Implicits extends LowPriorityImplicits with super.Implicits with ImplicitColumnTypes {
    implicit def ddlToDDLInvoker(d: DDL): DDLInvoker = createDDLInvoker(d)
    implicit def queryToDeleteInvoker[C[_]](q: Query[_ <: Table[_], _, C]): DeleteInvoker = createDeleteInvoker(deleteCompiler.run(q.toNode).tree, ())
    implicit def runnableCompiledToAppliedQueryInvoker[RU, C[_]](c: RunnableCompiled[_ <: Query[_, _, C], C[RU]]): QueryInvoker[RU] = createQueryInvoker[RU](c.compiledQuery, c.param)
    implicit def runnableCompiledToUpdateInvoker[RU, C[_]](c: RunnableCompiled[_ <: Query[_, _, C], C[RU]]): UpdateInvoker[RU] =
      createUpdateInvoker(c.compiledUpdate, c.param)
    implicit def runnableCompiledToDeleteInvoker[RU, C[_]](c: RunnableCompiled[_ <: Query[_, _, C], C[RU]]): DeleteInvoker =
      createDeleteInvoker(c.compiledDelete, c.param)
    implicit def jdbcFastPathExtensionMethods[T, P](mp: MappedProjection[T, P]) = new JdbcFastPathExtensionMethods[T, P](mp)

    // This conversion only works for fully packed types
    implicit def productQueryToUpdateInvoker[T, C[_]](q: Query[_ <: ColumnBase[T], T, C]): UpdateInvoker[T] =
      createUpdateInvoker(updateCompiler.run(q.toNode).tree, ())
  }

  trait SimpleQL extends super.SimpleQL with Implicits {
    type FastPath[T] = JdbcFastPath[T]
  }

  /** Jdbc meta data for all tables */
  def getTables: Invoker[MTable] = MTable.getTables

  /** Gets the Slick data model describing this data source */
  def createModel(implicit session: Backend#Session): Model = jdbcCreateModel(getTables.list,this)
}

object JdbcProfile {
  /** The capabilities specific to `JdbcProfile` */
  object capabilities {
    /** Can insert into AutoInc columns. */
    val forceInsert = Capability("jdbc.forceInsert")
    /** Supports mutable result sets */
    val mutable = Capability("jdbc.mutable")
    /** Can return primary key of inserted row */
    val returnInsertKey = Capability("jdbc.returnInsertKey")
    /** Can also return non-primary-key columns of inserted row */
    val returnInsertOther = Capability("jdbc.returnInsertOther")
    /** Can also return non-primary-key columns of inserted row */
    val createModel = Capability("jdbc.createModel")

    /** Supports all JdbcProfile features which do not have separate capability values */
    val other = Capability("jdbc.other")

    /** All JDBC capabilities */
    val all = Set(other, forceInsert, mutable, returnInsertKey, returnInsertOther, createModel)
  }
}

/** The internal implementation details of `JdbcProfile`-based drivers. These can be
  * used by driver implementors but are not intended to be accessed by users of a
  * driver. */
trait JdbcDriver extends SqlDriver
  with JdbcProfile
  with JdbcStatementBuilderComponent
  with JdbcMappingCompilerComponent { driver =>

  override val profile: JdbcProfile = this
}

/** A generic driver for JDBC-based databases. This can be used as a fallback
  * when a specific driver for a database is not available. */
object JdbcDriver extends JdbcDriver
