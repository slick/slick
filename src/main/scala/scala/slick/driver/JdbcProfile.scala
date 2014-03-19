package scala.slick.driver

import scala.language.implicitConversions
import scala.slick.ast.{Node, TypedType, BaseTypedType}
import scala.slick.compiler.{Phase, QueryCompiler}
import scala.slick.lifted._
import scala.slick.jdbc.{JdbcMappingCompilerComponent, JdbcBackend, Invoker}
import scala.slick.jdbc.meta.{MTable, createModel => jdbcCreateModel}
import scala.slick.profile.{SqlDriver, SqlProfile, Capability}
import scala.slick.model.Model

/**
 * A profile for accessing SQL databases via JDBC.
 */
trait JdbcProfile extends SqlProfile with JdbcTableComponent
  with JdbcInvokerComponent with JdbcExecutorComponent with JdbcTypesComponent { driver: JdbcDriver =>

  type Backend = JdbcBackend
  val backend: Backend = JdbcBackend
  val compiler = QueryCompiler.relational
  val simple: SimpleQL with Implicits = new SimpleQL with Implicits {}
  lazy val Implicit: Implicits = simple
  type ColumnType[T] = JdbcType[T]
  type BaseColumnType[T] = JdbcType[T] with BaseTypedType[T]
  val columnTypes = new JdbcTypes
  lazy val MappedColumnType = MappedJdbcType

  override protected def computeCapabilities = super.computeCapabilities ++ JdbcProfile.capabilities.all

  lazy val queryCompiler = compiler + new JdbcCodeGen(_.buildSelect)
  lazy val updateCompiler = compiler + new JdbcCodeGen(_.buildUpdate)
  lazy val deleteCompiler = compiler + new JdbcCodeGen(_.buildDelete)
  lazy val insertCompiler = QueryCompiler(Phase.inline, Phase.assignUniqueSymbols, new JdbcInsertCompiler)

  final def buildTableSchemaDescription(table: Table[_]): DDL = createTableDDLBuilder(table).buildDDL
  final def buildSequenceSchemaDescription(seq: Sequence[_]): DDL = createSequenceDDLBuilder(seq).buildDDL

  trait LowPriorityImplicits {
    implicit def queryToAppliedQueryInvoker[U](q: Query[_,U]): QueryInvoker[U] = createQueryInvoker[U](queryCompiler.run(q.toNode).tree, ())
    implicit def queryToUpdateInvoker[E, U](q: Query[E, U]): UpdateInvoker[U] = createUpdateInvoker(updateCompiler.run(q.toNode).tree, ())
  }

  trait Implicits extends LowPriorityImplicits with super.Implicits with ImplicitColumnTypes {
    implicit def ddlToDDLInvoker(d: DDL): DDLInvoker = createDDLInvoker(d)
    implicit def queryToDeleteInvoker(q: Query[_ <: Table[_], _]): DeleteInvoker = createDeleteInvoker(deleteCompiler.run(q.toNode).tree, ())
    implicit def runnableCompiledToAppliedQueryInvoker[RU](c: RunnableCompiled[_ <: Query[_, _], Seq[RU]]): QueryInvoker[RU] = createQueryInvoker[RU](c.compiledQuery, c.param)
    implicit def runnableCompiledToUpdateInvoker[RU](c: RunnableCompiled[_ <: Query[_, _], Seq[RU]]): UpdateInvoker[RU] =
      createUpdateInvoker(c.compiledUpdate, c.param)
    implicit def runnableCompiledToDeleteInvoker[RU](c: RunnableCompiled[_ <: Query[_, _], Seq[RU]]): DeleteInvoker =
      createDeleteInvoker(c.compiledDelete, c.param)

    // This conversion only works for fully packed types
    implicit def productQueryToUpdateInvoker[T](q: Query[_ <: ColumnBase[T], T]): UpdateInvoker[T] =
      createUpdateInvoker(updateCompiler.run(q.toNode).tree, ())
  }

  /**
   * Jdbc meta data for all tables
   */
  def getTables: Invoker[MTable] = MTable.getTables

  /** Gets the Slick data model describing this data source */
  def createModel(implicit session: Backend#Session): Model = jdbcCreateModel(getTables.list,this)
}

object JdbcProfile {
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

trait JdbcDriver extends SqlDriver
  with JdbcProfile
  with JdbcStatementBuilderComponent
  with JdbcMappingCompilerComponent { driver =>

  override val profile: JdbcProfile = this
}

object JdbcDriver extends JdbcDriver
