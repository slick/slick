package scala.slick.driver

import scala.language.{implicitConversions, higherKinds}
import scala.slick.ast.{BaseTypedType, Node}
import scala.slick.compiler.{Phase, QueryCompiler, InsertCompiler}
import scala.slick.lifted._
import scala.slick.jdbc.{JdbcMappingCompilerComponent, JdbcBackend, Invoker, JdbcType, JdbcFastPath}
import scala.slick.profile.{SqlDriver, SqlProfile, Capability}

/** A profile for accessing SQL databases via JDBC. All drivers for JDBC-based databases
  * implement this profile. */
trait JdbcProfile extends SqlProfile with JdbcTableComponent
  with JdbcInvokerComponent with JdbcInsertInvokerComponent with JdbcExecutorComponent with JdbcTypesComponent
  with JdbcModelComponent { driver: JdbcDriver =>

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
  lazy val checkConstraintCompiler = compiler + new JdbcCodeGen(_.buildCheckConstraint)
  lazy val insertCompiler = QueryCompiler(Phase.assignUniqueSymbols, new InsertCompiler(InsertCompiler.NonAutoInc), new JdbcInsertCodeGen(createInsertBuilder))
  lazy val forceInsertCompiler = QueryCompiler(Phase.assignUniqueSymbols, new InsertCompiler(InsertCompiler.AllColumns), new JdbcInsertCodeGen(createInsertBuilder))
  lazy val upsertCompiler = QueryCompiler(Phase.assignUniqueSymbols, new InsertCompiler(InsertCompiler.AllColumns), new JdbcInsertCodeGen(createUpsertBuilder))
  lazy val checkInsertCompiler = QueryCompiler(Phase.assignUniqueSymbols, new InsertCompiler(InsertCompiler.PrimaryKeys), new JdbcInsertCodeGen(createCheckInsertBuilder))
  lazy val updateInsertCompiler = QueryCompiler(Phase.assignUniqueSymbols, new InsertCompiler(InsertCompiler.AllColumns), new JdbcInsertCodeGen(createUpdateInsertBuilder))
  def compileInsert(tree: Node) = new JdbcCompiledInsert(tree)
  type CompiledInsert = JdbcCompiledInsert

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
    implicit def productQueryToUpdateInvoker[T, C[_]](q: Query[_ <: Rep[T], T, C]): UpdateInvoker[T] =
      createUpdateInvoker(updateCompiler.run(q.toNode).tree, ())
  }

  trait SimpleQL extends super.SimpleQL with Implicits {
    type FastPath[T] = JdbcFastPath[T]
  }
}

object JdbcProfile {
  /** The capabilities specific to `JdbcProfile` */
  object capabilities {
    /** Can be used for reverse-engineering the database schema */
    val createModel = Capability("jdbc.createModel")
    /** Can insert into AutoInc columns. */
    val forceInsert = Capability("jdbc.forceInsert")
    /** Supports a native insertOrUpdate command. Ootherwise the functionality
      * is emulated on the client side. The emulation uses transactions for
      * consistency but does not guarantee atomicity, so it may fail if another
      * insert for the same key happens concurrently. */
    val insertOrUpdate = Capability("jdbc.insertOrUpdate")
    /** Supports mutable result sets */
    val mutable = Capability("jdbc.mutable")
    /** Can return primary key of inserted rows */
    val returnInsertKey = Capability("jdbc.returnInsertKey")
    /** Can also return non-primary-key columns of inserted rows */
    val returnInsertOther = Capability("jdbc.returnInsertOther")

    /** Supports all JdbcProfile features which do not have separate capability values */
    val other = Capability("jdbc.other")

    /** All JDBC capabilities */
    val all = Set(other, createModel, forceInsert, insertOrUpdate, mutable, returnInsertKey, returnInsertOther)
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
