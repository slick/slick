package slick.driver

import scala.language.{implicitConversions, higherKinds}
import slick.ast.{BaseTypedType, Node}
import slick.compiler.{Phase, QueryCompiler, InsertCompiler}
import slick.lifted._
import slick.jdbc._
import slick.profile.{SqlDriver, SqlProfile, Capability}

/** A profile for accessing SQL databases via JDBC. All drivers for JDBC-based databases
  * implement this profile. */
trait JdbcProfile extends SqlProfile with JdbcActionComponent
  with JdbcInvokerComponent with JdbcInsertInvokerComponent with JdbcExecutorComponent with JdbcTypesComponent
  with JdbcModelComponent { driver: JdbcDriver =>

  type Backend = JdbcBackend
  val backend: Backend = JdbcBackend
  type ColumnType[T] = JdbcType[T]
  type BaseColumnType[T] = JdbcType[T] with BaseTypedType[T]
  val columnTypes = new JdbcTypes
  lazy val MappedColumnType = MappedJdbcType

  override protected def computeCapabilities = super.computeCapabilities ++ JdbcProfile.capabilities.all

  lazy val queryCompiler = compiler + new JdbcCodeGen(_.buildSelect)
  lazy val updateCompiler = compiler + new JdbcCodeGen(_.buildUpdate)
  lazy val deleteCompiler = compiler + new JdbcCodeGen(_.buildDelete)
  lazy val insertCompiler = QueryCompiler(Phase.assignUniqueSymbols, Phase.inferTypes, new InsertCompiler(InsertCompiler.NonAutoInc), new JdbcInsertCodeGen(createInsertBuilder))
  lazy val forceInsertCompiler = QueryCompiler(Phase.assignUniqueSymbols, Phase.inferTypes, new InsertCompiler(InsertCompiler.AllColumns), new JdbcInsertCodeGen(createInsertBuilder))
  lazy val upsertCompiler = QueryCompiler(Phase.assignUniqueSymbols, Phase.inferTypes, new InsertCompiler(InsertCompiler.AllColumns), new JdbcInsertCodeGen(createUpsertBuilder))
  lazy val checkInsertCompiler = QueryCompiler(Phase.assignUniqueSymbols, Phase.inferTypes, new InsertCompiler(InsertCompiler.PrimaryKeys), new JdbcInsertCodeGen(createCheckInsertBuilder))
  lazy val updateInsertCompiler = QueryCompiler(Phase.assignUniqueSymbols, Phase.inferTypes, new InsertCompiler(InsertCompiler.AllColumns), new JdbcInsertCodeGen(createUpdateInsertBuilder))
  def compileInsert(tree: Node) = new JdbcCompiledInsert(tree)
  type CompiledInsert = JdbcCompiledInsert

  final def buildTableSchemaDescription(table: Table[_]): DDL = createTableDDLBuilder(table).buildDDL
  final def buildSequenceSchemaDescription(seq: Sequence[_]): DDL = createSequenceDDLBuilder(seq).buildDDL

  protected trait CommonImplicits extends super.CommonImplicits with ImplicitColumnTypes {
    implicit def jdbcFastPathExtensionMethods[T, P](mp: MappedProjection[T, P]) = new JdbcFastPathExtensionMethods[T, P](mp)
  }

  protected trait LowPriorityImplicits {
    implicit def queryToAppliedQueryInvoker[U, C[_]](q: Query[_,U, C]): QueryInvoker[U] = createQueryInvoker[U](queryCompiler.run(q.toNode).tree, (), null)
    implicit def queryToUpdateInvoker[U, C[_]](q: Query[_, U, C]): UpdateInvoker[U] = createUpdateInvoker(updateCompiler.run(q.toNode).tree, ())
  }

  trait Implicits extends LowPriorityImplicits with super.Implicits with CommonImplicits {
    implicit def ddlToDDLInvoker(d: DDL): DDLInvoker = createDDLInvoker(d)
    implicit def queryToDeleteInvoker[C[_]](q: Query[_ <: Table[_], _, C]): DeleteInvoker = createDeleteInvoker(deleteCompiler.run(q.toNode).tree, ())
    implicit def runnableCompiledToAppliedQueryInvoker[RU, C[_]](c: RunnableCompiled[_ <: Query[_, _, C], C[RU]]): QueryInvoker[RU] = createQueryInvoker[RU](c.compiledQuery, c.param, null)
    implicit def runnableCompiledToUpdateInvoker[RU, C[_]](c: RunnableCompiled[_ <: Query[_, _, C], C[RU]]): UpdateInvoker[RU] =
      createUpdateInvoker(c.compiledUpdate, c.param)
    implicit def runnableCompiledToDeleteInvoker[RU, C[_]](c: RunnableCompiled[_ <: Query[_, _, C], C[RU]]): DeleteInvoker =
      createDeleteInvoker(c.compiledDelete, c.param)

    // This conversion only works for fully packed types
    implicit def productQueryToUpdateInvoker[T, C[_]](q: Query[_ <: Rep[T], T, C]): UpdateInvoker[T] =
      createUpdateInvoker(updateCompiler.run(q.toNode).tree, ())
  }

  trait SimpleQL extends super.SimpleQL with Implicits {
    type FastPath[T] = JdbcFastPath[T]
  }

  trait LowPriorityAPI {
    implicit def queryUpdateActionExtensionMethods[U, C[_]](q: Query[_, U, C]): UpdateActionExtensionMethodsImpl[U] =
      createUpdateActionExtensionMethods(updateCompiler.run(q.toNode).tree, ())
  }

  trait API extends LowPriorityAPI with super.API with CommonImplicits {
    type FastPath[T] = JdbcFastPath[T]
    type SimpleDBIO[+R] = SimpleJdbcAction[R]
    val SimpleDBIO = SimpleJdbcAction

    implicit def queryDeleteActionExtensionMethods[C[_]](q: Query[_ <: Table[_], _, C]): DeleteActionExtensionMethods =
      createDeleteActionExtensionMethods(deleteCompiler.run(q.toNode).tree, ())
    implicit def runnableCompiledDeleteActionExtensionMethods[RU, C[_]](c: RunnableCompiled[_ <: Query[_, _, C], C[RU]]): DeleteActionExtensionMethods =
      createDeleteActionExtensionMethods(c.compiledDelete, c.param)

    implicit def runnableCompiledUpdateActionExtensionMethods[RU, C[_]](c: RunnableCompiled[_ <: Query[_, _, C], C[RU]]): UpdateActionExtensionMethods[RU] =
      createUpdateActionExtensionMethods(c.compiledUpdate, c.param)

    implicit def jdbcActionExtensionMethods[E <: Effect, R, S <: NoStream](a: DBIOAction[R, S, E]): JdbcActionExtensionMethods[E, R, S] =
      new JdbcActionExtensionMethods[E, R, S](a)

    implicit def actionBasedSQLInterpolation(s: StringContext) = new ActionBasedSQLInterpolation(s)
  }

  @deprecated("Use 'api' instead of 'simple' or 'Implicit' to import the new API", "3.0")
  val simple: SimpleQL = new SimpleQL {}
  @deprecated("Use 'api' instead of 'simple' or 'Implicit' to import the new API", "3.0")
  lazy val Implicit: Implicits = simple
  val api: API = new API {}
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
    /** Returns column default values in meta data */
    val defaultValueMetaData = Capability("jdbc.defaultValueMetaData")
    /** Doesn't map types to Boolean in DatabaseMetaData */
    val booleanMetaData = Capability("jdbc.booleanMetaData")
    /** Reports no default and NULL default differently in meta data */
    val nullableNoDefault = Capability("jdbc.nullableNoDefault")
    /** Makes a difference between different integer types */
    val distinguishesIntTypes = Capability("jdbc.distinguishesIntTypes")
    /** Has a datatype directly corresponding to Scala Byte */
    val supportsByte = Capability("jdbc.supportsByte")

    /** Supports all JdbcProfile features which do not have separate capability values */
    val other = Capability("jdbc.other")

    /** All JDBC capabilities */
    val all = Set(other, createModel, forceInsert, insertOrUpdate, mutable, returnInsertKey, defaultValueMetaData, booleanMetaData, nullableNoDefault, distinguishesIntTypes, supportsByte, returnInsertOther)
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
@deprecated("JdbcDriver provides a generic implementation that needs to be customized for specific database systems; Use a concrete driver or implement and customize JdbcDriver yourself", "3.0.0")
object JdbcDriver extends JdbcDriver
