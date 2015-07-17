package slick.driver

import slick.ast.TypeUtil.:@
import slick.relational.CompiledMapping

import scala.collection.mutable.Builder
import scala.language.{implicitConversions, higherKinds}
import slick.ast._
import slick.compiler.{Phase, QueryCompiler, InsertCompiler}
import slick.lifted._
import slick.jdbc._
import slick.profile.{SqlDriver, SqlProfile, Capability}

/** A profile for accessing SQL databases via JDBC. All drivers for JDBC-based databases
  * implement this profile. */
trait JdbcProfile extends SqlProfile with JdbcActionComponent
  with JdbcInvokerComponent with JdbcTypesComponent
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

  trait LowPriorityAPI {
    implicit def queryUpdateActionExtensionMethods[U, C[_]](q: Query[_, U, C]): UpdateActionExtensionMethodsImpl[U] =
      createUpdateActionExtensionMethods(updateCompiler.run(q.toNode).tree, ())
  }

  trait API extends LowPriorityAPI with super.API with ImplicitColumnTypes {
    type FastPath[T] = JdbcFastPath[T]
    type SimpleDBIO[+R] = SimpleJdbcAction[R]
    val SimpleDBIO = SimpleJdbcAction

    implicit def jdbcFastPathExtensionMethods[T, P](mp: MappedProjection[T, P]) = new JdbcFastPathExtensionMethods[T, P](mp)

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

  val api: API = new API {}

  def runSynchronousQuery[R](tree: Node, param: Any)(implicit session: Backend#Session): R = tree match {
    case rsm @ ResultSetMapping(_, _, CompiledMapping(_, elemType)) :@ CollectionType(cons, el) =>
      val b = cons.createBuilder(el.classTag).asInstanceOf[Builder[Any, R]]
      createQueryInvoker[Any](rsm, param, null).foreach({ x => b += x }, 0)(session)
      b.result()
    case First(rsm: ResultSetMapping) =>
      createQueryInvoker[R](rsm, param, null).first
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
