package scala.slick.driver

import scala.language.implicitConversions
import scala.slick.ast.{Node, TypedType, BaseTypedType}
import scala.slick.compiler.{Phase, QueryCompiler}
import scala.slick.lifted._
import scala.slick.jdbc.{MappedJdbcType, JdbcMappingCompilerComponent, JdbcType, MutatingUnitInvoker, JdbcBackend}
import scala.slick.profile.{StandardParameterizedQueries, SqlDriver, SqlProfile, Capability}
import scala.slick.SlickException

/**
 * A profile for accessing SQL databases via JDBC.
 */
trait JdbcProfile extends SqlProfile with JdbcTableComponent
  with JdbcInvokerComponent with JdbcExecutorComponent with StandardParameterizedQueries { driver: JdbcDriver =>

  type Backend = JdbcBackend
  val backend: Backend = JdbcBackend
  val compiler = QueryCompiler.relational
  val Implicit: Implicits = new Implicits {}
  val simple: SimpleQL = new SimpleQL {}
  type ColumnType[T] = JdbcType[T]
  type BaseColumnType[T] = JdbcType[T] with BaseTypedType[T]
  val columnTypes = new JdbcTypes

  override protected def computeCapabilities = super.computeCapabilities ++ JdbcProfile.capabilities.all

  lazy val queryCompiler = compiler + new JdbcCodeGen(_.buildSelect)
  lazy val updateCompiler = compiler + new JdbcCodeGen(_.buildUpdate)
  lazy val deleteCompiler = compiler + new JdbcCodeGen(_.buildDelete)
  lazy val insertCompiler = QueryCompiler(Phase.inline, Phase.assignUniqueSymbols, new JdbcInsertCompiler)

  final def buildTableSchemaDescription(table: Table[_]): DDL = createTableDDLBuilder(table).buildDDL
  final def buildSequenceSchemaDescription(seq: Sequence[_]): DDL = createSequenceDDLBuilder(seq).buildDDL

  def compileParameterizedQuery[P,R](q: Query[_, R]) =
    new ParameterizedQuery[P, R](queryCompiler.run(q.toNode).tree)

  trait LowPriorityImplicits {
    implicit def queryToAppliedQueryInvoker[T, U](q: Query[T, _ <: U]): UnitQueryInvoker[U] = createUnitQueryInvoker[U](queryCompiler.run(q.toNode).tree)
    implicit def queryToUpdateInvoker[E, U](q: Query[E, U]): UpdateInvoker[U] = createUpdateInvoker(updateCompiler.run(q.toNode).tree)
  }

  trait Implicits extends LowPriorityImplicits with super.Implicits with ImplicitColumnTypes {
    implicit def ddlToDDLInvoker(d: DDL): DDLInvoker = new DDLInvoker(d)
    implicit def queryToDeleteInvoker(q: Query[_ <: Table[_], _]): DeleteInvoker = new DeleteInvoker(deleteCompiler.run(q.toNode).tree)
    implicit def parameterizedQueryToQueryInvoker[P, R](q: ParameterizedQuery[P, R]): QueryInvoker[P, R] = createQueryInvoker[P, R](q.tree)
    implicit def appliedQueryToAppliedQueryInvoker[R](q: AppliedQuery[R]): MutatingUnitInvoker[R] = createQueryInvoker[Any, R](q.tree)(q.param)

    // This conversion only works for fully packed types
    implicit def productQueryToUpdateInvoker[T](q: Query[_ <: ColumnBase[T], T]): UpdateInvoker[T] =
      createUpdateInvoker(updateCompiler.run(q.toNode).tree)
  }

  trait SimpleQL extends super.SimpleQL with Implicits {
    type MappedColumnType[T, U] = MappedJdbcType[T, U]
    val MappedColumnType = MappedJdbcType
  }
}

object JdbcProfile {
  object capabilities {
    /** Supports mutable result sets */
    val mutable = Capability("jdbc.mutable")
    /** Can return primary key of inserted row */
    val returnInsertKey = Capability("jdbc.returnInsertKey")
    /** Can also return non-primary-key columns of inserted row */
    val returnInsertOther = Capability("jdbc.returnInsertOther")

    /** Supports all JdbcProfile features which do not have separate capability values */
    val other = Capability("jdbc.other")

    /** All JDBC capabilities */
    val all = Set(other, mutable, returnInsertKey, returnInsertOther)
  }
}

trait JdbcDriver extends SqlDriver
  with JdbcProfile
  with JdbcStatementBuilderComponent
  with JdbcMappingCompilerComponent
  with JdbcTypesComponent { driver =>

  override val profile: JdbcProfile = this

  def quote[T](v: T)(implicit tm: TypedType[T]): String = typeInfoFor(tm).valueToSQLLiteral(v)
}

object JdbcDriver extends JdbcDriver
