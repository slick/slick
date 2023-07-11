package slick.jdbc

import java.sql.{PreparedStatement, ResultSet}

import scala.collection.mutable
import scala.language.implicitConversions
import slick.ast.*
import slick.ast.TypeUtil.:@
import slick.compiler.{InsertCompiler, Phase, QueryCompiler}
import slick.lifted.*
import slick.relational.{CompiledMapping, RelationalProfile}
import slick.sql.SqlProfile

/** Abstract profile for accessing SQL databases via JDBC. */
trait JdbcProfile extends SqlProfile with JdbcActionComponent
  with JdbcInvokerComponent with JdbcTypesComponent with JdbcModelComponent
  /* internal: */ with JdbcStatementBuilderComponent with JdbcMappingCompilerComponent {

  type ResultConverterReader = ResultSet
  type ResultConverterWriter = PreparedStatement
  type ResultConverterUpdater = ResultSet

  type Backend = JdbcBackend
  val backend: Backend = JdbcBackend
  type ColumnType[T] = JdbcType[T]
  type BaseColumnType[T] = JdbcType[T] & BaseTypedType[T]
  val columnTypes = new JdbcTypes
  override lazy val MappedColumnType: MappedJdbcType.type = MappedJdbcType

  override protected def computeCapabilities = super.computeCapabilities ++ JdbcCapabilities.all

  lazy val queryCompiler = compiler + new JdbcCodeGen(_.buildSelect())
  lazy val updateCompiler = compiler + new JdbcCodeGen(_.buildUpdate())
  lazy val deleteCompiler = compiler + new JdbcCodeGen(_.buildDelete())
  lazy val insertCompiler =
    QueryCompiler(
      Phase.assignUniqueSymbols,
      Phase.inferTypes,
      new InsertCompiler(InsertCompiler.NonAutoInc),
      new JdbcInsertCodeGen(createInsertBuilder)
    )
  lazy val forceInsertCompiler =
    QueryCompiler(
      Phase.assignUniqueSymbols,
      Phase.inferTypes,
      new InsertCompiler(InsertCompiler.AllColumns),
      new JdbcInsertCodeGen(createInsertBuilder)
    )
  lazy val upsertCompiler =
    QueryCompiler(
      Phase.assignUniqueSymbols,
      Phase.inferTypes,
      new InsertCompiler(InsertCompiler.AllColumns),
      new JdbcInsertCodeGen(createUpsertBuilder)
    )
  lazy val checkInsertCompiler =
    QueryCompiler(
      Phase.assignUniqueSymbols,
      Phase.inferTypes,
      new InsertCompiler(InsertCompiler.PrimaryKeys),
      new JdbcInsertCodeGen(createCheckInsertBuilder)
    )
  lazy val updateInsertCompiler =
    QueryCompiler(
      Phase.assignUniqueSymbols,
      Phase.inferTypes,
      new InsertCompiler(InsertCompiler.AllColumns),
      new JdbcInsertCodeGen(createUpdateInsertBuilder)
    )
  def compileInsert(tree: Node) = new JdbcCompiledInsert(tree)
  type CompiledInsert = JdbcCompiledInsert

  final def buildTableSchemaDescription(table: Table[?]): DDL = createTableDDLBuilder(table).buildDDL
  final def buildSequenceSchemaDescription(seq: Sequence[?]): DDL = createSequenceDDLBuilder(seq).buildDDL

  trait JdbcLowPriorityAPI {
    implicit def queryUpdateActionExtensionMethods[U, C[_]](q: Query[?, U, C]): UpdateActionExtensionMethodsImpl[U] =
      createUpdateActionExtensionMethods(updateCompiler.run(q.toNode).tree, ())
  }

  trait JdbcAPI extends JdbcLowPriorityAPI with RelationalAPI with JdbcImplicitColumnTypes {
    type SimpleDBIO[+R] = SimpleJdbcAction[R]
    val SimpleDBIO = SimpleJdbcAction

    implicit def queryDeleteActionExtensionMethods[C[_]](q: Query[? <: RelationalProfile#Table[?], ?, C]
                                                        ): DeleteActionExtensionMethods =
      createDeleteActionExtensionMethods(deleteCompiler.run(q.toNode).tree, ())
    implicit def runnableCompiledDeleteActionExtensionMethods[RU, C[_]](c: RunnableCompiled[? <: Query[?, ?, C], C[RU]]
                                                                       ): DeleteActionExtensionMethods =
      createDeleteActionExtensionMethods(c.compiledDelete, c.param)

    implicit def runnableCompiledUpdateActionExtensionMethods[RU, C[_]](c: RunnableCompiled[? <: Query[?, ?, C], C[RU]]
                                                                       ): UpdateActionExtensionMethods[RU] =
      createUpdateActionExtensionMethods(c.compiledUpdate, c.param)

    implicit def jdbcActionExtensionMethods[E <: Effect, R, S <: NoStream](a: DBIOAction[R, S, E]
                                                                          ): JdbcActionExtensionMethods[E, R, S] =
      new JdbcActionExtensionMethods[E, R, S](a)

    implicit def actionBasedSQLInterpolation(s: StringContext): ActionBasedSQLInterpolation =
      new ActionBasedSQLInterpolation(s)
  }

  val api: JdbcAPI = new JdbcAPI {}

  def runSynchronousQuery[R](tree: Node, param: Any)(implicit session: backend.Session): R = tree match {
    case rsm @ ResultSetMapping(_, _, CompiledMapping(_, _)) :@ CollectionType(cons, el) =>
      val b = cons.createBuilder(el.classTag).asInstanceOf[mutable.Builder[Any, R]]
      createQueryInvoker[Any](rsm, param, null).foreach({ x => b += x })(session)
      b.result()
    case First(rsm: ResultSetMapping)                                                    =>
      createQueryInvoker[R](rsm, param, null).first
  }
}
