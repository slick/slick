package scala.slick.driver

import scala.language.{higherKinds, existentials}
import java.sql.{Statement, PreparedStatement}
import scala.slick.SlickException
import scala.slick.ast.{Insert, CompiledStatement, ResultSetMapping, Node, ParameterSwitch}
import scala.slick.lifted.{CompiledStreamingExecutable, FlatShapeLevel, Query, Shape}
import scala.slick.jdbc._
import scala.slick.util.SQLBuilder
import scala.slick.profile.BasicInvokerComponent
import scala.slick.relational.{ResultConverter, CompiledMapping}

trait JdbcInvokerComponent extends BasicInvokerComponent{ driver: JdbcDriver =>

  type InsertInvoker[T] = CountingInsertInvoker[T]

  def createInsertInvoker[U](tree: CompiledInsert) = createCountingInsertInvoker(tree)

  // Create the different invokers -- these methods should be overridden by drivers as needed
  def createCountingInsertInvoker[U](compiled: CompiledInsert) = new CountingInsertInvoker[U](compiled)
  def createKeysInsertInvoker[U, RU](compiled: CompiledInsert, keys: Node) = new KeysInsertInvoker[U, RU](compiled, keys)
  def createMappedKeysInsertInvoker[U, RU, R](compiled: CompiledInsert, keys: Node, tr: (U, RU) => R) = new MappedKeysInsertInvoker[U, RU, R](compiled, keys, tr)
  def createUpdateInvoker[T](tree: Node, param: Any) = new UpdateInvoker[T](tree, param)
  def createDeleteInvoker(tree: Node, param: Any) = new DeleteInvoker(tree, param)
  def createQueryInvoker[R](tree: Node, param: Any): QueryInvoker[R] = new QueryInvoker[R](tree, param)
  def createDDLInvoker(ddl: SchemaDescription) = new DDLInvoker(ddl)

  // Parameters for invokers -- can be overridden by drivers as needed
  val invokerMutateConcurrency: ResultSetConcurrency = ResultSetConcurrency.Updatable
  val invokerMutateType: ResultSetType = ResultSetType.Auto
  val invokerPreviousAfterDelete = false

  /** An Invoker for queries. */
  class QueryInvoker[R](tree: Node, param: Any) extends MutatingStatementInvoker[R] {
    override protected val mutateConcurrency = invokerMutateConcurrency
    override protected val mutateType = invokerMutateType
    override protected val previousAfterDelete = invokerPreviousAfterDelete

    protected[this] val ResultSetMapping(_, compiled, CompiledMapping(_converter, _)) = tree
    protected[this] val converter = _converter.asInstanceOf[ResultConverter[JdbcResultConverterDomain, R]]
    protected[this] val CompiledStatement(_, sres: SQLBuilder.Result, _) = findCompiledStatement(compiled)

    protected[this] def findCompiledStatement(n: Node): CompiledStatement = n match {
      case c: CompiledStatement => c
      case ParameterSwitch(cases, default) =>
        findCompiledStatement(cases.find { case (f, n) => f(param) }.map(_._2).getOrElse(default))
    }

    protected def getStatement = sres.sql
    protected def setParam(st: PreparedStatement): Unit = sres.setter(st, 1, param)
    protected def extractValue(pr: PositionedResult): R = converter.read(pr.rs)
    protected def updateRowValues(pr: PositionedResult, value: R) = converter.update(value, pr.rs)
    def invoker: this.type = this
  }

  class DDLInvoker(ddl: DDL) extends super.DDLInvoker {
    def create(implicit session: Backend#Session): Unit = session.withTransaction {
      for(s <- ddl.createStatements)
        session.withPreparedStatement(s)(_.execute)
    }

    def drop(implicit session: Backend#Session): Unit = session.withTransaction {
      for(s <- ddl.dropStatements)
        session.withPreparedStatement(s)(_.execute)
    }
  }

  /** Pseudo-invoker for running DELETE calls. */
  class DeleteInvoker(protected val tree: Node, param: Any) {
    protected[this] val ResultSetMapping(_, CompiledStatement(_, sres: SQLBuilder.Result, _), _) = tree

    def deleteStatement = sres.sql

    def delete(implicit session: Backend#Session): Int = session.withPreparedStatement(deleteStatement) { st =>
      sres.setter(st, 1, param)
      st.executeUpdate
    }

    def deleteInvoker: this.type = this
  }

  /** Pseudo-invoker for running INSERT calls. */
  abstract class BaseInsertInvoker[U](protected val compiled: CompiledInsert) extends InsertInvokerDef[U] {

    protected def retOne(st: Statement, value: U, updateCount: Int): SingleInsertResult
    protected def retMany(values: Seq[U], individual: Seq[SingleInsertResult]): MultiInsertResult
    protected def retManyBatch(st: Statement, values: Seq[U], updateCounts: Array[Int]): MultiInsertResult

    lazy val insertStatement = compiled.standardInsertBuilderResult.sql
    lazy val forceInsertStatement = compiled.forceInsertBuilderResult.sql

    def insertStatementFor[TT, C[_]](query: Query[TT, U, C]): String = buildSubquery(query).sql
    def insertStatementFor[TT, C[_]](compiledQuery: CompiledStreamingExecutable[Query[TT, U, C], _, _]): String = buildSubquery(compiledQuery).sql
    def insertStatementFor[TT](c: TT)(implicit shape: Shape[_ <: FlatShapeLevel, TT, U, _]): String = insertStatementFor(Query(c)(shape))

    def useBatchUpdates(implicit session: Backend#Session) = session.capabilities.supportsBatchUpdates

    protected def buildSubquery[TT, C[_]](query: Query[TT, U, C]): SQLBuilder.Result =
      compiled.standardInsertBuilderResult.buildInsert(queryCompiler.run(query.toNode).tree)

    protected def buildSubquery[TT, C[_]](compiledQuery: CompiledStreamingExecutable[Query[TT, U, C], _, _]): SQLBuilder.Result =
      compiled.standardInsertBuilderResult.buildInsert(compiledQuery.compiledQuery)

    protected def prepared[T](sql: String)(f: PreparedStatement => T)(implicit session: Backend#Session) =
      session.withPreparedStatement(sql)(f)

    /** Insert a single row, skipping AutoInc columns. */
    final def insert(value: U)(implicit session: Backend#Session): SingleInsertResult = internalInsert(false, value)

    /** Insert a single row, including AutoInc columns. This is not supported
      * by all database engines (see
      * [[scala.slick.driver.JdbcProfile.capabilities.forceInsert]]). */
    final def forceInsert(value: U)(implicit session: Backend#Session): SingleInsertResult = internalInsert(true, value)

    protected def internalInsert(forced: Boolean, value: U)(implicit session: Backend#Session): SingleInsertResult =
      prepared(if(forced) forceInsertStatement else insertStatement) { st =>
        st.clearParameters()
        (if(forced) compiled.forceInsertConverter else compiled.standardInsertConverter).set(value, st, forced)
        val count = st.executeUpdate()
        retOne(st, value, count)
      }

    /** Insert multiple rows, skipping AutoInc columns.
      * Uses JDBC's batch update feature if supported by the JDBC driver.
      * Returns Some(rowsAffected), or None if the database returned no row
      * count for some part of the batch. If any part of the batch fails, an
      * exception is thrown. */
    final def insertAll(values: U*)(implicit session: Backend#Session): MultiInsertResult = internalInsertAll(false, values: _*)

    /** Insert multiple rows, including AutoInc columns.
      * This is not supported by all database engines (see
      * [[scala.slick.driver.JdbcProfile.capabilities.forceInsert]]).
      * Uses JDBC's batch update feature if supported by the JDBC driver.
      * Returns Some(rowsAffected), or None if the database returned no row
      * count for some part of the batch. If any part of the batch fails, an
      * exception is thrown. */
    final def forceInsertAll(values: U*)(implicit session: Backend#Session): MultiInsertResult = internalInsertAll(true, values: _*)

    protected def internalInsertAll(forced: Boolean, values: U*)(implicit session: Backend#Session): MultiInsertResult = session.withTransaction {
      if(!useBatchUpdates || (values.isInstanceOf[IndexedSeq[_]] && values.length < 2)) {
        retMany(values, values.map(insert))
      } else {
        prepared(if(forced) forceInsertStatement else insertStatement) { st =>
          st.clearParameters()
          for(value <- values) {
            (if(forced) compiled.forceInsertConverter else compiled.standardInsertConverter).set(value, st, forced)
            st.addBatch()
          }
          val counts = st.executeBatch()
          retManyBatch(st, values, counts)
        }
      }
    }

    def += (value: U)(implicit session: Backend#Session): SingleInsertResult = insert(value)
    def ++= (values: Iterable[U])(implicit session: Backend#Session): MultiInsertResult = insertAll(values.toSeq: _*)
  }

  /** An InsertInvoker that can also insert from another query. */
  trait FullInsertInvoker[U] { this: BaseInsertInvoker[U] =>
    type QueryInsertResult

    protected def retQuery(st: Statement, updateCount: Int): QueryInsertResult

    def insertExpr[TT](c: TT)(implicit shape: Shape[_ <: FlatShapeLevel, TT, U, _], session: Backend#Session): QueryInsertResult =
      insert(Query(c)(shape))(session)

    def insert[TT, C[_]](query: Query[TT, U, C])(implicit session: Backend#Session): QueryInsertResult =
      internalInsertQuery(buildSubquery(query))

    def insert[TT, C[_]](compiledQuery: CompiledStreamingExecutable[Query[TT, U, C], _, _])(implicit session: Backend#Session): QueryInsertResult =
      internalInsertQuery(buildSubquery(compiledQuery))

    protected def internalInsertQuery(sbr: SQLBuilder.Result)(implicit session: Backend#Session): QueryInsertResult = {
      prepared(sbr.sql) { st =>
        st.clearParameters()
        sbr.setter(st, 1, null)
        val count = st.executeUpdate()
        retQuery(st, count)
      }
    }
  }

  /** Pseudo-invoker for running INSERT calls and returning affected row counts. */
  class CountingInsertInvoker[U](compiled: CompiledInsert) extends BaseInsertInvoker[U](compiled) with FullInsertInvoker[U] {
    type SingleInsertResult = Int
    type MultiInsertResult = Option[Int]
    type QueryInsertResult = Int

    protected def retOne(st: Statement, value: U, updateCount: Int) = updateCount

    protected def retMany(values: Seq[U], individual: Seq[SingleInsertResult]) = Some(individual.sum)

    protected def retManyBatch(st: Statement, values: Seq[U], updateCounts: Array[Int]) = {
      var unknown = false
      var count = 0
      for((res, idx) <- updateCounts.zipWithIndex) res match {
        case Statement.SUCCESS_NO_INFO => unknown = true
        case Statement.EXECUTE_FAILED =>
          throw new SlickException("Failed to insert row #" + (idx+1))
        case i => count += i
      }
      if(unknown) None else Some(count)
    }

    protected def retQuery(st: Statement, updateCount: Int) = updateCount

    def returning[RT, RU, C[_]](value: Query[RT, RU, C]) =
      createKeysInsertInvoker[U, RU](compiled, value.toNode)
  }

  /** Base class with common functionality for KeysInsertInvoker and MappedKeysInsertInvoker. */
  abstract class AbstractKeysInsertInvoker[U, RU](compiled: CompiledInsert, keys: Node)
    extends BaseInsertInvoker[U](compiled) {

    protected def buildKeysResult(st: Statement): Invoker[RU] =
      ResultSetInvoker[RU](_ => st.getGeneratedKeys)(pr => keyConverter.read(pr.rs).asInstanceOf[RU])

    // Returning keys from batch inserts is generally not supported
    override def useBatchUpdates(implicit session: Backend#Session) = false

    protected lazy val (keyColumns, keyConverter) = compiled.buildReturnColumns(keys)

    override protected def prepared[T](sql: String)(f: PreparedStatement => T)(implicit session: Backend#Session) =
      session.withPreparedInsertStatement(sql, keyColumns.toArray)(f)
  }

  /** Pseudo-invoker for running INSERT calls and returning generated keys. */
  class KeysInsertInvoker[U, RU](compiled: CompiledInsert, keys: Node)
    extends AbstractKeysInsertInvoker[U, RU](compiled, keys) with FullInsertInvoker[U] {

    type SingleInsertResult = RU
    type MultiInsertResult = Seq[RU]
    type QueryInsertResult = MultiInsertResult

    protected def retOne(st: Statement, value: U, updateCount: Int) =
      buildKeysResult(st).first(null)

    protected def retMany(values: Seq[U], individual: Seq[SingleInsertResult]) = individual

    protected def retManyBatch(st: Statement, values: Seq[U], updateCounts: Array[Int]) = {
      implicit val session: Backend#Session = null
      buildKeysResult(st).buildColl[Vector]
    }

    protected def retQuery(st: Statement, updateCount: Int) = {
      implicit val session: Backend#Session = null
      buildKeysResult(st).buildColl[Vector]
    }
    /**
      * Specifies a mapping from inserted values and generated keys to a desired value.
      * @param f Function that maps inserted values and generated keys to a desired value.
      * @tparam R target type of the mapping
      */
    def into[R](f: (U, RU) => R) = createMappedKeysInsertInvoker[U, RU, R](compiled, keys, f)
  }

  /** Pseudo-invoker for running INSERT calls and returning generated keys combined with the values. */
  class MappedKeysInsertInvoker[U, RU, R](compiled: CompiledInsert, keys: Node, tr: (U, RU) => R)
    extends AbstractKeysInsertInvoker[U, RU](compiled, keys) {

    type SingleInsertResult = R
    type MultiInsertResult = Seq[R]

    protected def retOne(st: Statement, value: U, updateCount: Int) = {
      val ru = buildKeysResult(st).first(null)
      tr(value, ru)
    }

    protected def retMany(values: Seq[U], individual: Seq[SingleInsertResult]) = individual

    protected def retManyBatch(st: Statement, values: Seq[U], updateCounts: Array[Int]) = {
      implicit val session: Backend#Session = null
      val ru = buildKeysResult(st).buildColl[Vector]
      (values, ru).zipped.map(tr)
    }
  }

  /** Pseudo-invoker for running UPDATE calls. */
  class UpdateInvoker[T](protected val tree: Node, param: Any) {
    protected[this] val ResultSetMapping(_,
      CompiledStatement(_, sres: SQLBuilder.Result, _),
      CompiledMapping(_converter, _)) = tree
    protected[this] val converter = _converter.asInstanceOf[ResultConverter[JdbcResultConverterDomain, T]]

    def updateStatement = getStatement

    protected def getStatement = sres.sql

    def update(value: T)(implicit session: Backend#Session): Int = session.withPreparedStatement(updateStatement) { st =>
      st.clearParameters
      converter.set(value, st, true)
      sres.setter(st, converter.width+1, param)
      st.executeUpdate
    }

    def updateInvoker: this.type = this
  }
}
