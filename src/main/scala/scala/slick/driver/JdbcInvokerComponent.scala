package scala.slick.driver

import java.sql.{Statement, PreparedStatement}
import scala.slick.SlickException
import scala.slick.ast.{Insert, CompiledStatement, ResultSetMapping, Node}
import scala.slick.lifted.{ShapeLevel, Query, Shape}
import scala.slick.jdbc._
import scala.slick.util.SQLBuilder
import scala.slick.profile.BasicInvokerComponent

trait JdbcInvokerComponent extends BasicInvokerComponent{ driver: JdbcDriver =>

  type InsertInvoker[T] = CountingInsertInvoker[T]

  def createInsertInvoker[U](tree: Node) = createCountingInsertInvoker(tree)

  // Create the different invokers -- these methods should be overridden by drivers as needed
  def createCountingInsertInvoker[U](tree: Node) = new CountingInsertInvoker[U](tree)
  def createKeysInsertInvoker[U, RU](tree: Node, keys: Node) = new KeysInsertInvoker[U, RU](tree, keys)
  def createMappedKeysInsertInvoker[U, RU, R](tree: Node, keys: Node, tr: (U, RU) => R) = new MappedKeysInsertInvoker[U, RU, R](tree, keys, tr)
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

    protected[this] val ResultSetMapping(_,
      CompiledStatement(_, sres: SQLBuilder.Result, _),
      CompiledMapping(converter, _)) = tree

    protected def getStatement = sres.sql
    protected def setParam(st: PreparedStatement): Unit = sres.setter(new PositionedParameters(st), param)
    protected def extractValue(pr: PositionedResult): R = converter.read(pr).asInstanceOf[R]
    protected def updateRowValues(pr: PositionedResult, value: R) = converter.update(value, pr)
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
      sres.setter(new PositionedParameters(st), param)
      st.executeUpdate
    }

    def deleteInvoker: this.type = this
  }

  /** Pseudo-invoker for running INSERT calls. */
  abstract class BaseInsertInvoker[U](tree: Node) extends InsertInvokerDef[U] {

    protected[this] val ResultSetMapping(_, insertNode: Insert, CompiledMapping(converter, _)) = tree
    protected[this] lazy val builder = createInsertBuilder(insertNode)

    protected def retOne(st: Statement, value: U, updateCount: Int): SingleInsertResult
    protected def retMany(values: Seq[U], individual: Seq[SingleInsertResult]): MultiInsertResult
    protected def retManyBatch(st: Statement, values: Seq[U], updateCounts: Array[Int]): MultiInsertResult

    protected lazy val insertResult = builder.buildInsert(forced = false)
    protected lazy val insertForcedResult = builder.buildInsert(forced = true)
    lazy val insertStatement = insertResult.sql
    lazy val forceInsertStatement = insertForcedResult.sql
    def insertStatementFor[TT](query: Query[TT, U]): String = builder.buildInsert(query).sql
    def insertStatementFor[TT](c: TT)(implicit shape: Shape[_ <: ShapeLevel.Flat, TT, U, _]): String = insertStatementFor(Query(c)(shape))

    def useBatchUpdates(implicit session: Backend#Session) = session.capabilities.supportsBatchUpdates

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
        converter.set(value, new PositionedParameters(st), forced)
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
            converter.set(value, new PositionedParameters(st), forced)
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

    def insertExpr[TT](c: TT)(implicit shape: Shape[_ <: ShapeLevel.Flat, TT, U, _], session: Backend#Session): QueryInsertResult =
      insert(Query(c)(shape))(session)

    def insert[TT](query: Query[TT, U])(implicit session: Backend#Session): QueryInsertResult = {
      val sbr = builder.buildInsert(query)
      prepared(insertStatementFor(query)) { st =>
        st.clearParameters()
        sbr.setter(new PositionedParameters(st), null)
        val count = st.executeUpdate()
        retQuery(st, count)
      }
    }
  }

  /** Pseudo-invoker for running INSERT calls and returning affected row counts. */
  class CountingInsertInvoker[U](tree: Node) extends BaseInsertInvoker[U](tree) with FullInsertInvoker[U] {
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

    def returning[RT, RU](value: Query[RT, RU]) =
      createKeysInsertInvoker[U, RU](tree, value.toNode)
  }

  /** Base class with common functionality for KeysInsertInvoker and MappedKeysInsertInvoker. */
  abstract class AbstractKeysInsertInvoker[U, RU](tree: Node, keys: Node)
    extends BaseInsertInvoker[U](tree) {

    protected def buildKeysResult(st: Statement): Invoker[RU] =
      ResultSetInvoker[RU](_ => st.getGeneratedKeys)(pr => keyConverter.read(pr).asInstanceOf[RU])

    // Returning keys from batch inserts is generally not supported
    override def useBatchUpdates(implicit session: Backend#Session) = false

    protected lazy val (keyColumns, keyConverter) =
      builder.buildReturnColumns(keys, insertResult.table)

    override protected def prepared[T](sql: String)(f: PreparedStatement => T)(implicit session: Backend#Session) =
      session.withPreparedInsertStatement(sql, keyColumns.toArray)(f)
  }

  /** Pseudo-invoker for running INSERT calls and returning generated keys. */
  class KeysInsertInvoker[U, RU](tree: Node, keys: Node)
    extends AbstractKeysInsertInvoker[U, RU](tree, keys) with FullInsertInvoker[U] {

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

    def into[R](f: (U, RU) => R) = createMappedKeysInsertInvoker[U, RU, R](tree, keys, f)
  }

  /** Pseudo-invoker for running INSERT calls and returning generated keys combined with the values. */
  class MappedKeysInsertInvoker[U, RU, R](tree: Node, keys: Node, tr: (U, RU) => R)
    extends AbstractKeysInsertInvoker[U, RU](tree, keys) {

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
      CompiledMapping(converter, _)) = tree

    def updateStatement = getStatement

    protected def getStatement = sres.sql

    def update(value: T)(implicit session: Backend#Session): Int = session.withPreparedStatement(updateStatement) { st =>
      st.clearParameters
      val pp = new PositionedParameters(st)
      converter.set(value, pp, true)
      sres.setter(pp, param)
      st.executeUpdate
    }

    def updateInvoker: this.type = this
  }
}
