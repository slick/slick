package scala.slick.driver

import scala.language.{higherKinds, existentials}
import java.sql.{Statement, PreparedStatement}
import scala.slick.SlickException
import scala.slick.ast.{Insert, CompiledStatement, ResultSetMapping, Node}
import scala.slick.lifted.{CompiledStreamingExecutable, FlatShapeLevel, Query, Shape}
import scala.slick.jdbc._
import scala.slick.util.SQLBuilder
import scala.slick.profile.BasicInsertInvokerComponent
import scala.slick.relational.{ResultConverter, CompiledMapping}

/** A slice of the `JdbcProfile` cake which provides the functionality for
  * different kinds of insert operations. */
trait JdbcInsertInvokerComponent extends BasicInsertInvokerComponent{ driver: JdbcDriver =>
  type InsertInvoker[T] = CountingInsertInvokerDef[T]

  def createInsertInvoker[U](tree: CompiledInsert): CountingInsertInvokerDef[U] = createCountingInsertInvoker(tree)

  // Create the different invokers -- these methods should be overridden by drivers as needed
  def createCountingInsertInvoker[U](compiled: CompiledInsert): CountingInsertInvokerDef[U] = new CountingInsertInvoker[U](compiled)
  def createReturningInsertInvoker[U, RU](compiled: CompiledInsert, keys: Node): ReturningInsertInvokerDef[U, RU] = new ReturningInsertInvoker[U, RU](compiled, keys)

  protected lazy val useServerSideUpsert = capabilities contains JdbcProfile.capabilities.insertOrUpdate
  protected lazy val useTransactionForUpsert = !useServerSideUpsert
  protected lazy val useServerSideUpsertReturning = useServerSideUpsert
  protected lazy val useTransactionForUpsertReturning = !useServerSideUpsertReturning

  //////////////////////////////////////////////////////////// InsertInvokerDef Traits

  /** The JDBC-specific InsertInvoker with additional methods */
  trait InsertInvokerDef[U] extends super.InsertInvokerDef[U] {
    /** The return type for `insertOrUpdate` operations */
    type SingleInsertOrUpdateResult

    /** Get the SQL statement for a standard (soft) insert */
    def insertStatement: String

    /** Get the SQL statement for a forced insert */
    def forceInsertStatement: String

    /** Insert a single row, skipping AutoInc columns. */
    def insert(value: U)(implicit session: Backend#Session): SingleInsertResult

    /** Insert a single row, including AutoInc columns. This is not supported
      * by all database engines (see
      * [[scala.slick.driver.JdbcProfile.capabilities.forceInsert]]). */
    def forceInsert(value: U)(implicit session: Backend#Session): SingleInsertResult

    /** Insert multiple rows, skipping AutoInc columns.
      * Uses JDBC's batch update feature if supported by the JDBC driver.
      * Returns Some(rowsAffected), or None if the database returned no row
      * count for some part of the batch. If any part of the batch fails, an
      * exception is thrown. */
    def insertAll(values: U*)(implicit session: Backend#Session): MultiInsertResult

    /** Insert multiple rows, including AutoInc columns.
      * This is not supported by all database engines (see
      * [[scala.slick.driver.JdbcProfile.capabilities.forceInsert]]).
      * Uses JDBC's batch update feature if supported by the JDBC driver.
      * Returns Some(rowsAffected), or None if the database returned no row
      * count for some part of the batch. If any part of the batch fails, an
      * exception is thrown. */
    def forceInsertAll(values: U*)(implicit session: Backend#Session): MultiInsertResult

    /** Insert a single row if its primary key does not exist in the table,
      * otherwise update the existing record. */
    def insertOrUpdate(value: U)(implicit session: Backend#Session): SingleInsertOrUpdateResult

    final def += (value: U)(implicit session: Backend#Session): SingleInsertResult = insert(value)
    final def ++= (values: Iterable[U])(implicit session: Backend#Session): MultiInsertResult = insertAll(values.toSeq: _*)
  }

  /** An InsertInvoker that can also insert from another query. This is supported for
    * inserts which return the row count or the generated keys but not mappings which
    * involve the original data that has been inserted (because it is not available on
    * the client side). */
  trait FullInsertInvokerDef[U] extends InsertInvokerDef[U] {
    /** The result type of operations that insert data produced by another query */
    type QueryInsertResult

    /** Get the SQL statement for inserting a single row from a scalar expression */
    def insertStatementFor[TT](c: TT)(implicit shape: Shape[_ <: FlatShapeLevel, TT, U, _]): String

    /** Get the SQL statement for inserting data produced by another query */
    def insertStatementFor[TT, C[_]](query: Query[TT, U, C]): String

    /** Get the SQL statement for inserting data produced by another query */
    def insertStatementFor[TT, C[_]](compiledQuery: CompiledStreamingExecutable[Query[TT, U, C], _, _]): String

    /** Insert a single row from a scakar expression */
    def insertExpr[TT](c: TT)(implicit shape: Shape[_ <: FlatShapeLevel, TT, U, _], session: Backend#Session): QueryInsertResult

    /** Insert data produced by another query */
    def insert[TT, C[_]](query: Query[TT, U, C])(implicit session: Backend#Session): QueryInsertResult

    /** Insert data produced by another query */
    def insert[TT, C[_]](compiledQuery: CompiledStreamingExecutable[Query[TT, U, C], _, _])(implicit session: Backend#Session): QueryInsertResult
  }

  /** An InsertInvoker that returns the number of affected rows. */
  trait CountingInsertInvokerDef[U] extends FullInsertInvokerDef[U] {
    type SingleInsertResult = Int
    type MultiInsertResult = Option[Int]
    type SingleInsertOrUpdateResult = Int
    type QueryInsertResult = Int

    /** Add a mapping from the inserted values and the generated key to compute a new return value. */
    def returning[RT, RU, C[_]](value: Query[RT, RU, C]): ReturningInsertInvokerDef[U, RU]
  }

  /** An InsertInvoker that returns generated keys or other columns. */
  trait ReturningInsertInvokerDef[U, RU] extends FullInsertInvokerDef[U] { self =>
    type SingleInsertResult = RU
    type MultiInsertResult = Seq[RU]
    type SingleInsertOrUpdateResult = Option[RU]
    type QueryInsertResult = Seq[RU]

    /** Specifies a mapping from inserted values and generated keys to a desired value.
      * @param f Function that maps inserted values and generated keys to a desired value.
      * @tparam R target type of the mapping */
    def into[R](f: (U, RU) => R): IntoInsertInvokerDef[U, R] = new IntoInsertInvokerDef[U, R] {
      def forceInsert(value: U)(implicit session: Backend#Session): R = f(value, self.forceInsert(value))
      def forceInsertAll(values: U*)(implicit session: Backend#Session): Seq[R] = (values, self.forceInsertAll(values: _*)).zipped.map(f)
      def forceInsertStatement: String = self.forceInsertStatement
      def insert(value: U)(implicit session: Backend#Session): R = f(value, self.insert(value))
      def insertAll(values: U*)(implicit session: Backend#Session): Seq[R] = (values, self.insertAll(values: _*)).zipped.map(f)
      def insertOrUpdate(value: U)(implicit session: Backend#Session): Option[R] = self.insertOrUpdate(value).map(ru => f(value, ru))
      def insertStatement: String = self.insertStatement
      def insertStatementFor[TT, C[_]](query: Query[TT, U, C]): String = self.insertStatementFor[TT, C](query)
      def insertStatementFor[TT, C[_]](compiledQuery: CompiledStreamingExecutable[Query[TT, U, C], _, _]): String = self.insertStatementFor[TT, C](compiledQuery)
      def insertStatementFor[TT](c: TT)(implicit shape: Shape[_ <: FlatShapeLevel, TT, U, _]): String = self.insertStatementFor[TT](c)(shape)
    }
  }

  /** An InsertInvoker that returns a mapping of inserted values and generated keys. */
  trait IntoInsertInvokerDef[U, R] extends InsertInvokerDef[U] {
    type SingleInsertResult = R
    type MultiInsertResult = Seq[R]
    type SingleInsertOrUpdateResult = Option[R]
  }

  //////////////////////////////////////////////////////////// InsertInvoker Implementations

  protected abstract class BaseInsertInvoker[U](protected val compiled: CompiledInsert) extends FullInsertInvokerDef[U] {
    protected def useServerSideUpsert = driver.useServerSideUpsert
    protected def useTransactionForUpsert = driver.useTransactionForUpsert
    protected def useBatchUpdates(implicit session: Backend#Session) = session.capabilities.supportsBatchUpdates

    protected def retOne(st: Statement, value: U, updateCount: Int): SingleInsertResult
    protected def retMany(values: Seq[U], individual: Seq[SingleInsertResult]): MultiInsertResult
    protected def retManyBatch(st: Statement, values: Seq[U], updateCounts: Array[Int]): MultiInsertResult
    protected def retOneInsertOrUpdate(st: Statement, value: U, updateCount: Int): SingleInsertOrUpdateResult
    protected def retOneInsertOrUpdateFromInsert(st: Statement, value: U, updateCount: Int): SingleInsertOrUpdateResult
    protected def retOneInsertOrUpdateFromUpdate: SingleInsertOrUpdateResult

    lazy val insertStatement = compiled.standardInsert.sql
    lazy val forceInsertStatement = compiled.forceInsert.sql
    def insertStatementFor[TT, C[_]](query: Query[TT, U, C]): String = buildSubquery(query).sql
    def insertStatementFor[TT, C[_]](compiledQuery: CompiledStreamingExecutable[Query[TT, U, C], _, _]): String = buildSubquery(compiledQuery).sql
    def insertStatementFor[TT](c: TT)(implicit shape: Shape[_ <: FlatShapeLevel, TT, U, _]): String = insertStatementFor(Query(c)(shape))

    protected def buildSubquery[TT, C[_]](query: Query[TT, U, C]): SQLBuilder.Result =
      compiled.standardInsert.ibr.buildInsert(queryCompiler.run(query.toNode).tree)

    protected def buildSubquery[TT, C[_]](compiledQuery: CompiledStreamingExecutable[Query[TT, U, C], _, _]): SQLBuilder.Result =
      compiled.standardInsert.ibr.buildInsert(compiledQuery.compiledQuery)

    protected def preparedInsert[T](sql: String)(f: PreparedStatement => T)(implicit session: Backend#Session) =
      session.withPreparedStatement(sql)(f)

    protected def preparedOther[T](sql: String)(f: PreparedStatement => T)(implicit session: Backend#Session) =
      session.withPreparedStatement(sql)(f)

    final def insert(value: U)(implicit session: Backend#Session): SingleInsertResult = internalInsert(compiled.standardInsert, value)

    final def forceInsert(value: U)(implicit session: Backend#Session): SingleInsertResult = internalInsert(compiled.forceInsert, value)

    protected def internalInsert(a: compiled.Artifacts, value: U)(implicit session: Backend#Session): SingleInsertResult =
      preparedInsert(a.sql) { st =>
        st.clearParameters()
        a.converter.set(value, st)
        val count = st.executeUpdate()
        retOne(st, value, count)
      }

    final def insertAll(values: U*)(implicit session: Backend#Session): MultiInsertResult = internalInsertAll(compiled.standardInsert, values: _*)

    final def forceInsertAll(values: U*)(implicit session: Backend#Session): MultiInsertResult = internalInsertAll(compiled.forceInsert, values: _*)

    protected def internalInsertAll(a: compiled.Artifacts, values: U*)(implicit session: Backend#Session): MultiInsertResult = session.withTransaction {
      if(!useBatchUpdates || (values.isInstanceOf[IndexedSeq[_]] && values.length < 2))
        retMany(values, values.map(v => internalInsert(a, v)))
      else preparedInsert(a.sql) { st =>
        st.clearParameters()
        for(value <- values) {
          a.converter.set(value, st)
          st.addBatch()
        }
        val counts = st.executeBatch()
        retManyBatch(st, values, counts)
      }
    }

    final def insertOrUpdate(value: U)(implicit session: Backend#Session): SingleInsertOrUpdateResult = {
      def f(): SingleInsertOrUpdateResult = {
        if(useServerSideUpsert) {
          preparedInsert(compiled.upsert.sql) { st =>
            st.clearParameters()
            compiled.upsert.converter.set(value, st)
            val count = st.executeUpdate()
            retOneInsertOrUpdate(st, value, count)
          }
        } else internalInsertOrUpdateEmulation(value)
      }
      if(useTransactionForUpsert) session.withTransaction(f()) else f()
    }

    protected def internalInsertOrUpdateEmulation(value: U)(implicit session: Backend#Session): SingleInsertOrUpdateResult = {
      val found = preparedOther(compiled.checkInsert.sql) { st =>
        st.clearParameters()
        compiled.checkInsert.converter.set(value, st)
        val rs = st.executeQuery()
        try rs.next() finally rs.close()
      }
      if(found) preparedOther(compiled.updateInsert.sql) { st =>
        st.clearParameters()
        compiled.updateInsert.converter.set(value, st)
        st.executeUpdate()
        retOneInsertOrUpdateFromUpdate
      } else preparedInsert(compiled.standardInsert.sql) { st =>
        st.clearParameters()
        compiled.standardInsert.converter.set(value, st)
        val count = st.executeUpdate()
        retOneInsertOrUpdateFromInsert(st, value, count)
      }
    }

    protected def retQuery(st: Statement, updateCount: Int): QueryInsertResult

    def insertExpr[TT](c: TT)(implicit shape: Shape[_ <: FlatShapeLevel, TT, U, _], session: Backend#Session): QueryInsertResult =
      insert(Query(c)(shape))(session)

    def insert[TT, C[_]](query: Query[TT, U, C])(implicit session: Backend#Session): QueryInsertResult =
      internalInsertQuery(buildSubquery(query), null)

    def insert[TT, C[_]](compiledQuery: CompiledStreamingExecutable[Query[TT, U, C], _, _])(implicit session: Backend#Session): QueryInsertResult =
      internalInsertQuery(buildSubquery(compiledQuery), compiledQuery.param)

    protected def internalInsertQuery(sbr: SQLBuilder.Result, param: Any)(implicit session: Backend#Session): QueryInsertResult = {
      preparedInsert(sbr.sql) { st =>
        st.clearParameters()
        sbr.setter(st, 1, param)
        val count = st.executeUpdate()
        retQuery(st, count)
      }
    }
  }

  protected class CountingInsertInvoker[U](compiled: CompiledInsert) extends BaseInsertInvoker[U](compiled) with CountingInsertInvokerDef[U] {
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

    protected def retOneInsertOrUpdate(st: Statement, value: U, updateCount: Int) = 1
    protected def retOneInsertOrUpdateFromInsert(st: Statement, value: U, updateCount: Int) = 1
    protected def retOneInsertOrUpdateFromUpdate = 1

    def returning[RT, RU, C[_]](value: Query[RT, RU, C]) = createReturningInsertInvoker[U, RU](compiled, value.toNode)
  }

  protected class ReturningInsertInvoker[U, RU](compiled: CompiledInsert, keys: Node) extends BaseInsertInvoker[U](compiled) with ReturningInsertInvokerDef[U, RU] {
    override protected def useServerSideUpsert = driver.useServerSideUpsertReturning
    override protected def useTransactionForUpsert = driver.useTransactionForUpsertReturning

    protected def checkInsertOrUpdateKeys: Unit =
      if(keyReturnOther) throw new SlickException("Only a single AutoInc column may be returned from an insertOrUpdate call")

    protected def buildKeysResult(st: Statement): Invoker[RU] =
      ResultSetInvoker[RU](_ => st.getGeneratedKeys)(pr => keyConverter.read(pr.rs).asInstanceOf[RU])

    // Returning keys from batch inserts is generally not supported
    override def useBatchUpdates(implicit session: Backend#Session) = false

    protected lazy val (keyColumns, keyConverter, keyReturnOther) = compiled.buildReturnColumns(keys)

    override protected def preparedInsert[T](sql: String)(f: PreparedStatement => T)(implicit session: Backend#Session) =
      session.withPreparedInsertStatement(sql, keyColumns.toArray)(f)

    protected def retOne(st: Statement, value: U, updateCount: Int) =
      buildKeysResult(st).first(null)

    protected def retMany(values: Seq[U], individual: Seq[SingleInsertResult]) = individual

    protected def retManyBatch(st: Statement, values: Seq[U], updateCounts: Array[Int]) =
      buildKeysResult(st).buildColl[Vector](null, implicitly)

    protected def retQuery(st: Statement, updateCount: Int) =
      buildKeysResult(st).buildColl[Vector](null, implicitly)

    protected def retOneInsertOrUpdate(st: Statement, value: U, updateCount: Int): SingleInsertOrUpdateResult =
      if(updateCount != 1) None else buildKeysResult(st).firstOption(null)

    protected def retOneInsertOrUpdateFromInsert(st: Statement, value: U, updateCount: Int): SingleInsertOrUpdateResult =
      Some(buildKeysResult(st).first(null))

    protected def retOneInsertOrUpdateFromUpdate: SingleInsertOrUpdateResult = None
  }
}
