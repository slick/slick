package scala.slick.driver

import scala.language.{existentials, higherKinds}

import java.sql.{PreparedStatement, Statement}

import scala.collection.mutable.Builder
import scala.concurrent.Future
import scala.util.Try
import scala.util.control.NonFatal

import scala.slick.SlickException
import scala.slick.action._
import scala.slick.ast._
import scala.slick.ast.Util._
import scala.slick.ast.TypeUtil.:@
import scala.slick.backend.DatabaseComponent
import scala.slick.jdbc.{JdbcResultConverterDomain, ResultSetInvoker, Invoker}
import scala.slick.lifted.{CompiledStreamingExecutable, Query, FlatShapeLevel, Shape}
import scala.slick.profile.SqlActionComponent
import scala.slick.relational.{ResultConverter, CompiledMapping}
import scala.slick.util.{CloseableIterator, DumpInfo, SQLBuilder}

trait JdbcActionComponent extends SqlActionComponent { driver: JdbcDriver =>

  type StreamingDriverAction[-E <: Effect, +R, +S <: NoStream] = SqlAction[E, R, S]
  type StreamingJdbcDatabaseAction[+R, +S <: NoStream] = SynchronousDatabaseAction[Backend#This, Effect, R, S]
  type JdbcDatabaseAction[+R] = StreamingJdbcDatabaseAction[R, NoStream]

  class JdbcActionExtensionMethods[E <: Effect, R, S <: NoStream](a: Action[E, R, S]) {

    /** Run this Action transactionally. This does not guarantee failures to be atomic in the
      * presence of error handling combinators. If multiple `transactionally` combinators are
      * nested, only the outermost one will be backed by an actual database transaction. Depending
      * on the outcome of running the Action it surrounds, the transaction is committed if the
      * wrapped Action succeeds, or rolled back if the wrapped Action fails. When called on a
      * [[scala.slick.action.SynchronousDatabaseAction]], this combinator gets fused into the
      * action. */
    def transactionally: Action[E with Effect.BackendType[Backend#This] with Effect.Transactional, R, S] = {
      def nonFused =
        StartTransaction.andThen(a.asTry.flatMap(new EndTransaction(_))(Action.sameThreadExecutionContext))
          .asInstanceOf[Action[E with Effect.Transactional, R, S]]
      a match {
        case a: SynchronousDatabaseAction[_, _, _, _] => new SynchronousDatabaseAction.Fused[Backend#This, E with Effect.Transactional, R, S] {
          def run(context: ActionContext[Backend#This]): R = {
            context.pin
            context.session.startInTransaction
            val res = try {
              a.asInstanceOf[SynchronousDatabaseAction[Backend#This, E, R, S]].run(context)
            } catch {
              case NonFatal(ex) =>
                try context.session.conn.rollback()
                finally {
                  try context.session.endInTransaction finally context.unpin
                }
                throw ex
            }
            try context.session.conn.commit()
            finally {
              try context.session.endInTransaction finally context.unpin
            }
            res
          }
          override def nonFusedEquivalentAction = nonFused
        }
        case a => nonFused
      }
    }

    protected object StartTransaction extends JdbcDatabaseAction[Unit] {
      def run(context: ActionContext[Backend#This]): Unit = {
        context.pin
        context.session.startInTransaction
      }
      def getDumpInfo = DumpInfo(name = "startTransaction")
    }

    protected class EndTransaction[T](v: Try[T]) extends JdbcDatabaseAction[T] {
      def run(context: ActionContext[Backend#This]): T = {
        try {
          if(v.isSuccess) context.session.conn.commit()
          else context.session.conn.rollback()
        } finally {
          try context.session.endInTransaction finally context.unpin
        }
        v.get
      }
      def getDumpInfo = DumpInfo(name = "endTransaction", attrInfo = (if(v.isSuccess) "commit" else "rollback"))
    }
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////// Query Actions
  ///////////////////////////////////////////////////////////////////////////////////////////////

  type QueryActionExtensionMethods[R, S <: NoStream] = QueryActionExtensionMethodsImpl[R, S]

  def createQueryActionExtensionMethods[R, S <: NoStream](tree: Node, param: Any): QueryActionExtensionMethods[R, S] =
    new QueryActionExtensionMethods[R, S](tree, param)

  class QueryActionExtensionMethodsImpl[R, S <: NoStream](tree: Node, param: Any) extends super.QueryActionExtensionMethodsImpl[R, S] {
    def result: StreamingDriverAction[Effect.Read, R, S] = {
      val sql = tree.findNode(_.isInstanceOf[CompiledStatement]).get
        .asInstanceOf[CompiledStatement].extra.asInstanceOf[SQLBuilder.Result].sql
      tree match {
        case rsm @ ResultSetMapping(_, _, CompiledMapping(_, elemType)) :@ CollectionType(cons, el) =>
          new StreamingDriverAction[Effect.Read, R, S] with StreamingJdbcDatabaseAction[R, S] {
            type StreamState = CloseableIterator[Any]
            def statements = Iterator(sql)
            def run(ctx: ActionContext[Backend]): R = {
              val b = cons.createBuilder(el.classTag).asInstanceOf[Builder[Any, R]]
              createQueryInvoker[Any](rsm, param).foreach({ x => b += x }, 0)(ctx.session)
              b.result()
            }
            override def emitStream(ctx: StreamingActionContext[Backend], limit: Long, state: StreamState): StreamState = {
              val it = if(state ne null) state else createQueryInvoker[Any](rsm, param).iterator(ctx.session)
              var count = 0L
              try {
                while(count < limit && it.hasNext) {
                  count += 1
                  ctx.emit(it.next())
                }
              } catch {
                case NonFatal(ex) =>
                  try it.close() catch { case NonFatal(_) => }
                  throw ex
              }
              if(it.hasNext) it else null
            }
            override def cancelStream(ctx: StreamingActionContext[Backend], state: StreamState): Unit = state.close()
            override def getDumpInfo = super.getDumpInfo.copy(name = "query")
          }
        case First(rsm: ResultSetMapping) =>
          new StreamingDriverAction[Effect.Read, R, S] with StreamingJdbcDatabaseAction[R, S] {
            def statements = Iterator(sql)
            def run(ctx: ActionContext[Backend]): R =
              createQueryInvoker[R](rsm, param).first(ctx.session)
            override def getDumpInfo = super.getDumpInfo.copy(name = "query one")
          }
      }
    }
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////// Delete Actions
  ///////////////////////////////////////////////////////////////////////////////////////////////

  type DeleteActionExtensionMethods = DeleteActionExtensionMethodsImpl

  def createDeleteActionExtensionMethods(tree: Node, param: Any): DeleteActionExtensionMethods =
    new DeleteActionExtensionMethods(tree, param)

  class DeleteActionExtensionMethodsImpl(tree: Node, param: Any) {
    /** An Action that deletes the data selected by this query. */
    def delete: DriverAction[Effect.Write, Int] = {
      val ResultSetMapping(_, CompiledStatement(_, sres: SQLBuilder.Result, _), _) = tree
      new DriverAction[Effect.Write, Int] with JdbcDatabaseAction[Int] {
        def statements = Iterator(sres.sql)
        def run(ctx: ActionContext[Backend]): Int = ctx.session.withPreparedStatement(sres.sql) { st =>
          sres.setter(st, 1, param)
          st.executeUpdate
        }
        override def getDumpInfo = super.getDumpInfo.copy(name = "delete")
      }
    }
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////// Schema Actions
  ///////////////////////////////////////////////////////////////////////////////////////////////

  type SchemaActionExtensionMethods = SchemaActionExtensionMethodsImpl

  def createSchemaActionExtensionMethods(schema: SchemaDescription): SchemaActionExtensionMethods =
    new SchemaActionExtensionMethodsImpl(schema)

  private def mkAction[E <: Effect](dumpInfoKey: String, _statements: Iterator[String])
    = new DriverAction[E, Unit] with JdbcDatabaseAction[Unit] {
      def statements = _statements
      def run(ctx: ActionContext[Backend]): Unit =
        for(s <- statements) ctx.session.withPreparedStatement(s)(_.execute)
      override def getDumpInfo = super.getDumpInfo.copy(name = dumpInfoKey)
    }

  final def truncateTableAction(table: Table[_]): DriverAction[Effect.Write,Unit]
    = mkAction[Effect.Write](
        "write.truncate",
        Seq(createTableDDLBuilder(table).truncateStatement).iterator
      )

  class SchemaActionExtensionMethodsImpl(schema: SchemaDescription) extends super.SchemaActionExtensionMethodsImpl {
    def create: DriverAction[Effect.Schema, Unit]
      = mkAction[Effect.Schema]("schema.create", schema.createStatements)

    def drop: DriverAction[Effect.Schema, Unit]
      = mkAction[Effect.Schema]("schema.drop", schema.dropStatements)
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////// Update Actions
  ///////////////////////////////////////////////////////////////////////////////////////////////

  type UpdateActionExtensionMethods[T] = UpdateActionExtensionMethodsImpl[T]

  def createUpdateActionExtensionMethods[T](tree: Node, param: Any): UpdateActionExtensionMethods[T] =
    new UpdateActionExtensionMethodsImpl[T](tree, param)

  class UpdateActionExtensionMethodsImpl[T](tree: Node, param: Any) {
    /** An Action that updates the data selected by this query. */
    def update(value: T): DriverAction[Effect.Write, Int] = {
      val ResultSetMapping(_,
        CompiledStatement(_, sres: SQLBuilder.Result, _),
        CompiledMapping(_converter, _)) = tree
      val converter = _converter.asInstanceOf[ResultConverter[JdbcResultConverterDomain, T]]
      new DriverAction[Effect.Write, Int] with JdbcDatabaseAction[Int] {
        def statements = Iterator(sres.sql)
        def run(ctx: ActionContext[Backend]): Int = ctx.session.withPreparedStatement(sres.sql) { st =>
          st.clearParameters
          converter.set(value, st)
          sres.setter(st, converter.width+1, param)
          st.executeUpdate
        }
        override def getDumpInfo = super.getDumpInfo.copy(name = "update")
      }
    }
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////// Insert Actions
  ///////////////////////////////////////////////////////////////////////////////////////////////

  type InsertActionExtensionMethods[T] = CountingInsertActionComposerImpl[T]

  def createInsertActionExtensionMethods[T](compiled: CompiledInsert): InsertActionExtensionMethods[T] =
    new CountingInsertActionComposerImpl[T](createInsertInvoker(compiled))

  type SimpleInsertAction[+R] = DatabaseAction[Backend#This, Effect.Write, R, NoStream]

  //////////////////////////////////////////////////////////// InsertActionComposer Traits

  /** Extension methods to generate the JDBC-specific insert actions. */
  trait InsertActionComposer[U] extends super.InsertActionExtensionMethodsImpl[U] {
    /** The return type for `insertOrUpdate` operations */
    type SingleInsertOrUpdateResult

    /** The result type of operations that insert data produced by another query */
    type QueryInsertResult

    /** Get the SQL statement for a standard (soft) insert */
    def insertStatement: String

    /** Get the SQL statement for a forced insert */
    def forceInsertStatement: String

    /** Insert a single row, skipping AutoInc columns. */
    def += (value: U): DriverAction[Effect.Write, SingleInsertResult]

    /** Insert a single row, including AutoInc columns. This is not supported
      * by all database engines (see
      * [[scala.slick.driver.JdbcProfile.capabilities.forceInsert]]). */
    def forceInsert(value: U): DriverAction[Effect.Write, SingleInsertResult]

    /** Insert multiple rows, skipping AutoInc columns.
      * Uses JDBC's batch update feature if supported by the JDBC driver.
      * Returns Some(rowsAffected), or None if the database returned no row
      * count for some part of the batch. If any part of the batch fails, an
      * exception is thrown. */
    def ++= (values: Iterable[U]): DriverAction[Effect.Write, MultiInsertResult]

    /** Insert multiple rows, including AutoInc columns.
      * This is not supported by all database engines (see
      * [[scala.slick.driver.JdbcProfile.capabilities.forceInsert]]).
      * Uses JDBC's batch update feature if supported by the JDBC driver.
      * Returns Some(rowsAffected), or None if the database returned no row
      * count for some part of the batch. If any part of the batch fails, an
      * exception is thrown. */
    def forceInsertAll(values: Iterable[U]): DriverAction[Effect.Write, MultiInsertResult]

    /** Insert a single row if its primary key does not exist in the table,
      * otherwise update the existing record. */
    def insertOrUpdate(value: U): SimpleInsertAction[SingleInsertOrUpdateResult]

    /** Get the SQL statement for inserting a single row from a scalar expression */
    def insertStatementFor[TT](c: TT)(implicit shape: Shape[_ <: FlatShapeLevel, TT, U, _]): String

    /** Get the SQL statement for inserting data produced by another query */
    def insertStatementFor[TT, C[_]](query: Query[TT, U, C]): String

    /** Get the SQL statement for inserting data produced by another query */
    def insertStatementFor[TT, C[_]](compiledQuery: CompiledStreamingExecutable[Query[TT, U, C], _, _]): String

    /** Insert a single row from a scakar expression */
    def insertExpr[TT](c: TT)(implicit shape: Shape[_ <: FlatShapeLevel, TT, U, _]): SimpleInsertAction[QueryInsertResult]

    /** Insert data produced by another query */
    def insert[TT, C[_]](query: Query[TT, U, C]): SimpleInsertAction[QueryInsertResult]

    /** Insert data produced by another query */
    def insert[TT, C[_]](compiledQuery: CompiledStreamingExecutable[Query[TT, U, C], _, _]): SimpleInsertAction[QueryInsertResult]
  }

  /** An InsertInvoker that returns the number of affected rows. */
  trait CountingInsertActionComposer[U] extends InsertActionComposer[U] {
    type SingleInsertResult = Int
    type MultiInsertResult = Option[Int]
    type SingleInsertOrUpdateResult = Int
    type QueryInsertResult = Int

    /** Add a mapping from the inserted values and the generated key to compute a new return value. */
    def returning[RT, RU, C[_]](value: Query[RT, RU, C]): ReturningInsertActionComposer[U, RU]
  }

  /** An InsertActionComposer that returns generated keys or other columns. */
  trait ReturningInsertActionComposer[U, RU] extends InsertActionComposer[U] { self =>
    type SingleInsertResult = RU
    type MultiInsertResult = Seq[RU]
    type SingleInsertOrUpdateResult = Option[RU]
    type QueryInsertResult = Seq[RU]
  }

  //////////////////////////////////////////////////////////// InsertActionComposer Implementations

  /* Currently dispatching all calls to an equivalent InsertInvoker to avoid duplicating the
   * intricate implementation details which need to be partly overridden in the drivers.
   * We should change this in the future because it causes some CPU-intensive compuations
   * (like query compilation) to be performed inside of some of the database Actions. */

  protected class InsertActionComposerImpl[U](inv: FullInsertInvokerDef[U]) extends InsertActionComposer[U] {
    protected[this] def wrapAction[E <: Effect, T](name: String, sql: String, f: Backend#Session => Any): DriverAction[E, T] =
      new DriverAction[E, T] with JdbcDatabaseAction[T] {
        def statements = Iterator(sql)
        def run(ctx: ActionContext[Backend]) = f(ctx.session).asInstanceOf[T]
        override def getDumpInfo = super.getDumpInfo.copy(name = name)
      }
    protected[this] def wrapAction[E <: Effect, T](name: String, f: Backend#Session => Any): SimpleInsertAction[T] =
      new SimpleInsertAction[T] with JdbcDatabaseAction[T] {
        def run(ctx: ActionContext[Backend]) = f(ctx.session).asInstanceOf[T]
        def getDumpInfo = DumpInfo(name)
      }

    def insertStatement = inv.insertStatement
    def forceInsertStatement = inv.forceInsertStatement
    def += (value: U): DriverAction[Effect.Write, SingleInsertResult] = wrapAction("+=", inv.insertStatement, inv.+=(value)(_))
    def forceInsert(value: U): DriverAction[Effect.Write, SingleInsertResult] = wrapAction("forceInsert", inv.forceInsertStatement, inv.forceInsert(value)(_))
    def ++= (values: Iterable[U]): DriverAction[Effect.Write, MultiInsertResult] = wrapAction("++=", inv.insertStatement, inv.++=(values)(_))
    def forceInsertAll(values: Iterable[U]): DriverAction[Effect.Write, MultiInsertResult] = wrapAction("forceInsertAll", inv.forceInsertStatement, inv.forceInsertAll(values.toSeq: _*)(_))
    def insertOrUpdate(value: U): SimpleInsertAction[SingleInsertOrUpdateResult] = wrapAction("insertOrUpdate", inv.insertOrUpdate(value)(_))
    def insertStatementFor[TT](c: TT)(implicit shape: Shape[_ <: FlatShapeLevel, TT, U, _]) = inv.insertStatementFor(c)
    def insertStatementFor[TT, C[_]](query: Query[TT, U, C]) = inv.insertStatementFor(query)
    def insertStatementFor[TT, C[_]](compiledQuery: CompiledStreamingExecutable[Query[TT, U, C], _, _]) = inv.insertStatementFor(compiledQuery)
    def insertExpr[TT](c: TT)(implicit shape: Shape[_ <: FlatShapeLevel, TT, U, _]): SimpleInsertAction[QueryInsertResult] = wrapAction("insertExpr", inv.insertExpr(c)(shape, _))
    def insert[TT, C[_]](query: Query[TT, U, C]): SimpleInsertAction[QueryInsertResult] = wrapAction("insert(query)", inv.insert(query)(_))
    def insert[TT, C[_]](compiledQuery: CompiledStreamingExecutable[Query[TT, U, C], _, _]): SimpleInsertAction[QueryInsertResult] = wrapAction("insert(compiledQuery)", inv.insert(compiledQuery)(_))
  }

  protected class CountingInsertActionComposerImpl[U](inv: CountingInsertInvokerDef[U]) extends InsertActionComposerImpl[U](inv) with CountingInsertActionComposer[U] {
    def returning[RT, RU, C[_]](value: Query[RT, RU, C]): ReturningInsertActionComposer[U, RU] =
      new InsertActionComposerImpl[U](inv.returning(value)) with ReturningInsertActionComposer[U, RU]
  }
}
