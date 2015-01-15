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
import scala.slick.jdbc._
import scala.slick.lifted.{CompiledStreamingExecutable, Query, FlatShapeLevel, Shape}
import scala.slick.profile.{FixedSqlStreamingAction, FixedSqlAction, SqlActionComponent}
import scala.slick.relational.{ResultConverter, CompiledMapping}
import scala.slick.util.{CloseableIterator, DumpInfo, SQLBuilder, ignoreFollowOnError}

trait JdbcActionComponent extends SqlActionComponent { driver: JdbcDriver =>

  type DriverAction[-E <: Effect, +R, +S <: NoStream] = FixedSqlAction[E, R, S]
  type StreamingDriverAction[-E <: Effect, +R, +T] = FixedSqlStreamingAction[E, R, T]

  trait JdbcDriverAction[+R, +S <: NoStream] extends SynchronousDatabaseAction[Backend#This, Effect, R, S] with DriverAction[Effect, R, S]

  protected object StartTransaction extends SynchronousDatabaseAction[Backend#This, Effect, Unit, NoStream] {
    def run(context: ActionContext[Backend#This]): Unit = {
      context.pin
      context.session.startInTransaction
    }
    def getDumpInfo = DumpInfo(name = "StartTransaction")
  }

  case class SimpleJdbcAction[+R](f: ActionContext[Backend#This] => R) extends SynchronousDatabaseAction[Backend#This, Nothing, R, NoStream] {
    def run(context: ActionContext[Backend#This]): R = f(context)
    def getDumpInfo = DumpInfo(name = "SimpleJdbcAction")
  }

  protected object Commit extends SynchronousDatabaseAction[Backend#This, Effect, Unit, NoStream] {
    def run(context: ActionContext[Backend#This]): Unit =
      try context.session.endInTransaction(context.session.conn.commit()) finally context.unpin
    def getDumpInfo = DumpInfo(name = "Commit")
  }

  protected object Rollback extends SynchronousDatabaseAction[Backend#This, Effect, Unit, NoStream] {
    def run(context: ActionContext[Backend#This]): Unit =
      try context.session.endInTransaction(context.session.conn.rollback()) finally context.unpin
    def getDumpInfo = DumpInfo(name = "Rollback")
  }

  class JdbcActionExtensionMethods[E <: Effect, R, S <: NoStream](a: EffectfulAction[E, R, S]) {

    /** Run this Action transactionally. This does not guarantee failures to be atomic in the
      * presence of error handling combinators. If multiple `transactionally` combinators are
      * nested, only the outermost one will be backed by an actual database transaction. Depending
      * on the outcome of running the Action it surrounds, the transaction is committed if the
      * wrapped Action succeeds, or rolled back if the wrapped Action fails. When called on a
      * [[scala.slick.action.SynchronousDatabaseAction]], this combinator gets fused into the
      * action. */
    def transactionally: EffectfulAction[E with Effect.Transactional, R, S] = {
      def nonFused =
        StartTransaction.andThen(a).cleanUp(eo => if(eo.isEmpty) Commit else Rollback)(Action.sameThreadExecutionContext)
          .asInstanceOf[EffectfulAction[E with Effect.Transactional, R, S]]
      a match {
        case a: SynchronousDatabaseAction[_, _, _, _] => new SynchronousDatabaseAction.Fused[Backend#This, E with Effect.Transactional, R, S] {
          def run(context: ActionContext[Backend#This]): R = {
            context.pin
            context.session.startInTransaction
            val res = try {
              a.asInstanceOf[SynchronousDatabaseAction[Backend#This, E, R, S]].run(context)
            } catch {
              case NonFatal(ex) =>
                try context.session.endInTransaction(context.session.conn.rollback()) finally context.unpin
                throw ex
            }
            try context.session.endInTransaction(context.session.conn.commit()) finally context.unpin
            res
          }
          override def nonFusedEquivalentAction = nonFused
        }
        case a => nonFused
      }
    }
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////// Query Actions
  ///////////////////////////////////////////////////////////////////////////////////////////////

  type QueryActionExtensionMethods[R, S <: NoStream] = QueryActionExtensionMethodsImpl[R, S]
  type StreamingQueryActionExtensionMethods[R, T] = StreamingQueryActionExtensionMethodsImpl[R, T]

  def createQueryActionExtensionMethods[R, S <: NoStream](tree: Node, param: Any): QueryActionExtensionMethods[R, S] =
    new QueryActionExtensionMethods[R, S](tree, param)
  def createStreamingQueryActionExtensionMethods[R, T](tree: Node, param: Any): StreamingQueryActionExtensionMethods[R, T] =
    new StreamingQueryActionExtensionMethods[R, T](tree, param)

  class MutatingResultAction[T](rsm: ResultSetMapping, elemType: Type, collectionType: CollectionType, sql: String, param: Any, sendEndMarker: Boolean) extends JdbcDriverAction[Nothing, Streaming[ResultSetMutator[T]]] { streamingAction =>
    class Mutator(val prit: PositionedResultIterator[T], val bufferNext: Boolean, val inv: QueryInvokerImpl[T]) extends ResultSetMutator[T] {
      val pr = prit.pr
      val rs = pr.rs
      var current: T = _
      /** The state of the stream. 0 = in result set, 1 = before end marker, 2 = after end marker. */
      var state = 0
      def row = if(state > 0) throw new SlickException("After end of result set") else current
      def row_=(value: T): Unit = {
        if(state > 0) throw new SlickException("After end of result set")
        pr.restart
        inv.updateRowValues(pr, value)
        rs.updateRow()
      }
      def += (value: T): Unit = {
        rs.moveToInsertRow()
        pr.restart
        inv.updateRowValues(pr, value)
        rs.insertRow()
        if(state == 0) rs.moveToCurrentRow()
      }
      def delete: Unit = {
        if(state > 0) throw new SlickException("After end of result set")
        rs.deleteRow()
        if(invokerPreviousAfterDelete) rs.previous()
      }
      def emitStream(ctx: StreamingActionContext[Backend], limit: Long): this.type = {
        var count = 0L
        try {
          while(count < limit && state == 0) {
            if(!pr.nextRow) state = if(sendEndMarker) 1 else 2
            if(state == 0) {
              current = inv.extractValue(pr)
              count += 1
              ctx.emit(this)
            }
          }
          if(count < limit && state == 1) {
            ctx.emit(this)
            state = 2
          }
        } catch {
          case NonFatal(ex) =>
            try prit.close() catch ignoreFollowOnError
            throw ex
        }
        if(state < 2) this else null
      }
      def end = if(state > 1) throw new SlickException("After end of result set") else state > 0
      override def toString = s"Mutator(state = $state, current = $current)"
    }
    type StreamState = Mutator
    def statements = List(sql)
    def run(ctx: ActionContext[Backend]) =
      throw new SlickException("The result of .mutate can only be used in a streaming way")
    override def emitStream(ctx: StreamingActionContext[Backend], limit: Long, state: StreamState): StreamState = {
      val mu = if(state ne null) state else {
        val inv = createQueryInvoker[T](rsm, param)
        new Mutator(
          inv.results(0, defaultConcurrency = invokerMutateConcurrency, defaultType = invokerMutateType)(ctx.session).right.get,
          ctx.asInstanceOf[Backend#JdbcStreamingDatabaseActionContext].bufferNext,
          inv)
      }
      mu.emitStream(ctx, limit)
    }
    override def cancelStream(ctx: StreamingActionContext[Backend], state: StreamState): Unit = state.prit.close()
    override def getDumpInfo = super.getDumpInfo.copy(name = "mutate")
  }

  class QueryActionExtensionMethodsImpl[R, S <: NoStream](tree: Node, param: Any) extends super.QueryActionExtensionMethodsImpl[R, S] {
    def result: DriverAction[Effect.Read, R, S] = {
      val sql = tree.findNode(_.isInstanceOf[CompiledStatement]).get
        .asInstanceOf[CompiledStatement].extra.asInstanceOf[SQLBuilder.Result].sql
      tree match {
        case (rsm @ ResultSetMapping(_, _, CompiledMapping(_, elemType))) :@ (ct: CollectionType) =>
          (new StreamingInvokerAction[Effect, R, Any] { streamingAction =>
            protected[this] val invoker = createQueryInvoker(rsm, param)
            protected[this] def createBuilder = ct.cons.createBuilder(ct.elementType.classTag).asInstanceOf[Builder[Any, R]]
            def statements = List(sql)
            override def getDumpInfo = super.getDumpInfo.copy(name = "result")
          }).asInstanceOf[DriverAction[Effect.Read, R, S]]
        case First(rsm: ResultSetMapping) =>
          new JdbcDriverAction[R, S] {
            def statements = List(sql)
            def run(ctx: ActionContext[Backend]): R =
              createQueryInvoker[R](rsm, param).first(ctx.session)
            override def getDumpInfo = super.getDumpInfo.copy(name = "result")
          }
      }
    }
  }

  class StreamingQueryActionExtensionMethodsImpl[R, T](tree: Node, param: Any) extends QueryActionExtensionMethodsImpl[R, Streaming[T]](tree, param) with super.StreamingQueryActionExtensionMethodsImpl[R, T] {
    override def result: StreamingDriverAction[Effect.Read, R, T] = super.result.asInstanceOf[StreamingDriverAction[Effect.Read, R, T]]

    /** Same as `mutate(sendEndMarker = false)`. */
    def mutate: DriverAction[Effect.Read with Effect.Write, Nothing, Streaming[ResultSetMutator[T]]] = mutate(false)

    /** Create an Action that can be streamed in order to modify a mutable result set. All stream
      * elements will be the same [[scala.slick.jdbc.ResultSetMutator]] object but it is in a different state each
      * time. Thre resulting stream is always non-buffered and events can be processed either
      * synchronously or asynchronously (but all processing must happen in sequence).
      *
      * @param sendEndMarker If set to true, an extra event is sent after the end of the result
      *                      set, poviding you with a chance to insert additional rows after
      *                      seeing all results. Only `end` (to check for this special event) and
      *                      `insert` may be called in the ResultSetMutator in this case. */
    def mutate(sendEndMarker: Boolean = false): DriverAction[Effect.Read with Effect.Write, Nothing, Streaming[ResultSetMutator[T]]] = {
      val sql = tree.findNode(_.isInstanceOf[CompiledStatement]).get
        .asInstanceOf[CompiledStatement].extra.asInstanceOf[SQLBuilder.Result].sql
      val (rsm @ ResultSetMapping(_, _, CompiledMapping(_, elemType))) :@ (ct: CollectionType) = tree
      new MutatingResultAction[T](rsm, elemType, ct, sql, param, sendEndMarker)
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
    def delete: DriverAction[Effect.Write, Int, NoStream] = {
      val ResultSetMapping(_, CompiledStatement(_, sres: SQLBuilder.Result, _), _) = tree
      new JdbcDriverAction[Int, NoStream] {
        def statements = List(sres.sql)
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

  class SchemaActionExtensionMethodsImpl(schema: SchemaDescription) extends super.SchemaActionExtensionMethodsImpl {
    def create: DriverAction[Effect.Schema, Unit, NoStream] = new JdbcDriverAction[Unit, NoStream] {
      def statements = schema.createStatements.toSeq
      def run(ctx: ActionContext[Backend]): Unit =
        for(s <- statements) ctx.session.withPreparedStatement(s)(_.execute)
      override def getDumpInfo = super.getDumpInfo.copy(name = "schema.create")
    }

    def drop: DriverAction[Effect.Schema, Unit, NoStream] = new JdbcDriverAction[Unit, NoStream] {
      def statements = schema.dropStatements.toSeq
      def run(ctx: ActionContext[Backend]): Unit =
        for(s <- statements) ctx.session.withPreparedStatement(s)(_.execute)
      override def getDumpInfo = super.getDumpInfo.copy(name = "schema.drop")
    }
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////// Update Actions
  ///////////////////////////////////////////////////////////////////////////////////////////////

  type UpdateActionExtensionMethods[T] = UpdateActionExtensionMethodsImpl[T]

  def createUpdateActionExtensionMethods[T](tree: Node, param: Any): UpdateActionExtensionMethods[T] =
    new UpdateActionExtensionMethodsImpl[T](tree, param)

  class UpdateActionExtensionMethodsImpl[T](tree: Node, param: Any) {
    protected[this] val ResultSetMapping(_,
      CompiledStatement(_, sres: SQLBuilder.Result, _),
      CompiledMapping(_converter, _)) = tree
    protected[this] val converter = _converter.asInstanceOf[ResultConverter[JdbcResultConverterDomain, T]]

    /** An Action that updates the data selected by this query. */
    def update(value: T): DriverAction[Effect.Write, Int, NoStream] = {
      new JdbcDriverAction[Int, NoStream] {
        def statements = List(sres.sql)
        def run(ctx: ActionContext[Backend]): Int = ctx.session.withPreparedStatement(sres.sql) { st =>
          st.clearParameters
          converter.set(value, st)
          sres.setter(st, converter.width+1, param)
          st.executeUpdate
        }
        override def getDumpInfo = super.getDumpInfo.copy(name = "update")
      }
    }
    /** Get the statement usd by `update` */
    def updateStatement: String = sres.sql
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////// Insert Actions
  ///////////////////////////////////////////////////////////////////////////////////////////////

  type InsertActionExtensionMethods[T] = CountingInsertActionComposerImpl[T]

  def createInsertActionExtensionMethods[T](compiled: CompiledInsert): InsertActionExtensionMethods[T] =
    new CountingInsertActionComposerImpl[T](createInsertInvoker(compiled))

  //////////////////////////////////////////////////////////// InsertActionComposer Traits

  /** Extension methods to generate the JDBC-specific insert actions. */
  trait SimpleInsertActionComposer[U] extends super.InsertActionExtensionMethodsImpl[U] {
    /** The return type for `insertOrUpdate` operations */
    type SingleInsertOrUpdateResult

    /** Get the SQL statement for a standard (soft) insert */
    def insertStatement: String

    /** Get the SQL statement for a forced insert */
    def forceInsertStatement: String

    /** Insert a single row, skipping AutoInc columns. */
    def += (value: U): DriverAction[Effect.Write, SingleInsertResult, NoStream]

    /** Insert a single row, including AutoInc columns. This is not supported
      * by all database engines (see
      * [[scala.slick.driver.JdbcProfile.capabilities.forceInsert]]). */
    def forceInsert(value: U): DriverAction[Effect.Write, SingleInsertResult, NoStream]

    /** Insert multiple rows, skipping AutoInc columns.
      * Uses JDBC's batch update feature if supported by the JDBC driver.
      * Returns Some(rowsAffected), or None if the database returned no row
      * count for some part of the batch. If any part of the batch fails, an
      * exception is thrown. */
    def ++= (values: Iterable[U]): DriverAction[Effect.Write, MultiInsertResult, NoStream]

    /** Insert multiple rows, including AutoInc columns.
      * This is not supported by all database engines (see
      * [[scala.slick.driver.JdbcProfile.capabilities.forceInsert]]).
      * Uses JDBC's batch update feature if supported by the JDBC driver.
      * Returns Some(rowsAffected), or None if the database returned no row
      * count for some part of the batch. If any part of the batch fails, an
      * exception is thrown. */
    def forceInsertAll(values: Iterable[U]): DriverAction[Effect.Write, MultiInsertResult, NoStream]

    /** Insert a single row if its primary key does not exist in the table,
      * otherwise update the existing record. */
    def insertOrUpdate(value: U): DriverAction[Effect.Write, SingleInsertOrUpdateResult, NoStream]
  }

  /** Extension methods to generate the JDBC-specific insert actions. */
  trait InsertActionComposer[U] extends SimpleInsertActionComposer[U] {
    /** The result type of operations that insert data produced by another query */
    type QueryInsertResult

    /** Get the SQL statement for inserting a single row from a scalar expression */
    def insertStatementFor[TT](c: TT)(implicit shape: Shape[_ <: FlatShapeLevel, TT, U, _]): String

    /** Get the SQL statement for inserting data produced by another query */
    def insertStatementFor[TT, C[_]](query: Query[TT, U, C]): String

    /** Get the SQL statement for inserting data produced by another query */
    def insertStatementFor[TT, C[_]](compiledQuery: CompiledStreamingExecutable[Query[TT, U, C], _, _]): String

    /** Insert a single row from a scalar expression */
    def insertExpr[TT](c: TT)(implicit shape: Shape[_ <: FlatShapeLevel, TT, U, _]): DriverAction[Effect.Write, QueryInsertResult, NoStream]

    /** Insert data produced by another query */
    def insert[TT, C[_]](query: Query[TT, U, C]): DriverAction[Effect.Write, QueryInsertResult, NoStream]

    /** Insert data produced by another query */
    def insert[TT, C[_]](compiledQuery: CompiledStreamingExecutable[Query[TT, U, C], _, _]): DriverAction[Effect.Write, QueryInsertResult, NoStream]
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

    /** Specifies a mapping from inserted values and generated keys to a desired value.
      * @param f Function that maps inserted values and generated keys to a desired value.
      * @tparam R target type of the mapping */
    def into[R](f: (U, RU) => R): IntoInsertActionComposer[U, R]
  }

  /** An InsertActionComposer that returns a mapping of the inserted and generated data. */
  trait IntoInsertActionComposer[U, RU] extends SimpleInsertActionComposer[U] { self =>
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

  protected class InsertActionComposerImpl[U](inv: InsertInvokerDef[U]) extends InsertActionComposer[U] {
    private[this] def fullInv = inv.asInstanceOf[FullInsertInvokerDef[U]]
    protected[this] def wrapAction[E <: Effect, T](name: String, sql: String, f: Backend#Session => Any): DriverAction[E, T, NoStream] =
      new JdbcDriverAction[T, NoStream] {
        def statements = if(sql eq null) Nil else List(sql)
        def run(ctx: ActionContext[Backend]) = f(ctx.session).asInstanceOf[T]
        override def getDumpInfo = super.getDumpInfo.copy(name = name)
      }

    def insertStatement = inv.insertStatement
    def forceInsertStatement = inv.forceInsertStatement
    def += (value: U): DriverAction[Effect.Write, SingleInsertResult, NoStream] = wrapAction("+=", inv.insertStatement, inv.+=(value)(_))
    def forceInsert(value: U): DriverAction[Effect.Write, SingleInsertResult, NoStream] = wrapAction("forceInsert", inv.forceInsertStatement, inv.forceInsert(value)(_))
    def ++= (values: Iterable[U]): DriverAction[Effect.Write, MultiInsertResult, NoStream] = wrapAction("++=", inv.insertStatement, inv.++=(values)(_))
    def forceInsertAll(values: Iterable[U]): DriverAction[Effect.Write, MultiInsertResult, NoStream] = wrapAction("forceInsertAll", inv.forceInsertStatement, inv.forceInsertAll(values.toSeq: _*)(_))
    def insertOrUpdate(value: U): DriverAction[Effect.Write, SingleInsertOrUpdateResult, NoStream] = wrapAction("insertOrUpdate", inv.insertOrUpdateStatement(value), inv.insertOrUpdate(value)(_))
    def insertStatementFor[TT](c: TT)(implicit shape: Shape[_ <: FlatShapeLevel, TT, U, _]) = fullInv.insertStatementFor(c)
    def insertStatementFor[TT, C[_]](query: Query[TT, U, C]) = fullInv.insertStatementFor(query)
    def insertStatementFor[TT, C[_]](compiledQuery: CompiledStreamingExecutable[Query[TT, U, C], _, _]) = fullInv.insertStatementFor(compiledQuery)
    def insertExpr[TT](c: TT)(implicit shape: Shape[_ <: FlatShapeLevel, TT, U, _]): DriverAction[Effect.Write, QueryInsertResult, NoStream] = wrapAction("insertExpr", fullInv.insertExprStatement(c), fullInv.insertExpr(c)(shape, _))
    def insert[TT, C[_]](query: Query[TT, U, C]): DriverAction[Effect.Write, QueryInsertResult, NoStream] = wrapAction("insert(query)", fullInv.insertStatement(query), fullInv.insert(query)(_))
    def insert[TT, C[_]](compiledQuery: CompiledStreamingExecutable[Query[TT, U, C], _, _]): DriverAction[Effect.Write, QueryInsertResult, NoStream] = wrapAction("insert(compiledQuery)", fullInv.insertStatement(compiledQuery), fullInv.insert(compiledQuery)(_))
  }

  protected class CountingInsertActionComposerImpl[U](inv: CountingInsertInvokerDef[U]) extends InsertActionComposerImpl[U](inv) with CountingInsertActionComposer[U] {
    def returning[RT, RU, C[_]](value: Query[RT, RU, C]): ReturningInsertActionComposer[U, RU] = {
      val invr = inv.returning(value)
      new InsertActionComposerImpl[U](invr) with ReturningInsertActionComposer[U, RU] {
        def into[R](f: (U, RU) => R): IntoInsertActionComposer[U, R] = {
          val invri = invr.into(f)
          new InsertActionComposerImpl[U](invri) with IntoInsertActionComposer[U, R]
        }
      }
    }
  }
}
