package slick.jdbc

import scala.collection.mutable

import slick.dbio.*
import slick.sql.{FixedSqlAction, FixedSqlStreamingAction}
import slick.util.{CloseableIterator, DumpInfo}

/** A streaming Action that wraps an Invoker.
  * It is used for the Lifted Embedding, Direct Embedding, Plain SQL queries, and JDBC metadata.
  */
trait StreamingInvokerAction[R, T, -E <: Effect]
  extends SynchronousDatabaseAction[R, Streaming[T], JdbcBackend#JdbcActionContext, E]
    with FixedSqlStreamingAction[R, T, E] { self =>

  protected[this] def createInvoker(sql: Iterable[String]): Invoker[T]
  protected[this] def createBuilder: mutable.Builder[T, R]

  final def run(ctx: JdbcBackend#JdbcActionContext): R = {
    val b = createBuilder
    createInvoker(statements).foreach(x => b += x)(ctx.session)
    b.result()
  }

  override final def openStream(ctx: JdbcBackend#JdbcActionContext): CloseableIterator[T] =
    createInvoker(statements).iteratorTo(0)(ctx.session)

  override def getDumpInfo = super.getDumpInfo.copy(name = "StreamingResultAction")

  final def head: FixedSqlAction[T, NoStream, E] = new HeadAction(statements)

  final def headOption: FixedSqlAction[Option[T], NoStream, E] = new HeadOptionAction(statements)

  private[this] class HeadAction(val statements: Iterable[String])
    extends SynchronousDatabaseAction[T, NoStream, JdbcBackend#JdbcActionContext, E]
      with FixedSqlAction[T, NoStream, E] {
    def run(ctx: JdbcBackend#JdbcActionContext): T = createInvoker(statements).first(ctx.session)
    override def overrideStatements(_statements: Iterable[String]): HeadAction = new HeadAction(_statements)
  }

  private[this] class HeadOptionAction(val statements: Iterable[String])
    extends SynchronousDatabaseAction[Option[T], NoStream, JdbcBackend#JdbcActionContext, E]
      with FixedSqlAction[Option[T], NoStream, E] {
    def run(ctx: JdbcBackend#JdbcActionContext): Option[T] = createInvoker(statements).firstOption(ctx.session)
    override def overrideStatements(_statements: Iterable[String]): HeadOptionAction = new HeadOptionAction(_statements)
  }

  final def overrideStatements(_statements: Iterable[String]): FixedSqlAction[R, Streaming[T], E] =
    new StreamingInvokerAction[R, T, E] {
      def statements = _statements
      protected[this] def createInvoker(sql: Iterable[String]): Invoker[T] = self.createInvoker(sql)
      protected[this] def createBuilder: mutable.Builder[T, R] = self.createBuilder
    }
}

/** A non-streaming Action that wraps a JDBC call. */
case class SimpleJdbcAction[+R](f: JdbcBackend#JdbcActionContext => R)
  extends SynchronousDatabaseAction[R, NoStream, JdbcBackend#JdbcActionContext, Effect.All] {
  def run(context: JdbcBackend#JdbcActionContext): R = f(context)
  def getDumpInfo = DumpInfo(name = "SimpleJdbcAction")
}
