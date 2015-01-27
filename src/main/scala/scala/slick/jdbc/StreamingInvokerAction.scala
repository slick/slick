package scala.slick.jdbc

import scala.collection.mutable.Builder
import scala.util.control.NonFatal

import scala.slick.action._
import scala.slick.profile.{FixedSqlAction, FixedSqlStreamingAction}
import scala.slick.util.{DumpInfo, CloseableIterator, ignoreFollowOnError}

/** A streaming Action that wraps an Invoker.
  * It is used for the Lifted Embedding, Direct Embedding, Plain SQL queries, and JDBC metadata.
  */
trait StreamingInvokerAction[-E <: Effect, R, T] extends SynchronousDatabaseAction[JdbcBackend, E, R, Streaming[T]] with FixedSqlStreamingAction[E, R, T] { self =>

  protected[this] def createInvoker(sql: Iterable[String]): Invoker[T]
  protected[this] def createBuilder: Builder[T, R]

  type StreamState = CloseableIterator[T]

  final def run(ctx: JdbcBackend#Context): R = {
    val b = createBuilder
    createInvoker(statements).foreach(x => b += x)(ctx.session)
    b.result()
  }

  override final def emitStream(ctx: JdbcBackend#StreamingContext, limit: Long, state: StreamState): StreamState = {
    val bufferNext = ctx.bufferNext
    val it = if(state ne null) state else createInvoker(statements).iterator(ctx.session)
    var count = 0L
    try {
      while(if(bufferNext) it.hasNext && count < limit else count < limit && it.hasNext) {
        count += 1
        ctx.emit(it.next())
      }
    } catch {
      case NonFatal(ex) =>
        try it.close() catch ignoreFollowOnError
        throw ex
    }
    if(if(bufferNext) it.hasNext else count == limit) it else null
  }

  override final def cancelStream(ctx: JdbcBackend#StreamingContext, state: StreamState): Unit = state.close()

  override def getDumpInfo = super.getDumpInfo.copy(name = "StreamingResultAction")

  final def head: FixedSqlAction[E, T, NoStream] = new HeadAction(statements)

  final def headOption: FixedSqlAction[E, Option[T], NoStream] = new HeadOptionAction(statements)

  private[this] class HeadAction(val statements: Iterable[String]) extends SynchronousDatabaseAction[JdbcBackend, E, T, NoStream] with FixedSqlAction[E, T, NoStream] {
    def run(ctx: JdbcBackend#Context): T = createInvoker(statements).first(ctx.session)
    def overrideStatements(_statements: Iterable[String]) = new HeadAction(_statements)
  }

  private[this] class HeadOptionAction(val statements: Iterable[String]) extends SynchronousDatabaseAction[JdbcBackend, E, Option[T], NoStream] with FixedSqlAction[E, Option[T], NoStream] {
    def run(ctx: JdbcBackend#Context): Option[T] = createInvoker(statements).firstOption(ctx.session)
    def overrideStatements(_statements: Iterable[String]) = new HeadOptionAction(_statements)
  }

  final def overrideStatements(_statements: Iterable[String]): FixedSqlAction[E, R, Streaming[T]] = new StreamingInvokerAction[E, R, T] {
    def statements = _statements
    protected[this] def createInvoker(sql: Iterable[String]): Invoker[T] = self.createInvoker(sql)
    protected[this] def createBuilder: Builder[T, R] = self.createBuilder
  }
}

/** A non-streaming Action that wraps a JDBC call. */
case class SimpleJdbcAction[+R](f: JdbcBackend#Context => R) extends SynchronousDatabaseAction[JdbcBackend, Effect.All, R, NoStream] {
  def run(context: JdbcBackend#Context): R = f(context)
  def getDumpInfo = DumpInfo(name = "SimpleJdbcAction")
}
