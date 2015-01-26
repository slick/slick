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

  protected[this] val invoker: Invoker[T]
  protected[this] def createBuilder: Builder[T, R]

  type StreamState = CloseableIterator[T]

  def run(ctx: JdbcBackend#Context): R = {
    val b = createBuilder
    invoker.foreach(x => b += x)(ctx.session)
    b.result()
  }

  override def emitStream(ctx: JdbcBackend#StreamingContext, limit: Long, state: StreamState): StreamState = {
    val bufferNext = ctx.bufferNext
    val it = if(state ne null) state else invoker.iterator(ctx.session)
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

  override def cancelStream(ctx: JdbcBackend#StreamingContext, state: StreamState): Unit = state.close()

  override def getDumpInfo = super.getDumpInfo.copy(name = "StreamingResultAction")

  override def head: FixedSqlAction[E, T, NoStream] = new SynchronousDatabaseAction[JdbcBackend, E, T, NoStream] with FixedSqlAction[E, T, NoStream] {
    def statements = self.statements
    def run(ctx: JdbcBackend#Context): T = invoker.first(ctx.session)
  }

  override def headOption: FixedSqlAction[E, Option[T], NoStream] = new SynchronousDatabaseAction[JdbcBackend, E, Option[T], NoStream] with FixedSqlAction[E, Option[T], NoStream] {
    def statements = self.statements
    def run(ctx: JdbcBackend#Context): Option[T] = invoker.firstOption(ctx.session)
  }
}

/** A non-streaming Action that wraps a JDBC call. */
case class SimpleJdbcAction[+R](f: JdbcBackend#Context => R) extends SynchronousDatabaseAction[JdbcBackend, Effect.All, R, NoStream] {
  def run(context: JdbcBackend#Context): R = f(context)
  def getDumpInfo = DumpInfo(name = "SimpleJdbcAction")
}
