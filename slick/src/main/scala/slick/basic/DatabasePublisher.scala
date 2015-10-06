package slick.basic

import org.reactivestreams._

import scala.concurrent.{Promise, Future, ExecutionContext}

import slick.dbio.DBIO
import scala.util.{Failure, Success}

/** A Reactive Streams `Publisher` for database Actions. */
abstract class DatabasePublisher[T] extends Publisher[T] { self =>
  /** Create a Publisher that runs a synchronous mapping function on this Publisher. This
    * can be used to materialize LOB values in a convenient way. */
  def mapResult[U](f: T => U): DatabasePublisher[U] = new DatabasePublisher[U] {
    def subscribe(s: Subscriber[_ >: U]) = self.subscribe(new Subscriber[T] {
      def onSubscribe(sn: Subscription): Unit = s.onSubscribe(sn)
      def onComplete(): Unit = s.onComplete()
      def onError(t: Throwable): Unit = s.onError(t)
      def onNext(t: T): Unit = s.onNext(f(t))
    })
  }

  /** Consume the stream, processing each element sequentially on the specified ExecutionContext.
    * The resulting Future completes when all elements have been processed or an error was
    * signaled. */
  def foreach[U](f: T => U)(implicit ec: ExecutionContext): Future[Unit] = {
    val p = Promise[Unit]()
    @volatile var lastMsg: Future[Any] = null
    @volatile var subscr: Subscription = null
    subscribe(new Subscriber[T] {
      def onSubscribe(s: Subscription): Unit = {
        subscr = s
        s.request(1L)
      }
      def onComplete(): Unit = {
        val l = lastMsg
        if(l ne null) l.onComplete {
          case Success(_) => p.trySuccess(())
          case Failure(t) => p.tryFailure(t)
        }(DBIO.sameThreadExecutionContext)
        else p.trySuccess(())
      }
      def onError(t: Throwable): Unit = {
        val l = lastMsg
        if(l ne null) l.onComplete(_ => p.tryFailure(t))(DBIO.sameThreadExecutionContext)
        else p.tryFailure(t)
      }
      def onNext(t: T): Unit = {
        lastMsg = Future(f(t))
        lastMsg.onComplete {
          case Success(v) => subscr.request(1L)
          case Failure(t) =>
            subscr.cancel()
            p.tryFailure(t)
        }
      }
    })
    p.future
  }
}
