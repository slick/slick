package scala.slick.backend

import java.util.concurrent.atomic.AtomicBoolean

import org.reactivestreams._

/** A Reactive Streams `Publisher` for database Actions. */
abstract class DatabasePublisher[T] extends Publisher[T] { self =>
  private[this] val used = new AtomicBoolean()

  protected[this] def allowSubscriber(s: Subscriber[_ >: T]): Boolean = {
    if(used.getAndSet(true)) {
      s.onError(new IllegalStateException("Database Action Publisher may not be subscribed to more than once"))
      false
    } else true
  }

  /** Create a Publisher that runs a synchronous mapping function on this Publisher. This
    * can be used to materialize LOB values in a convenient way. */
  def mapResult[U](f: T => U): Publisher[U] = new Publisher[U] {
    def subscribe(s: Subscriber[_ >: U]) = self.subscribe(new Subscriber[T] {
      def onSubscribe(sn: Subscription): Unit = s.onSubscribe(sn)
      def onComplete(): Unit = s.onComplete()
      def onError(t: Throwable): Unit = s.onError(t)
      def onNext(t: T): Unit = s.onNext(f(t))
    })
  }
}
