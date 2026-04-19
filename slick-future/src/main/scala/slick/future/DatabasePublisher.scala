package slick.future

import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}

import scala.concurrent.{ExecutionContext, Future}

import org.reactivestreams.{Publisher, Subscriber, Subscription}

/** A Reactive Streams `Publisher` for database streaming actions. */
trait DatabasePublisher[T] extends Publisher[T] { self =>

  /** Create a Publisher that runs a synchronous mapping function on this Publisher.
    * This can be used to materialize LOB values in a convenient way. */
  def mapResult[U](f: T => U): DatabasePublisher[U]

  /** Consume the stream, processing each element sequentially on the specified ExecutionContext.
    * The resulting Future completes when all elements have been processed or an error was
    * signaled. */
  def foreach[U](f: T => U)(implicit ec: ExecutionContext): Future[Unit]
}

/** A `DatabasePublisher` backed by an iterator and a release action.
  *
  * Pass the tuple from `BasicDatabaseDef#stream(a).allocated` — `(Iterator[T], F[Unit])` —
  * converted to `(Iterator[T], () => Unit)` by the caller.
  *
  * Elements are emitted strictly one at a time: `it.next()` is called only to produce the value
  * passed to the immediately following `subscriber.onNext(...)` call.  The iterator does not
  * advance further until the downstream signals demand again, regardless of how large an `n` the
  * subscriber passes to `request(n)`.  This guarantees that cursor-bound values such as
  * `java.sql.Blob`/`Clob` handles and `ResultSetMutator` objects remain valid during `onNext`.
  *
  * `release` is called exactly once when the stream completes, is cancelled, or errors.
  *
  * Single-subscription: a second `subscribe` call delivers an immediate `onError`.
  */
final class DatabasePublisherImpl[T](
  it: Iterator[T],
  release: () => Unit
) extends DatabasePublisher[T] {

  private val subscribed = new AtomicBoolean(false)

  override def mapResult[U](f: T => U): DatabasePublisher[U] =
    new DatabasePublisherImpl[U](it.map(f), release)

  override def foreach[U](f: T => U)(implicit ec: ExecutionContext): Future[Unit] =
    Future {
      try { it.foreach(f) }
      finally { release() }
    }

  override def subscribe(s: Subscriber[? >: T]): Unit = {
    if (!subscribed.compareAndSet(false, true)) {
      // Reactive Streams rule 2.12: must call onSubscribe before onError
      s.onSubscribe(new Subscription {
        override def request(n: Long): Unit = ()
        override def cancel(): Unit = ()
      })
      s.onError(new IllegalStateException("DatabasePublisher allows only a single subscriber"))
      return
    }

    @volatile var cancelled = false
    @volatile var done = false

    // Accumulated unfulfilled demand. Negative values are not possible in normal use;
    // we saturate at Long.MaxValue to avoid overflow.
    val demand = new AtomicLong(0L)
    // True while the emit loop is running. Re-entrant request() calls just add to demand
    // and return; the running loop will pick up the extra demand before exiting.
    val emitting = new AtomicBoolean(false)

    val subscription = new Subscription {

      override def request(n: Long): Unit = {
        if (n <= 0) {
          if (!done) {
            done = true
            try release() catch { case _: Throwable => () }
            s.onError(new IllegalArgumentException(
              s"request must be called with a positive demand but got $n (Reactive Streams rule 3.9)"))
          }
          return
        }
        if (cancelled || done) return

        // Accumulate demand (saturate at Long.MaxValue)
        demand.getAndUpdate(cur => if (cur > Long.MaxValue - n) Long.MaxValue else cur + n)

        // Only one thread runs the emit loop at a time. If already emitting (re-entrant
        // call from onNext), just return — the loop will see the updated demand and continue.
        if (!emitting.compareAndSet(false, true)) return

        try {
          // Emit one element at a time, consuming exactly one demand unit per element.
          while (!cancelled && !done && demand.get() > 0 && it.hasNext) {
            demand.decrementAndGet()
            val elem = it.next()  // advance cursor …
            s.onNext(elem)        // … immediately deliver to subscriber
          }
          if (!cancelled && !done && !it.hasNext) {
            done = true
            try release() catch { case _: Throwable => () }
            s.onComplete()
          }
        } catch {
          case t: Throwable =>
            if (!done) {
              done = true
              try release() catch { case _: Throwable => () }
              s.onError(t)
            }
        } finally {
          emitting.set(false)
        }
      }

      override def cancel(): Unit = {
        cancelled = true
        if (!done) {
          done = true
          try release() catch { case _: Throwable => () }
        }
      }
    }

    s.onSubscribe(subscription)
  }
}
