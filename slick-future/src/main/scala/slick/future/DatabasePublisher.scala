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
  * '''Internal use only.''' Instances are created exclusively by
  * [[slick.future.Database.fromCore]] via `lazyPublisher`; each instance is
  * single-use (one `subscribe` or one `foreach`).  A second `subscribe` call
  * will signal `onError` to the second subscriber per the single-subscription
  * contract.  Calling `foreach` and `subscribe` on the same instance, or
  * calling `mapResult` and then subscribing to both the original and the
  * derived publisher, is undefined behaviour — `it` and `release` are not
  * safe for concurrent or repeated traversal.
  *
  * Elements are emitted strictly one at a time: `it.next()` is called only to produce the value
  * passed to the immediately following `subscriber.onNext(...)` call.  The iterator does not
  * advance further until the downstream signals demand again, regardless of how large an `n` the
  * subscriber passes to `request(n)`.  This guarantees that cursor-bound values such as
  * `java.sql.Blob`/`Clob` handles and `ResultSetMutator` objects remain valid during `onNext`.
  *
  * `release` is called exactly once when the stream completes, is cancelled, or errors.
  * The caller must ensure `release` is idempotent.
  */
private[future] final class DatabasePublisherImpl[T](
  it: Iterator[T],
  release: () => Unit
) extends DatabasePublisher[T] {

  private val subscribed = new AtomicBoolean(false)

  private def tryRelease(): Unit = try release() catch { case _: Throwable => () }

  override def mapResult[U](f: T => U): DatabasePublisher[U] =
    new DatabasePublisherImpl[U](it.map(f), release)

  override def foreach[U](f: T => U)(implicit ec: ExecutionContext): Future[Unit] =
    Future {
      try { it.foreach(f) }
      finally { tryRelease() }
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
            tryRelease()
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

        // Drain loop: after releasing the CAS we re-check whether demand arrived
        // concurrently and re-enter if so, closing the window where a request()
        // call sees emitting==true, backs off, but we have already decided to exit.
        var continue = true
        while (continue) {
          try {
            // Emit one element at a time, consuming exactly one demand unit per element.
            while (!cancelled && !done && demand.get() > 0 && it.hasNext) {
              demand.decrementAndGet()
              s.onNext(it.next()) // advance cursor and immediately deliver to subscriber
            }
            if (!cancelled && !done && !it.hasNext) {
              done = true
              tryRelease()
              s.onComplete()
            } else if (cancelled && !done) {
              done = true
              tryRelease()
            }
          } catch {
            case t: Throwable =>
              if (!done) {
                done = true
                tryRelease()
                s.onError(t)
              }
          }
          // Release the CAS, then check if there is new demand (or the stream is
          // already terminal). If so, try to re-acquire immediately so no pending
          // request() call is left stranded with no thread to serve it.
          emitting.set(false)
          continue = !done && !cancelled && demand.get() > 0 && it.hasNext &&
            emitting.compareAndSet(false, true)
        }
      }

      override def cancel(): Unit = {
        cancelled = true
        // If the emit loop is running it will observe cancelled==true on its next
        // iteration and call tryRelease() itself when it exits.  We only need to
        // release here when no loop is running, which we detect by successfully
        // claiming the emitting CAS.  If we cannot claim it the loop is active and
        // will take care of cleanup.
        if (!done && emitting.compareAndSet(false, true)) {
          // We own the emitting flag now; no loop will start. Release and hand
          // the flag back.
          done = true
          emitting.set(false)
          tryRelease()
        }
      }
    }

    s.onSubscribe(subscription)
  }
}
