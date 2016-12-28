package slick.basic

import java.util.concurrent.atomic.AtomicReference

import org.reactivestreams.{Subscriber, Subscription}
import slick.dbio.DBIO
import slick.relational.RelationalProfile

import scala.concurrent.ExecutionContext

/** A Reactive Streams `Subscriber` for database Actions. */
class DatabaseSubscriber[E, P <: RelationalProfile](db: P#Backend#Database)(implicit ec: ExecutionContext) extends Subscriber[DBIO[E]] {
  val subscription: AtomicReference[Subscription] = new AtomicReference[Subscription]()
  val done: AtomicReference[Boolean] = new AtomicReference[Boolean](true)

  override def onError(t: Throwable): Unit = {
    if (null == t) throw new NullPointerException("throwable is null")
  }

  override def onSubscribe(s: Subscription): Unit = {
    if (null == subscription.get()) {
      subscription.set(s)
      done.set(false)
      subscription.get().request(1L)
    } else {
      s.cancel()
    }
  }

  override def onComplete(): Unit = {
    // mmhh What to do here?
  }

  /**
    * Consumes the element and executes the dbio action with the provided database
    */
  override def onNext(e: DBIO[E]): Unit = {
    if (null == e) {
      throw new NullPointerException("element is null")
    } else {
      if (!done.get()) {
        db
          .run(e)
          .andThen{case _ => subscription.get().request(1L)}
      }
    }
  }
}
