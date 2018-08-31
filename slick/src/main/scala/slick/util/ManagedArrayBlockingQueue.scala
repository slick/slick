package slick.util

import java.util.concurrent.{BlockingQueue, TimeUnit}
import java.util.concurrent.locks._
import java.util

import slick.util.AsyncExecutor._

/** A simplified copy of `java.util.concurrent.ArrayBlockingQueue` with additional logic for
  * temporarily rejecting elements based on the current size. All features of the original
  * ArrayBlockingQueue have been ported, except the mutation methods of the iterator. See
  * `java.util.concurrent.ArrayBlockingQueue` for documentation.
  *
  * Furthermore this implementation has a `pause` feature where it does not pass through
  * low- or mid-priority tasks when paused.
  */
class ManagedArrayBlockingQueue[E >: Null <: PrioritizedRunnable](maximumInUse: Int, capacity: Int, fair: Boolean = false)
  extends util.AbstractQueue[E]
  with BlockingQueue[E]
  with Logging { self =>

  private[this] val lock = new ReentrantLock(fair)
  private[this] val notEmpty = lock.newCondition
  private[this] val itemQueueNotFull = lock.newCondition

  private[this] def checkNotNull(v: AnyRef): Unit = if (v == null) throw new NullPointerException
  private[this] def checkNotInUse(e: E) = require(!e.inUseCounterSet, "in use count is already set")

  private[this] val itemQueue = new InternalArrayQueue[E](2*capacity)
  private[this] val highPrioItemQueue = new InternalArrayQueue[E](capacity)

  private[this] def counts = (if (paused) 0 else itemQueue.count) + highPrioItemQueue.count

  /**
    * The number of low/medium priority items in use
    */
  private[this] var inUseCount = 0

  private[this] var paused = false

  private[util] def increaseInUseCount(pr: PrioritizedRunnable): Unit = locked {
    if (!pr.inUseCounterSet) {
      require(inUseCount < maximumInUse, "count cannot be increased")
      inUseCount += 1
      pr.inUseCounterSet = true
      if (inUseCount == maximumInUse) {
        logger.debug("pausing")
        paused = true
      }
    }
  }

  private[util] def decreaseInUseCount(): Unit = locked {
    require(inUseCount > 0, "count cannot be decreased")
    inUseCount -= 1
    if (inUseCount == maximumInUse - 1) {
      logger.debug("resuming")
      paused = false
      if (counts > 0) notEmpty.signalAll()
    }
  }

  // implementation of offer(e), put(e) and offer(e, timeout, unit)
  private[this] def insert(e: E): Boolean = {
    val r = e.priority match {
      case WithConnection => highPrioItemQueue.insert(e)
      case Continuation => itemQueue.insert(e)
      case Fresh => if (itemQueue.count < capacity) itemQueue.insert(e) else false
    }
    if (r) notEmpty.signal()
    r
  }

  def offer(e: E): Boolean = {
    checkNotNull(e)
    checkNotInUse(e)
    locked { insert(e) }
  }

  def put(e: E): Unit = {
    checkNotNull(e)
    checkNotInUse(e)
    lockedInterruptibly {
      while (e.priority == Fresh && itemQueue.count >= capacity) itemQueueNotFull.await()
      insert(e)
    }
  }

  def offer(e: E, timeout: Long, unit: TimeUnit): Boolean = {
    checkNotNull(e)
    checkNotInUse(e)
    var nanos: Long = unit.toNanos(timeout)
    lockedInterruptibly {
      while (e.priority == Fresh && itemQueue.count >= capacity) {
        if (nanos <= 0) return false
        nanos = itemQueueNotFull.awaitNanos(nanos)
      }
      insert(e)
    }
  }

  // implementation of poll, take and poll(timeout, unit)
  private[this] def extract(): E = {
    if (highPrioItemQueue.count != 0) highPrioItemQueue.extract
    else if (!paused && itemQueue.count != 0) {
      val item = itemQueue.extract
      increaseInUseCount(item)
      item
    }
    else null
  }

  def poll: E = locked { extract() }

  def take: E = lockedInterruptibly {
    while (counts == 0) notEmpty.await()
    extract()
  }

  def poll(timeout: Long, unit: TimeUnit): E = {
    var nanos: Long = unit.toNanos(timeout)
    lockedInterruptibly {
      while (counts == 0) {
        if (nanos <= 0) return null
        nanos = notEmpty.awaitNanos(nanos)
      }
      extract()
    }
  }

  def peek: E = locked {
    if (counts == 0) null
    else {
      val e = highPrioItemQueue.peek
      if (e != null) e else itemQueue.peek
    }
  }

  // how many items in the queue
  def size: Int = locked(itemQueue.count + highPrioItemQueue.count) // can't use `counts`
                                                                    // here, it refers to
                                                                    // `paused`

  // how much normal capacity we have left before put/offer start blocking
  def remainingCapacity: Int = math.max(locked(capacity - itemQueue.count), 0)

  override def remove(o: AnyRef): Boolean = if (o eq null) false else locked {
    highPrioItemQueue.remove(o) || {
      val r = itemQueue.remove(o)
      if (r && remainingCapacity != 0) itemQueueNotFull.signalAll()
      r
    }
  }

  override def contains(o: AnyRef): Boolean = locked {
    itemQueue.contains(o) || highPrioItemQueue.contains(o)
  }

  override def clear(): Unit = locked {
    itemQueue.clear()
    highPrioItemQueue.clear()
    itemQueueNotFull.signalAll()
  }

  def drainTo(c: util.Collection[_ >: E]): Int = locked {
    val n = highPrioItemQueue.drainTo(c) + itemQueue.drainTo(c)
    if (remainingCapacity != 0) {
      itemQueueNotFull.signalAll()
    }
    n
  }

  def drainTo(c: util.Collection[_ >: E], maxElements: Int): Int = locked {
    var n = highPrioItemQueue.drainTo(c, maxElements)
    if (n < maxElements) {
      n += itemQueue.drainTo(c, maxElements - n)
    }
    if (remainingCapacity != 0) {
      itemQueueNotFull.signalAll()
    }
    n
  }

  def iterator: util.Iterator[E] = {
    import scala.collection.JavaConverters._

    // copy all items from queues and build a snapshot
    val items = locked {
      (highPrioItemQueue.iterator.asScala ++ itemQueue.iterator.asScala).toList.toIterator
    }

    new util.Iterator[E] {
      override def hasNext: Boolean = items.hasNext
      override def next: E = items.next
      override def remove(): Unit = throw new UnsupportedOperationException
    }
  }

  @inline private[this] def locked[T](f: => T) = {
    lock.lock()
    try f finally lock.unlock()
  }

  @inline private[this] def lockedInterruptibly[T](f: => T) = {
    lock.lockInterruptibly()
    try f finally lock.unlock()
  }
}
