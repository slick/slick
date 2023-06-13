package slick.util

import java.util
import java.util.concurrent.{BlockingQueue, TimeUnit}
import java.util.concurrent.locks.*

import slick.util.AsyncExecutor.*

/** A simplified copy of `java.util.concurrent.ArrayBlockingQueue` with additional logic for
 * temporarily rejecting elements based on the current size. All features of the original
 * ArrayBlockingQueue have been ported, except the mutation methods of the iterator. See
 * `java.util.concurrent.ArrayBlockingQueue` for documentation.
 *
 * Furthermore this implementation has a `pause` feature where it does not pass through
 * low- or mid-priority tasks when paused.
 */
class ManagedArrayBlockingQueue(maximumInUse: Int, capacity: Int, fair: Boolean = false)
  extends util.AbstractQueue[PrioritizedRunnable]
    with BlockingQueue[PrioritizedRunnable]
    with Logging { self =>

  private[this] val lock = new ReentrantLock(fair)
  private[this] val notEmpty = lock.newCondition
  private[this] val itemQueueNotFull = lock.newCondition

  private[this] def checkNotNull(v: AnyRef): Unit = if (v == null) throw new NullPointerException
  private[this] def checkNotInUse(e: PrioritizedRunnable) = require(!e.inUseCounterSet, "in use count is already set")

  private[this] val itemQueue = new InternalArrayQueue[PrioritizedRunnable](2 * capacity)
  private[this] val highPriorityItemQueue = new InternalArrayQueue[PrioritizedRunnable](capacity)

  private[this] def counts = (if (paused) 0 else itemQueue.count) + highPriorityItemQueue.count

  /**
   * The number of low/medium priority items in use
   */
  private[slick] var nonHighItemsInUseCount = 0

  private[this] var paused = false

  /**
   * Non-high [[PrioritizedRunnable]]s should call this at the beginning of the run method.
   *
   * Ensures that [[nonHighItemsInUseCount]] accounts for the given [[PrioritizedRunnable]], as appropriate.
   *
   * @return true if the [[nonHighItemsInUseCount]] was incremented now or in the past for the [[PrioritizedRunnable]],
   *         false if [[maximumInUse]] would be exceeded.
   */
  private[util] def attemptPrepare(pr: PrioritizedRunnable): Boolean = locked {
    if (pr.inUseCounterSet)
      true
    else if (nonHighItemsInUseCount >= maximumInUse)
      false
    else {
      nonHighItemsInUseCount += 1
      pr.inUseCounterSet = true
      if (nonHighItemsInUseCount == maximumInUse) {
        logger.debug("pausing")
        paused = true
      }
      true
    }
  }

  /**
   * Non-high [[PrioritizedRunnable]]s should call this at the end of the run method.
   *
   * Decrement [[nonHighItemsInUseCount]] for the given [[PrioritizedRunnable]] as appropriate.
   *
   * @return true if [[PrioritizedRunnable.connectionReleased]] is false or [[nonHighItemsInUseCount]] is decremented,
   *         false if [[nonHighItemsInUseCount]] was already 0
   */
  private[util] def attemptCleanUp(pr: PrioritizedRunnable): Boolean =
    if (!pr.connectionReleased)
      true
    else
      locked {
        if (nonHighItemsInUseCount <= 0)
          false
        else {
          nonHighItemsInUseCount -= 1
          if (nonHighItemsInUseCount == maximumInUse - 1) {
            logger.debug("resuming")
            paused = false
            if (counts > 0) notEmpty.signalAll()
          }
          true
        }
      }

  // implementation of offer(e), put(e) and offer(e, timeout, unit)
  private[this] def insert(e: PrioritizedRunnable): Boolean = {
    val r = e.priority() match {
      case WithConnection => highPriorityItemQueue.insert(e)
      case Continuation   => itemQueue.insert(e)
      case Fresh          => if (itemQueue.count < capacity) itemQueue.insert(e) else false
    }
    if (r) notEmpty.signal()
    r
  }

  def offer(e: PrioritizedRunnable): Boolean = {
    checkNotNull(e)
    checkNotInUse(e)
    locked {
      insert(e)
    }
  }

  def put(e: PrioritizedRunnable): Unit = {
    checkNotNull(e)
    checkNotInUse(e)
    lockedInterruptibly {
      while (e.priority() == Fresh && itemQueue.count >= capacity) itemQueueNotFull.await()
      insert(e)
    }
  }

  def offer(e: PrioritizedRunnable, timeout: Long, unit: TimeUnit): Boolean = {
    checkNotNull(e)
    checkNotInUse(e)
    var nanos: Long = unit.toNanos(timeout)
    lockedInterruptibly {
      while (e.priority() == Fresh && itemQueue.count >= capacity) {
        if (nanos <= 0) return false
        nanos = itemQueueNotFull.awaitNanos(nanos)
      }
      insert(e)
    }
  }

  // implementation of poll, take and poll(timeout, unit)
  private[this] def extract(): PrioritizedRunnable = {
    if (highPriorityItemQueue.count != 0) highPriorityItemQueue.extract
    else if (!paused && itemQueue.count != 0) {
      val item = itemQueue.extract
      require(attemptPrepare(item), "In-use count limit reached")
      item
    }
    else null
  }

  def poll: PrioritizedRunnable = locked {
    extract()
  }

  def take: PrioritizedRunnable = lockedInterruptibly {
    while (counts == 0) notEmpty.await()
    extract()
  }

  def poll(timeout: Long, unit: TimeUnit): PrioritizedRunnable = {
    var nanos: Long = unit.toNanos(timeout)
    lockedInterruptibly {
      while (counts == 0) {
        if (nanos <= 0) return null
        nanos = notEmpty.awaitNanos(nanos)
      }
      extract()
    }
  }

  def peek: PrioritizedRunnable = locked {
    if (counts == 0) null
    else {
      val e = highPriorityItemQueue.peek
      if (e != null) e else itemQueue.peek
    }
  }

  // how many items in the queue
  def size: Int = locked(itemQueue.count + highPriorityItemQueue.count) // can't use `counts`
                                                                    // here, it refers to
                                                                    // `paused`

  // how much normal capacity we have left before put/offer start blocking
  def remainingCapacity: Int = math.max(locked(capacity - itemQueue.count), 0)

  override def remove(o: AnyRef): Boolean = if (o eq null) false else locked {
    highPriorityItemQueue.remove(o) || {
      val r = itemQueue.remove(o)
      if (r && remainingCapacity != 0) itemQueueNotFull.signalAll()
      r
    }
  }

  override def contains(o: AnyRef): Boolean = locked {
    itemQueue.contains(o) || highPriorityItemQueue.contains(o)
  }

  override def clear(): Unit = locked {
    itemQueue.clear()
    highPriorityItemQueue.clear()
    itemQueueNotFull.signalAll()
  }

  def drainTo(c: util.Collection[_ >: PrioritizedRunnable]): Int = locked {
    val n = highPriorityItemQueue.drainTo(c) + itemQueue.drainTo(c)
    if (remainingCapacity != 0) {
      itemQueueNotFull.signalAll()
    }
    n
  }

  def drainTo(c: util.Collection[_ >: PrioritizedRunnable], maxElements: Int): Int = locked {
    var n = highPriorityItemQueue.drainTo(c, maxElements)
    if (n < maxElements) {
      n += itemQueue.drainTo(c, maxElements - n)
    }
    if (remainingCapacity != 0) {
      itemQueueNotFull.signalAll()
    }
    n
  }

  def iterator: util.Iterator[PrioritizedRunnable] = {
    import scala.jdk.CollectionConverters.*

    // copy all items from queues and build a snapshot
    val items = locked {
      (highPriorityItemQueue.iterator.asScala ++ itemQueue.iterator.asScala).toList.iterator
    }

    new util.Iterator[PrioritizedRunnable] {
      override def hasNext: Boolean = items.hasNext
      override def next(): PrioritizedRunnable = items.next()
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
