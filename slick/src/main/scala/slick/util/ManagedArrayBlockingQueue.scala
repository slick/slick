package slick.util

import java.util.concurrent.{BlockingQueue, TimeUnit}
import java.util.concurrent.locks._
import java.util

import slick.util.AsyncExecutor._

/** A simplified copy of `java.util.concurrent.ArrayBlockingQueue` with additional logic for
  * temporarily rejecting elements based on the current size. All features of the original
  * ArrayBlockingQueue have been ported, except the mutation methods of the iterator. See
  * `java.util.concurrent.ArrayBlockingQueue` for documentation. */
class ManagedArrayBlockingQueue[E >: Null <: PrioritizedRunnable](maximumInUse: Int, capacity: Int, fair: Boolean = false)
  extends util.AbstractQueue[E]
  with BlockingQueue[E]
  with Logging { self =>

  private[this] val lock = new ReentrantLock(fair)
  private[this] val notEmpty = lock.newCondition
  private[this] val notFull = lock.newCondition

  private[this] def checkNotNull(v: AnyRef): Unit = if (v == null) throw new NullPointerException
  private[this] def checkNotInUse(e: E) = require(!e.inUseCounterSet, "in use count is already set")

  private[this] val itemQueue = new InternalArrayQueue[E](2*capacity)
  private[this] val highPrioItemQueue = new InternalArrayQueue[E](capacity)

  private[this] def counts = (if (paused) 0 else  itemQueue.count) + highPrioItemQueue.count

  /**
    * The number of low/medium priority items in use
    */
  private[this] var inUseCount = 0

  private[this] var paused = false

  private[util] def increaseInUseCount(pr: PrioritizedRunnable): Unit = {
    if (!pr.inUseCounterSet) {
      locked {
        require(inUseCount < maximumInUse, "count cannot be increased")
        inUseCount += 1
        pr.inUseCounterSet = true
        if (inUseCount == maximumInUse) {
          logger.debug("pausing")
          paused = true
        }
      }
    }
  }

  private[util] def decreaseInUseCount(): Unit = {
    locked {
      require(inUseCount > 0, "count cannot be decreased")
      inUseCount -= 1
      if (inUseCount == maximumInUse - 1) {
        logger.debug("resuming")
        paused = false
        if (counts > 0) notEmpty.signalAll()
      }
    }
  }

  def offer(e: E): Boolean = {
    checkNotNull(e)
    checkNotInUse(e)
    locked { insert(e) }
  }

  private[this] def insert(e: E): Boolean = {
    val r = e.priority match {
      case WithConnection => highPrioItemQueue.insert(e)
      case Continuation => itemQueue.insert(e)
      case Fresh => if (itemQueue.count < capacity) itemQueue.insert(e) else false
    }
    if (counts > 0) notEmpty.signal()
    r
  }

  def put(e: E): Unit = {
    checkNotNull(e)
    checkNotInUse(e)
    lockedInterruptibly {
      while (e.priority == Fresh && itemQueue.count >= capacity) notFull.await()
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
        nanos = notFull.awaitNanos(nanos)
      }
      insert(e)
      return true
    }
  }

  def poll: E = locked { extract() }

  private[this] def extract(): E = {
    if (highPrioItemQueue.count != 0) highPrioItemQueue.extract
    else if (!paused && itemQueue.count != 0) {
      val item = itemQueue.extract
      increaseInUseCount(item)
      item
    }
    else null
  }

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

  def size: Int = locked(counts)

  def remainingCapacity: Int = locked(capacity - itemQueue.count - highPrioItemQueue.count)

  override def remove(o: AnyRef): Boolean = if (o eq null) false else {
    locked {
      if (highPrioItemQueue.remove(o)) {
        true
      } else {
        itemQueue.remove(o)
      }
    }
  }

  override def contains(o: AnyRef): Boolean = {
    locked {
      itemQueue.contains(o) || highPrioItemQueue.contains(o)
    }
  }

  override def clear() {
    locked {
      itemQueue.clear()
      highPrioItemQueue.clear()
      notFull.signalAll()
    }
  }

  def drainTo(c: util.Collection[_ >: E]): Int = {
    locked {
      val n = highPrioItemQueue.drainTo(c) + itemQueue.drainTo(c)
      if (n > 0) {
        notFull.signalAll()
      }
      n
    }
  }

  def drainTo(c: util.Collection[_ >: E], maxElements: Int): Int = {
    locked {
      var n = highPrioItemQueue.drainTo(c, maxElements)
      if (n < maxElements) {
        n += itemQueue.drainTo(c, maxElements - n)
      }
      if (n > 0) {
        notFull.signalAll()
      }
      n
    }
  }

  def iterator: util.Iterator[E] = new util.Iterator[E] {

    private var current = 0
    private val iterators = Array(highPrioItemQueue.iterator, itemQueue.iterator)

    override def hasNext: Boolean = {
      locked {
        while (current < iterators.length && !iterators(current).hasNext)
          current = current + 1

        current < iterators.length
      }
    }

    def next: E = {
      locked {
        while (current < iterators.length && !iterators(current).hasNext)
          current = current + 1

        return iterators(current).next()
      }
    }

    def remove(): Unit = throw new UnsupportedOperationException
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
