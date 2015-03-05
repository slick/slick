package slick.util

import java.util.concurrent.{ArrayBlockingQueue, TimeUnit, BlockingQueue}
import java.util.concurrent.locks._
import java.util._

/** A simplified copy of `java.util.concurrent.ArrayBlockingQueue` with additional logic for
  * temporarily rejecting elements based on the current size. All features of the original
  * ArrayBlockingQueue have been ported, except the mutation methods of the iterator. See
  * `java.util.concurrent.ArrayBlockingQueue` for documentation. */
abstract class ManagedArrayBlockingQueue[E >: Null <: AnyRef](capacity: Int, fair: Boolean = false) extends AbstractQueue[E] with BlockingQueue[E] { self =>

  /** Determine if the item should be accepted at the current time. */
  protected[this] def accept(item: E, size: Int): Boolean

  private[this] val items = new Array[AnyRef](capacity)
  private[this] val lock = new ReentrantLock(fair)
  private[this] val notEmpty = lock.newCondition
  private[this] val notFull = lock.newCondition
  private[this] var takeIndex, putIndex, count = 0

  private[this] def checkNotNull(v: AnyRef): Unit = if (v == null) throw new NullPointerException

  private[this] def inc(i: Int): Int = if(i+1 == items.length) 0 else i+1

  private[this] def dec(i: Int): Int = (if(i == 0) items.length else i) - 1

  private[this] def itemAt(i: Int): E = items(i).asInstanceOf[E]

  private[this] def insert(x: E) {
    items(putIndex) = x
    putIndex = inc(putIndex)
    count += 1
    notEmpty.signal
  }

  private[this] def extract: E = {
    val items = this.items
    val x: E = items(takeIndex).asInstanceOf[E]
    items(takeIndex) = null
    takeIndex = inc(takeIndex)
    count -= 1
    notFull.signal
    return x
  }

  private[this] def removeAt(_i: Int) {
    var i = _i
    val items = this.items
    if (i == takeIndex) {
      items(takeIndex) = null
      takeIndex = inc(takeIndex)
    }
    else {
      var cond = true
      while (cond) {
        val nexti: Int = inc(i)
        if (nexti != putIndex) {
          items(i) = items(nexti)
          i = nexti
        }
        else {
          items(i) = null
          putIndex = i
          cond = false
        }
      }
    }
    count -= 1
    notFull.signal
  }

  def offer(e: E): Boolean = {
    checkNotNull(e)
    locked {
      if (count == items.length || !accept(e, count)) false
      else { insert(e); true }
    }
  }

  def put(e: E) {
    checkNotNull(e)
    lockedInterruptibly {
      while (count == items.length || !accept(e, count)) notFull.await
      insert(e)
    }
  }

  def offer(e: E, timeout: Long, unit: TimeUnit): Boolean = {
    checkNotNull(e)
    var nanos: Long = unit.toNanos(timeout)
    lockedInterruptibly {
      while (count == items.length || !accept(e, count)) {
        if (nanos <= 0) return false
        nanos = notFull.awaitNanos(nanos)
      }
      insert(e)
      return true
    }
  }

  def poll: E = locked(if ((count == 0)) null else extract)

  def take: E = lockedInterruptibly {
    while (count == 0) notEmpty.await
    extract
  }

  def poll(timeout: Long, unit: TimeUnit): E = {
    var nanos: Long = unit.toNanos(timeout)
    lockedInterruptibly {
      while (count == 0) {
        if (nanos <= 0) return null
        nanos = notEmpty.awaitNanos(nanos)
      }
      extract
    }
  }

  def peek: E = locked((if(count == 0) null else itemAt(takeIndex)))

  def size: Int = locked(count)

  def remainingCapacity: Int = locked(items.length - count)

  override def remove(o: AnyRef): Boolean = if (o eq null) false else {
    val items = this.items
    locked {
      var i: Int = takeIndex
      var k: Int = count
      while (k > 0) {
        if (o == items(i)) {
          removeAt(i)
          return true
        }
        i = inc(i)
        k -= 1
      }
      false
    }
  }

  override def contains(o: AnyRef): Boolean = {
    if (o == null) return false
    val items = this.items
    locked {
      var i = takeIndex
      var k = count
      while (k > 0) {
        if (o == items(i)) return true
        i = inc(i)
        k -= 1
      }
      false
    }
  }

  override def clear {
    val items = this.items
    locked {
      var i = takeIndex
      var k = count
      while (k > 0) {
        items(i) = null
        i = inc(i)
        k -= 1
      }
      count = 0
      putIndex = 0
      takeIndex = 0
      notFull.signalAll
    }
  }

  def drainTo(c: Collection[_ >: E]): Int = {
    checkNotNull(c)
    if (c eq this) throw new IllegalArgumentException
    val items = this.items
    locked {
      var i = takeIndex
      var n = 0
      val max = count
      while (n < max) {
        c.add(items(i).asInstanceOf[E])
        items(i) = null
        i = inc(i)
        n += 1
      }
      if (n > 0) {
        count = 0
        putIndex = 0
        takeIndex = 0
        notFull.signalAll
      }
      n
    }
  }

  def drainTo(c: Collection[_ >: E], maxElements: Int): Int = {
    checkNotNull(c)
    if (c eq this) throw new IllegalArgumentException
    if (maxElements <= 0) return 0
    val items = this.items
    locked {
      var i: Int = takeIndex
      var n: Int = 0
      val max: Int = if ((maxElements < count)) maxElements else count
      while (n < max) {
        c.add(items(i).asInstanceOf[E])
        items(i) = null
        i = inc(i)
        n += 1
      }
      if (n > 0) {
        count -= n
        takeIndex = i
        notFull.signalAll
      }
      n
    }
  }

  def iterator: Iterator[E] = new Iterator[E] {
    private var remaining: Int = _
    private var nextIndex: Int = _
    private var nextItem: E = _
    private var lastItem: E = _
    private var lastRet: Int = -1

    locked {
      remaining = count
      if(remaining > 0) {
        nextIndex = takeIndex
        nextItem = itemAt(nextIndex)
      }
    }

    def hasNext: Boolean = remaining > 0

    def next: E = {
      locked {
        if (remaining <= 0) throw new NoSuchElementException
        lastRet = nextIndex
        var x: E = itemAt(nextIndex)
        if (x == null) {
          x = nextItem
          lastItem = null
        }
        else lastItem = x
        while ({ remaining -= 1; remaining > 0 } && { nextIndex = inc(nextIndex); nextItem = itemAt(nextIndex); nextItem == null }) ()
        x
      }
    }

    def remove: Unit = throw new UnsupportedOperationException
  }

  @inline private[this] def locked[T](f: => T) = {
    lock.lock
    try f finally lock.unlock
  }

  @inline private[this] def lockedInterruptibly[T](f: => T) = {
    lock.lockInterruptibly
    try f finally lock.unlock
  }
}
