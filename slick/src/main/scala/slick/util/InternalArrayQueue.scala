package slick.util

import java.util

/** A simplified copy of `java.util.concurrent.ArrayBlockingQueue` with additional logic for
  * temporarily rejecting elements based on the current size. All features of the original
  * ArrayBlockingQueue have been ported, except the mutation methods of the iterator. See
  * `java.util.concurrent.ArrayBlockingQueue` for documentation. */
private[util] final class InternalArrayQueue[E >: Null <: AnyRef](capacity: Int) {

  private[this] val items = new Array[AnyRef](capacity)
  private[this] var takeIndex, putIndex = 0
  private[util] var count = 0

  private[this] def checkNotNull(v: AnyRef): Unit = if (v == null) throw new NullPointerException
  private[this] def inc(i: Int): Int = if(i+1 == items.length) 0 else i+1
  private[this] def itemAt(i: Int): E = items(i).asInstanceOf[E]

  private[util] def extract: E = {
    val items = this.items
    val x: E = items(takeIndex).asInstanceOf[E]
    items(takeIndex) = null
    takeIndex = inc(takeIndex)
    count -= 1
    x
  }

  private[util] def insert(x: E): Boolean = {
    if (count == items.length) {
      false
    } else {
      items(putIndex) = x
      putIndex = inc(putIndex)
      count += 1
      true
    }
  }

  private[this] def removeAt(_i: Int): Unit = {
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
  }

  private[util] def contains(o: AnyRef): Boolean = {
    if (o == null) return false
    val items = this.items
    var i = takeIndex
    var k = count
    while (k > 0) {
      if (o == items(i)) return true
      i = inc(i)
      k -= 1
    }
    false
  }

  private[util] def remove(o: AnyRef): Boolean = if (o eq null) false else {
    val items = this.items
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

  def peek: E = if(count == 0) null else itemAt(takeIndex)

  private[util] def clear(): Unit = {
    val items = this.items
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
  }

  private[util] def drainTo(c: util.Collection[_ >: E]): Int = {
    checkNotNull(c)
    val items = this.items
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
    }
    n
  }

  private[util] def drainTo(c: util.Collection[_ >: E], maxElements: Int): Int = {
    checkNotNull(c)
    if (maxElements <= 0) return 0
    val items = this.items
    var i: Int = takeIndex
    var n: Int = 0
    val max: Int = if (maxElements < count) maxElements else count
    while (n < max) {
      c.add(items(i).asInstanceOf[E])
      items(i) = null
      i = inc(i)
      n += 1
    }
    if (n > 0) {
      count -= n
      takeIndex = i
    }
    n
  }

  private[util] def iterator: util.Iterator[E] = new util.Iterator[E] {
    private var remaining: Int = _
    private var nextIndex: Int = _
    private var nextItem: E = _
    private var lastItem: E = _
    private var lastRet: Int = -1

    remaining = count
    if(remaining > 0) {
      nextIndex = takeIndex
      nextItem = itemAt(nextIndex)
    }

    def hasNext: Boolean = remaining > 0

    def next: E = {
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

    override def remove(): Unit = throw new UnsupportedOperationException
  }

}
