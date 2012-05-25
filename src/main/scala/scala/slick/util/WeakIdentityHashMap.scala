package scala.slick.util

import scala.collection.generic.{CanBuildFrom, MutableMapFactory}
import java.lang.ref.{Reference, ReferenceQueue, WeakReference}
import java.util.HashMap

/**
 * A WeakHashMap which uses object identity instead of equality on the keys.
 */
class WeakIdentityHashMap[A, B]
  extends scala.collection.mutable.Map[A, B]
  with scala.collection.mutable.MapLike[A, B, WeakIdentityHashMap[A, B]] { self =>

  private[this] val map = new HashMap[WeakReference[A], B]
  private[this] val refQueue = new ReferenceQueue[A]

  override def apply(key: A): B = {
    expunge()
    val r = map.get(new WeakRefId(key))
    if(r.asInstanceOf[AnyRef] eq null) throw new NoSuchElementException
    else r
  }

  def get(key: A): Option[B] = {
    expunge()
    Option(map.get(new WeakRefId(key)))
  }

  def -= (key: A): this.type = {
    expunge()
    map.remove(new WeakRefId(key))
    this
  }

  override def remove(k: A): Option[B] = {
    expunge()
    val r = map.remove(new WeakRefId(k))
    Option(r)
  }

  def += (kv: (A, B)): this.type = {
    expunge()
    map.put(new WeakRefId(kv._1, refQueue), kv._2)
    this
  }

  override def put(k: A, v: B): Option[B] = {
    expunge()
    val r = map.put(new WeakRefId(k, refQueue), v)
    Option(r)
  }

  override def update(k: A, v: B) {
    expunge()
    map.put(new WeakRefId(k, refQueue), v)
  }

  override def clear() {
    expunge()
    map.clear()
  }

  def iterator: Iterator[(A, B)] = {
    expunge()
    new ReadAheadIterator[(A, B)] {
      private[this] val it = self.map.entrySet().iterator()
      protected def fetchNext(): (A, B) = {
        while(it.hasNext) {
          val e = it.next()
          val got = e.getKey.get()
          if(got.asInstanceOf[AnyRef] ne null)
            return (got, e.getValue)
        }
        finished()
      }
    }
  }

  override def size: Int = {
    expunge()
    map.size()
  }

  def expunge() {
    var ref: Reference[_ <: A] = null
    while(true) {
      ref = refQueue.poll()
      if(ref eq null) return
      //println("Expunging "+ref)
      map.remove(ref)
    }
  }

  override def empty = new WeakIdentityHashMap[A, B]
}

object WeakIdentityHashMap extends MutableMapFactory[WeakIdentityHashMap] {
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), WeakIdentityHashMap[A, B]] = new MapCanBuildFrom[A, B]
  def empty[A, B]: WeakIdentityHashMap[A, B] = new WeakIdentityHashMap[A, B]
}
