package slick.util

import java.util.Arrays

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.compat.*
import scala.collection.immutable
import scala.reflect.ClassTag
import scala.util.hashing.MurmurHash3

/** An efficient immutable array implementation which is used in the AST. Semantics are generally
  * the same as for Scala collections but for performance reasons it does not implement any
  * standard collection traits. */
final class ConstArray[+T] private[util] (a: Array[Any], val length: Int) extends Product { self =>
  private def this(a: Array[Any]) = this(a, a.length)

  def apply(i: Int): T =
    if(i > length) throw new IndexOutOfBoundsException
    else a(i).asInstanceOf[T]

  def lengthCompare(n: Int): Int = length.compare(n)

  def isEmpty = length == 0

  def nonEmpty = length != 0

  def foreach[R](f: T => R): Unit = {
    var i = 0
    while(i < length) {
      f(a(i).asInstanceOf[T])
      i += 1
    }
  }

  def map[R](f: T => R): ConstArray[R] = {
    var i = 0
    val ar = new Array[Any](length)
    while(i < length) {
      ar(i) = f(a(i).asInstanceOf[T])
      i += 1
    }
    new ConstArray[R](ar)
  }

  def collect[R](f: PartialFunction[T, R]): ConstArray[R] = {
    var i, j = 0
    var matched = true
    def d(x: T): R = {
      matched = false
      null.asInstanceOf[R]
    }
    val ar = new Array[Any](length)
    while(i < length) {
      matched = true
      val v = f.applyOrElse(a(i).asInstanceOf[T], d)
      if(matched) {
        ar(j) = v
        j += 1
      }
      i += 1
    }
    if(j == 0) ConstArray.empty
    else new ConstArray[R](ar, j)
  }

  def flatMap[R](f: T => ConstArray[R]): ConstArray[R] = {
    var len = 0
    val buf = new Array[ConstArray[R]](length)
    var i = 0
    while(i < length) {
      val r = f(a(i).asInstanceOf[T])
      buf(i) = r
      len += r.length
      i += 1
    }
    if(len == 0) ConstArray.empty
    else {
      val ar = new Array[Any](len)
      i = 0
      var j = 0
      while(i < length) {
        val r = buf(i)
        val l = r.length
        r.copySliceTo(ar, 0, j, l)
        j += l
        i += 1
      }
      new ConstArray[R](ar)
    }
  }

  def flatten[R](implicit ev: T <:< ConstArray[R]): ConstArray[R] = {
    var len = 0
    var i = 0
    while(i < length) {
      len += a(i).asInstanceOf[ConstArray[_]].length
      i += 1
    }
    if(len == 0) ConstArray.empty
    else {
      val ar = new Array[Any](len)
      i = 0
      var j = 0
      while(i < length) {
        val r = a(i).asInstanceOf[ConstArray[_]]
        val l = r.length
        r.copySliceTo(ar, 0, j, l)
        j += l
        i += 1
      }
      new ConstArray[R](ar)
    }
  }

  /** Perform a mapping operation that does not change the type. If all elements remain unchanged
    * (as determined by object identity), return this ConstArray instead of building a new one. */
  def endoMap(f: T => T @uncheckedVariance): ConstArray[T] = {
    var i = 0
    var changed = false
    val ar = new Array[Any](length)
    while(i < length) {
      val n0 = a(i)
      val n1 = f(n0.asInstanceOf[T])
      ar(i) = n1
      if(n1.asInstanceOf[AnyRef] ne n0.asInstanceOf[AnyRef]) changed = true
      i += 1
    }
    if(changed) new ConstArray[T](ar) else this
  }

  def zipWithIndex: ConstArrayOp[(T, Int)] = new ConstArrayOp[(T, Int)] {
    def map[R](f: ((T, Int)) => R): ConstArray[R] = {
      var i = 0
      self.map { v =>
        val r = f(v, i)
        i += 1
        r
      }
    }
    def foreach[R](f: ((T, Int)) => R): Unit = {
      var i = 0
      self.foreach { v =>
        f(v, i)
        i += 1
      }
    }
  }

  def zip[U](u: ConstArray[U]): ConstArrayOp[(T, U)] = new ConstArrayOp[(T, U)] {
    def map[R](f: ((T, U)) => R): ConstArray[R] = {
      var i = 0
      val len = math.min(length, u.length)
      var ar = new Array[Any](len)
      while(i < len) {
        ar(i) = f((a(i).asInstanceOf[T], u(i)))
        i += 1
      }
      new ConstArray[R](ar)
    }
    def foreach[R](f: ((T, U)) => R): Unit = {
      var i = 0
      val len = math.min(length, u.length)
      while(i < len) {
        f((a(i).asInstanceOf[T], u(i)))
        i += 1
      }
    }
  }

  override def toString = a.mkString("ConstArray(", ", ", ")")

  def iterator: Iterator[T] = new Iterator[T] {
    private[this] var pos = 0
    def hasNext: Boolean = pos < self.length
    def next(): T = {
      var r = a(pos)
      pos += 1
      r.asInstanceOf[T]
    }
  }

  def indexWhere(f: T => Boolean): Int = {
    var i = 0
    while(i < length) {
      if(f(a(i).asInstanceOf[T])) return i
      i += 1
    }
    -1
  }

  def find(f: T => Boolean): Option[T] = {
    var idx = indexWhere(f)
    if(idx == -1) None else Some(a(idx).asInstanceOf[T])
  }

  def exists(f: T => Boolean): Boolean = indexWhere(f) >= 0

  def forall(f: T => Boolean): Boolean = {
    var i = 0
    while(i < length) {
      if(!f(a(i).asInstanceOf[T])) return false
      i += 1
    }
    true
  }

  def filter(p: T => Boolean): ConstArray[T] = {
    val ar = new Array[Any](length)
    var i, ri = 0
    while(i < length) {
      val v = a(i)
      if(p(v.asInstanceOf[T])) {
        ar(ri) = v
        ri += 1
      }
      i += 1
    }
    if(ri == length) this
    else if(ri == 0) ConstArray.empty
    else new ConstArray[T](ar, ri)
  }

  def withFilter(p: T => Boolean): ConstArrayOp[T] = new ConstArrayOp[T] {
    def map[R](f: T => R): ConstArray[R] = {
      val ar = new Array[Any](length)
      var i, ri = 0
      while(i < length) {
        val v = a(i).asInstanceOf[T]
        if(p(v)) {
          ar(ri) = f(v)
          ri += 1
        }
        i += 1
      }
      if(ri == 0) ConstArray.empty
      else new ConstArray[R](ar, ri)
    }
    def foreach[R](f: T => R): Unit = {
      var i = 0
      while(i < length) {
        val v = a(i).asInstanceOf[T]
        if(p(v)) f(v)
        i += 1
      }
    }
  }

  def mkString(sep: String) = iterator.mkString(sep)

  def mkString(start: String, sep: String, end: String) = iterator.mkString(start, sep, end)

  def foldLeft[B](z: B)(op: (B, T) => B): B = {
    var v = z
    var i = 0
    while(i < length) {
      v = op(v, a(i).asInstanceOf[T])
      i += 1
    }
    v
  }

  def foldRight[B](z: B)(op: (T, B) => B): B = {
    var v = z
    var i = length - 1
    while(i >= 0) {
      v = op(a(i).asInstanceOf[T], v)
      i -= 1
    }
    v
  }

  ///////////////////////////////////////////////////////// conversion

  def toSeq: immutable.IndexedSeq[T] = new immutable.IndexedSeq[T] {
    def apply(idx: Int) = self(idx)
    def length = self.length
  }

  def toSet: immutable.HashSet[T @uncheckedVariance] = {
    val b = immutable.HashSet.newBuilder[T]
    var i = 0
    while(i < length) {
      b += a(i).asInstanceOf[T]
      i += 1
    }
    b.result()
  }

  def toMap[R, U](implicit ev: T <:< (R, U)): immutable.HashMap[R, U] = {
    val b = immutable.HashMap.newBuilder[R, U]
    var i = 0
    while(i < length) {
      b += a(i).asInstanceOf[(R, U)]
      i += 1
    }
    b.result()
  }

  def toArray[R >: T : ClassTag]: Array[R] = {
    val ar = new Array[R](length)
    System.arraycopy(a, 0, ar, 0, length)
    ar
  }

  private[util] def copySliceTo(dest: Array[Any], srcPos: Int, destPos: Int, len: Int): Unit = {
    if(len+srcPos > length) throw new IndexOutOfBoundsException
    System.arraycopy(a, srcPos, dest, destPos, len)
  }

  ///////////////////////////////////////////////////////// concatenation

  def :+ (v: T @uncheckedVariance): ConstArray[T] = {
    val a2 = Arrays.copyOf[Any](a.asInstanceOf[Array[AnyRef]], length + 1).asInstanceOf[Array[Any]]
    a2(length) = v
    new ConstArray[T](a2)
  }

  def +: (v: T @uncheckedVariance): ConstArray[T] = {
    val a2 = new Array[Any](length + 1)
    a2(0) = v
    System.arraycopy(a, 0, a2, 1, length)
    new ConstArray[T](a2)
  }

  def ++ [U >: T](u: ConstArray[U]): ConstArray[U] = {
    val len2 = u.length
    val ar = new Array[Any](length + len2)
    System.arraycopy(a, 0, ar, 0, length)
    u.copySliceTo(ar, 0, length, len2)
    new ConstArray[U](ar)
  }

  def ++ (o: Option[T] @uncheckedVariance): ConstArray[T] =
    if(o.isDefined) this :+ o.get else this

  ///////////////////////////////////////////////////////// slicing

  def head: T = apply(0)

  def last: T = apply(length-1)

  def headOption: Option[T] = if(isEmpty) None else Some(head)

  def lastOption: Option[T] = if(isEmpty) None else Some(last)

  def slice(from: Int, until: Int): ConstArray[T] = {
    if(from == 0) {
      if(until == length) this
      else new ConstArray(a, until)
    } else new ConstArray(Arrays.copyOfRange[AnyRef](a.asInstanceOf[Array[AnyRef]], from, until).asInstanceOf[Array[Any]])
  }

  def tail = slice(1, length)

  def init = slice(0, length-1)

  def take(n: Int) = slice(0, math.min(n, length))

  def drop(n: Int) =
    if(n >= length) ConstArray.empty else slice(n, length-n)

  ///////////////////////////////////////////////////////// Equals

  def canEqual(that: Any): Boolean = that.isInstanceOf[ConstArray[_]]

  private[this] var _hashCode: Int = 0

  override def hashCode = {
    if(_hashCode != 0) _hashCode
    else {
      val h = MurmurHash3.productHash(this)
      _hashCode = h
      h
    }
  }

  override def equals(o: Any): Boolean = o match {
    case o: ConstArray[_] =>
      if(length != o.length) false
      else {
        var i = 0
        while(i < length) {
          if(a(i) != o(i)) return false
          i += 1
        }
        true
      }
    case _ => false
  }

  ///////////////////////////////////////////////////////// Product

  def productArity = length

  def productElement(i: Int): Any = apply(i)

  override def productPrefix = "ConstArray"
}

object ConstArray {
  val empty: ConstArray[Nothing] = new ConstArray[Nothing](new Array[Any](0))

  def apply[T](v0: T): ConstArray[T] = {
    val a = new Array[Any](1)
    a(0) = v0
    new ConstArray(a)
  }

  def apply[T](v0: T, v1: T): ConstArray[T] = {
    val a = new Array[Any](2)
    a(0) = v0
    a(1) = v1
    new ConstArray(a)
  }

  def apply[T](v0: T, v1: T, v2: T): ConstArray[T] = {
    val a = new Array[Any](3)
    a(0) = v0
    a(1) = v1
    a(2) = v2
    new ConstArray(a)
  }

  def from[T](values: Iterable[T]): ConstArray[T] = {
    val a = new Array[Any](values.size)
    var i = 0
    values.foreach { v =>
      a(i) = v
      i += 1
    }
    new ConstArray[T](a)
  }

  def from[T](o: Option[T]): ConstArray[T] =
    if(o.isDefined) apply(o.get) else empty

  def unsafeWrap[T](values: Array[Any]): ConstArray[T] =
    new ConstArray(values)

  //def unapplySeq[T](a: ConstArray[T]) = new ConstArrayExtract[T](a) // Requires Scala 2.11
  def unapplySeq[T](a: ConstArray[T]): Some[IndexedSeq[T]] = Some(a.toSeq)

  def newBuilder[T](initialCapacity: Int = 16, growFactor: Double = 2.0): ConstArrayBuilder[T] =
    new ConstArrayBuilder[T](initialCapacity, growFactor)
}

/** A lazy operation on a ConstArray, produced by `withFilter`, `zip`, `zipWithIndex` and
  * `ConstArrayOp.from(Range)`. */
trait ConstArrayOp[+T] extends Any {
  def map[R](f: T => R): ConstArray[R]
  def foreach[R](f: T => R): Unit
  def force: ConstArray[T] = map(identity)
}

object ConstArrayOp {
  def from(r: Range): RangeConstArrayOp = new RangeConstArrayOp(r)
}

final class RangeConstArrayOp(val r: Range) extends ConstArrayOp[Int] {
  def map[R](f: Int => R): ConstArray[R] = {
    val len = r.length
    val a = new Array[Any](len)
    var i = 0
    var v = r.start
    while(i < len) {
      a(i) = f(v)
      i += 1
      v += r.step
    }
    ConstArray.unsafeWrap[R](a)
  }
  def foreach[R](f: Int => R): Unit = r.foreach(f)
}

/*final class ConstArrayExtract[T](val ca: ConstArray[T]) extends AnyVal {
  def isEmpty = ca eq null
  def get = ca
}*/

/** A mutable builder for ConstArrays. */
final class ConstArrayBuilder[T](initialCapacity: Int = 16, growFactor: Double = 2.0) { self =>
  private[this] var a: Array[Any] = new Array[Any](initialCapacity)
  private[this] var len: Int = 0

  def length = len

  def result: ConstArray[T] =
    if(len == 0) ConstArray.empty
    else new ConstArray[T](a, len)

  def += (v: T): Unit = {
    ensure(1)
    a(len) = v
    len += 1
  }

  def ++= (vs: ConstArray[T]): Unit = {
    val vslen = vs.length
    ensure(vslen)
    vs.copySliceTo(a, 0, len, vslen)
    len += vslen
  }

  def ++= (vs: IterableOnce[T]): Unit = {
    if(vs.isInstanceOf[scala.collection.IndexedSeq[_]]) ensure(vs.asInstanceOf[scala.collection.IndexedSeq[_]].size)
    vs.iterator.foreach(self += _)
  }

  def ++= (vs: Option[T]): Unit =
    if(vs.isDefined) this += vs.get

  private[this] def ensure(i: Int): Unit = {
    val total = len + i
    if(a.length < total)
      a = Arrays.copyOf[Any](a.asInstanceOf[Array[AnyRef]], math.max((a.length * growFactor).toInt, total)).asInstanceOf[Array[Any]]
  }

  def + (v: T): this.type = { this += v; this }
  def ++ (vs: ConstArray[T]): this.type = { this ++= vs; this }
  def ++ (vs: IterableOnce[T]): this.type = { this ++= vs; this }
  def ++ (vs: Option[T]): this.type = { this ++= vs; this }
}
