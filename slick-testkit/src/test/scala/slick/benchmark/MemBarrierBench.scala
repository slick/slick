package slick.benchmark

import java.util.concurrent.atomic.AtomicLong

object MemBarrierBench extends App {

  trait Subscr[T] {
    def onNext(v: T): Unit
  }

  val subscr = new Subscr[Unit] {
    def onNext(v: Unit) = ()
  }

  def pushLocal(n: Long, s: Subscr[Unit]): Unit = {
    var i = 0L
    while(i < n) {
      s.onNext(())
      i += 1
    }
  }

  var count = 0L
  @volatile var volatileCount = 0L
  val atomicCount = new AtomicLong(0L)

  def pushLocalCounting(n: Long, s: Subscr[Unit]): Unit = {
    var i = 0L
    while(i < n) {
      s.onNext(())
      count += 1
      i += 1
    }
  }

  def pushVolatileCounting(n: Long, s: Subscr[Unit]): Unit = {
    var i = 0L
    while(i < n) {
      s.onNext(())
      volatileCount += 1
      i += 1
    }
  }

  def pushAtomicCounting(n: Long, s: Subscr[Unit]): Unit = {
    var i = 0L
    while(i < n) {
      s.onNext(())
      atomicCount.incrementAndGet()
      i += 1
    }
  }

  def time(name: String)(f: => Unit): Unit = {
    val t0 = System.currentTimeMillis()
    f
    val t1 = System.currentTimeMillis()
    println(name+": "+(t1-t0)+"ms")
  }

  val num = 100000000L

  for(_ <- 1 to 10) {
    time("Local            ")(pushLocal(num, subscr))
    time("Local Counting   ")(pushLocalCounting(num, subscr))
    time("Volatile Counting")(pushVolatileCounting(num, subscr))
    time("Atomic Counting  ")(pushAtomicCounting(num, subscr))
  }
}
