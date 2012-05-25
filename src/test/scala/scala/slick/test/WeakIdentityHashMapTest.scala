package scala.slick.test

import org.junit.Test
import org.junit.Assert._
import scala.slick.util.WeakIdentityHashMap

class WeakIdentityHashMapTest {
  case class Foo(name: String)

  @Test def test() {
    val m = new WeakIdentityHashMap[Foo, String]

    val t1 = Foo("t1")
    val t2 = Foo("t2")
    val t3 = Foo("t3")
    val t4 = Foo("t4")
    val t5 = Foo("t5")
    var x1 = Foo("x1")
    var x2 = Foo("x2")
    var x3 = Foo("x3")
    var x4 = Foo("x4")
    var x5 = Foo("x5")

    m.put(t1, t1.name)
    m.put(t2, t2.name)
    m.put(t3, t3.name)
    m.put(t4, t4.name)
    m.put(t5, t5.name)
    m.put(x1, x1.name)
    m.put(x2, x2.name)
    m.put(x3, x3.name)
    m.put(x4, x4.name)
    m.put(x5, x5.name)

    println("Before gc")
    for { (k, v) <- m } println("  "+k+" -> "+v)
    assertEquals(10, m.size)

    x1 = null
    x2 = null
    x3 = null
    x4 = null
    x5 = null
    System.gc()

    println("After gc")
    for { (k, v) <- m } println("  "+k+" -> "+v)
    assertEquals(5, m.size)

    assertEquals(Some("t1"), m.get(t1))
    assertEquals(None, m.get(Foo("t1")))
    assertEquals(None, m.get(Foo("x1")))
  }
}
