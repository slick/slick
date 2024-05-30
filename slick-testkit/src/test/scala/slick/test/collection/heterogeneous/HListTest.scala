package slick.test.collection.heterogeneous

import org.junit.Test
import org.junit.Assert._
import slick.collection.heterogeneous.*
import org.w3c.dom.Text


class HListTest {

  val hl1 = "test" :: HNil
  val hl2 = 1 :: "test2" :: HNil

  val c = hl1 ::: hl2
 val cc = c ::: c
  
  @Test
  def concatTest: Unit = {

    assertTrue(c.length == 3)

    assertTrue(c.head == "test")
    assertTrue(c.tail.head == 1)
    assertTrue(c.tail.tail.head == "test2")
  }

  @Test 
  def concatTest1: Unit = {

    assert(cc.toString == HList.concat(c, c).toString)

    assert(cc.length == 6)

    assert(cc.head == "test")
    assert(cc.tail.head == 1)
    assert(cc.tail.tail.head == "test2")    
    assert(cc.tail.tail.tail.head == "test")
    assert(cc.tail.tail.tail.tail.head == 1)
    assert(cc.tail.tail.tail.tail.tail.head == "test2")   
  }

  @Test 
  def concatTest3: Unit = {
    val h1 = cc ::: hl2 ::: hl1
    val c1 = HList.concat(cc, HList.concat(hl2, hl1))

    assert(h1.toString == c1.toString)
    assert(h1.length == c1.length)

  }

}
