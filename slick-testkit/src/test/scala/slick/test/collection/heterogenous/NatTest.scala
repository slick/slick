package slick.test.collection.heterogenous

import org.junit.Test
import org.junit.Assert._
import slick.collection.heterogeneous.Nat

class NatTest {
  @Test
  def testNat: Unit = {
    import Nat._
    println( (_2 + _2): _4 )
    println( (_3 + _0): _3 )
    println( (_0 + _2): _2 )
    println( (_1 + _2): _3 )
    println( (_2 * _3): _6 )
    println( (_2 ^ _3): _8 )
    println( (_3 ^ _2): _9 )
    println( _1._0: _10 )
    println( _1._6: (_8 # * [_2]) )

    implicitly[_1._6 =:= (_4 # * [_4])]
    implicitly[(_1._5 # ++) =:= (_4 # * [_4])]
    implicitly[(_1 # * [_10] # + [_6]) =:= (_4 # * [_4])]

    val x: List[List[List[String]]] = (null: _3#Fold[Any, List, String])
  }

  @Test
  def testMacro: Unit = {
    val _0 = Nat(0)
    val _1: Nat._1 = Nat(1)
    val _2 = Nat(2)
    val _3 = Nat(3)
    var i = 42
    val _42 = Nat(i)

    implicitly[_0.type <:< Nat._0]
    implicitly[_1.type <:< Nat._1]
    implicitly[_2.type <:< Nat._2]
    implicitly[_3.type <:< Nat._3]
    implicitly[_42.type <:< Nat]

    assertEquals(Nat._0, _0)
    assertEquals(Nat._1, _1)
    assertEquals(Nat._2, _2)
    assertEquals(Nat._3, _3)
    assertEquals(Nat._4 * Nat._10 + Nat._2, _42)
  }
}
