package scala.slick.test.collection.heterogenous

import org.junit.Test
import scala.slick.collection.heterogenous._
import syntax._
import org.junit.Assert._

class HListTest {
  @Test
  def testHList {
    val l1 = 42 :: "foo" :: Some(1.0) :: "bar" :: HNil
    val l1a = l1.head
    val l1b = l1.tail.head
    val l1c = l1.tail.tail.head
    val l1d = l1.tail.tail.tail.head

    assertEquals( 42, l1(Nat._0) )
    assertEquals( "bar", l1(Nat._3) )

    println(l1)
    val l2 = l1.drop(Nat._3)
    println(l2)
    val e0: Int = l1(Nat._0)
    val e2a: Option[Double] = l1.apply(Nat._2)
    val e2b: Option[Double] = l1.drop(Nat._2).head

    val x1 = null : l1.type#Tail#Tail#Tail#Head
    val x2 = null : Nat._3#Fold[HList, ({ type L[X <: HList] = X#Tail })#L, l1.type#Self]#Head
    val x3: Option[Double] = null : l1.type#Drop[Nat._2]#Head

    implicitly[l1.Length =:= Nat._4]
    implicitly[l2.Length =:= Nat._1]

    println((l1.length, l2.length))

    val l3a = "foo" :: 42 :: HNil
    val l3b = true :: "baz" :: Some(1.0) :: HNil
    val l3 = l3a ::: l3b
    println(l3 : String :: Int :: Boolean :: String :: Some[Double] :: HNil)

    val l4 = new HCons(42, new HCons(10.0d, HNil))
    println(l4.getClass)
    println(l4.tail.getClass)
    ;{
      import scala.slick.lifted.{Shape,FlatShapeLevel}
      import scala.slick.driver.JdbcDriver.simple._

      type HList10[T <: HList] = HCons[String,HCons[String,HCons[String,HCons[String,HCons[String,HCons[String,HCons[String,HCons[String,HCons[String,HCons[String,T]]]]]]]]]]
      type HList100[T <: HList] = HList10[HList10[HList10[HList10[HList10[HList10[HList10[HList10[HList10[HList10[T]]]]]]]]]]

      // making sure compile times for long hlists are sane
      // measured roughly:
      // 30 seconds for size 1000
      // 12 seconds for size 500
      //  9 seconds for size 400
      //  7 seconds for size 300

      type HListFull = HList100[HList100[HList100[HList100[HNil]]]]
      implicitly[Shape[_ <: FlatShapeLevel, HListFull, HListFull, _]] 
    }
  }
}
