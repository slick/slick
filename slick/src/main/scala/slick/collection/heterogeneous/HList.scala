package slick.collection.heterogeneous

import scala.annotation.unchecked.uncheckedVariance as uv
import scala.reflect.ClassTag

import slick.lifted.{MappedScalaProductShape, Shape, ShapeLevel}

/** A heterogenous list where each element has its own type. */
sealed abstract class HList extends Product {
  /** The type of this HList object */
  type Self <: HList
  /** The type of the first element */
  type Head
  /** The type of the tail of this HList */
  type Tail <: HList

  /** The type of prepending an element of type E to this HList */
  type :: [E] = HCons[E, Self]

  /** Get the first element, or throw a NoSuchElementException if this HList is empty. */
  def head: Head
  /** Get the tail of the list, or throw a NoSuchElementException if this HList is empty. */
  def tail: Tail
  /** Return this HList typed as `Self`/ */
  def self: Self
  /** Check if this HList is non-empty. */
  def nonEmpty: Boolean
  /** Convert this HList to a `List[Any]`. */
  def toList: List[Any]

  /** Check if this list is empty. */
  final def isEmpty = !nonEmpty

  /** Get the length of this list. */
  @inline final def length: Int = productArity
  /** Get the length of this list as an `Int`. */
  final def productArity: Int = {
    var i = 0
    var h = this
    while(h.nonEmpty) {
      i += 1
      h = h.tail
    }
    i
  }

  /** Prepend an element to this HList, returning a new HList. */
  @inline final def :: [E](elem: E): :: [E] = new HCons[E, Self](elem, this.asInstanceOf[Self])

  /** Drop the first `n` elements from this HList. */
  final def drop(i: Int): HList = {
    var h = this
    var ii = i
    while(ii > 0) {
      ii -= 1
      h = h.tail
    }
    h
  }

  final def productElement(i: Int): Any = drop(i).head

  /** Evaluate a function for each element of this HList. */
  final def foreach(f: Any => Unit): Unit = {
    var h = this
    while(h.nonEmpty) {
      f(h.head)
      h = h.tail
    }
  }

  override final def toString = {
    val b = new StringBuffer
    foreach { v =>
      v match {
        case _: HList =>
          b.append("(").append(v).append(")")
        case _        =>
          b.append(v)
      }
      b.append(" :: ") }
    b.append("HNil").toString
  }

  override final lazy val hashCode: Int = toList.hashCode
  override final def equals(that: Any) = that match {
    case that: HList => toList == that.toList
    case _ => false
  }
  final def canEqual(that: Any) = that.isInstanceOf[HList]
}

object HList {
  import syntax.*

  final class HListShape[
    Level <: ShapeLevel,
    M <: HList,
    U <: HList : ClassTag,
    P <: HList
  ](val shapes: Seq[Shape[? <: ShapeLevel, ?, ?, ?]]) extends MappedScalaProductShape[Level, HList, M, U, P] {
    override def buildValue(elems: IndexedSeq[Any]): HList = elems.foldRight(HNil: HList)(_ :: _)
    override def copy(shapes: Seq[Shape[? <: ShapeLevel, ?, ?, ?]]): HListShape[Level, Nothing, U, Nothing] =
      new HListShape(shapes)
  }
  implicit def hnilShape[Level <: ShapeLevel]: HListShape[Level, HNil.type, HNil.type, HNil.type] =
    new HListShape[Level, HNil.type, HNil.type, HNil.type](Nil)
  implicit def hconsShape[
    Level <: ShapeLevel,
    M1,
    M2 <: HList,
    U1,
    U2 <: HList,
    P1,
    P2 <: HList
  ](implicit s1: Shape[? <: Level, M1, U1, P1],
    s2: HListShape[? <: Level, M2, U2, P2]): HListShape[Level, M1 :: M2, U1 :: U2, P1 :: P2] =
    new HListShape[Level, M1 :: M2, U1 :: U2, P1 :: P2](s1 +: s2.shapes)
}

/** A cons cell of an `HList`, containing an element type and the element */
final class HCons[+H, +T <: HList](val head: H, val tail: T) extends HList {
  type Self = HCons[H @uv, T @uv]
  type Head = H @uv
  type Tail = T @uv

  def self = this
  def toList: List[Any] = head :: tail.toList
  def nonEmpty = true
}

object HCons {
  def unapply[H, T <: HList](l: HCons[H, T]) = Some((l.head, l.tail))
}

/** The empty `HList` */
object HNil extends HList {
  type Self = HNil.type
  type Head = Nothing
  type Tail = Nothing

  def self = HNil
  def head = throw new NoSuchElementException("HNil.head")
  def tail = throw new NoSuchElementException("HNil.tail")
  override def toList: Nil.type = Nil
  def nonEmpty = false
}
