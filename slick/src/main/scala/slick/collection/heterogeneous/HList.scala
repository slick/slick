package slick.collection.heterogeneous

import scala.language.higherKinds
import scala.language.experimental.macros
import scala.annotation.unchecked.{uncheckedVariance => uv}
import scala.reflect.macros.whitebox.Context
import slick.lifted.{MappedScalaProductShape, Shape, ShapeLevel}
import scala.reflect.ClassTag

/** A heterogenous list where each element has its own type. */
sealed abstract class HList extends Product {
  /** The type of this HList object */
  type Self <: HList
  /** The type of the first element */
  type Head
  /** The type of the tail of this HList */
  type Tail <: HList
  /** The type of a Fold operation on this HList */
  type Fold[U, F[_ <: HList, _ <: U] <: U, Z <: U] <: U

  // Some helper type projections to avoid type lambdas
  protected[this] type TailOf[T <: HList] = T#Tail
  protected[this] type HeadOf[T <: HList] = T#Head
  protected[this] type PrependHead[X <: HList, Z <: HList] = Z # :: [X#Head]
  /** Ignore X and increment Z by one (for counting with Fold) */
  protected[this] type IncrementForFold[X, Z <: Nat] = Succ[Z]

  /** Drop the first N elements from this HList and return the resulting type */
  type Drop[N <: Nat] = N#Fold[HList, TailOf, Self]
  /** Get the type of the Nth element of this HList */
  type Apply[N <: Nat] = HeadOf[Drop[N]] // should be Drop[N]#Head (work-around for SI-5294)
  /** Get the Nat type of the length of this HList */
  type Length = Fold[Nat, IncrementForFold, Nat._0]
  /** The type of prepending an element of type E to this HList */
  type :: [E] = HCons[E, Self]
  /** The type of concatenating another HList with this HList */
  type ::: [L <: HList] = L#Fold[HList, PrependHead, Self]

  /** Get the first element, or throw a NoSuchElementException if this HList is empty. */
  def head: Head
  /** Get the tail of the list, or throw a NoSuchElementException if this HList is empty. */
  def tail: Tail
  /** Return this HList typed as `Self`/ */
  def self: Self
  /** Fold the elements of this HList. */
  def fold[U, F[_ <: HList, _ <: U] <: U, Z <: U](f: TypedFunction2[HList, U, U, F], z: Z): Fold[U, F, Z]
  /** Check if this HList is non-empty. */
  def nonEmpty: Boolean
  /** Convert this HList to a `List[Any]`. */
  def toList: List[Any]

  /** Check if this list is empty. */
  final def isEmpty = !nonEmpty

  /** Get the length of this list as a `Nat`. */
  @inline final def length: Length = Nat._unsafe[Length](productArity)
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
  @inline final def :: [@specialized E](elem: E): :: [E] = new HCons[E, Self](elem, this.asInstanceOf[Self])
  /** Concatenate another HList to this HList, returning a new HList. */
  final def ::: [L <: HList](l: L): ::: [L] = l.fold[HList, PrependHead, Self](
    new TypedFunction2[HList, HList, HList, PrependHead] {
      def apply[P1 <: HList, P2 <: HList](p1: P1, p2: P2) = p1.head :: p2
    }, self)

  /** Drop the first `n` elements from this HList. */
  @inline final def drop [N <: Nat](n: N): Drop[N] = drop(n.value).asInstanceOf[Drop[N]]
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

  @inline final def _unsafeApply [N <: Nat](i: Int): Apply[N] = productElement(i).asInstanceOf[Apply[N]]
  /** Return the nth element from this HList, using the correct return type. */
  @inline final def apply [N <: Nat](n: N): Apply[N] = _unsafeApply[N](n.value)
  /** Return the nth element from this HList, using the correct return type if n is a literal, otherwise Any. */
  final def apply(n: Int): Any = macro HListMacros.applyImpl

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
        case h: HList =>
          b.append("(").append(v).append(")")
        case _ =>
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

final object HList {
  import syntax._

  final class HListShape[Level <: ShapeLevel, M <: HList, U <: HList : ClassTag, P <: HList](val shapes: Seq[Shape[_ <: ShapeLevel, _, _, _]]) extends MappedScalaProductShape[Level, HList, M, U, P] {
    def buildValue(elems: IndexedSeq[Any]) = elems.foldRight(HNil: HList)(_ :: _)
    def copy(shapes: Seq[Shape[_ <: ShapeLevel, _, _, _]]) = new HListShape(shapes)
  }
  implicit def hnilShape[Level <: ShapeLevel] = new HListShape[Level, HNil.type, HNil.type, HNil.type](Nil)
  implicit def hconsShape[Level <: ShapeLevel, M1, M2 <: HList, U1, U2 <: HList, P1, P2 <: HList](implicit s1: Shape[_ <: Level, M1, U1, P1], s2: HListShape[_ <: Level, M2, U2, P2]) =
    new HListShape[Level, M1 :: M2, U1 :: U2, P1 :: P2](s1 +: s2.shapes)
}
// Separate object for macro impl to avoid dependency of companion class on scala.reflect, see https://github.com/xeno-by/sbt-example-paradise210/issues/1#issuecomment-21021396
final object HListMacros{
  def applyImpl(ctx: Context { type PrefixType = HList })(n: ctx.Expr[Int]): ctx.Expr[Any] = {
    import ctx.universe._
    val _Succ = typeOf[Succ[_]].typeSymbol
    val _Zero = reify(Zero).tree
    n.tree match {
      case t @ Literal(Constant(v: Int)) =>
        val tt = (1 to v).foldLeft[Tree](SingletonTypeTree(_Zero)) { case (z, _) =>
          AppliedTypeTree(Ident(_Succ), List(z))
        }
        ctx.Expr(
          Apply(
            TypeApply(
              Select(ctx.prefix.tree, TermName("_unsafeApply")),
              List(tt)
            ),
            List(t)
          )
        )
      case _ => reify(ctx.prefix.splice.productElement(n.splice))
    }
  }
}

/** A cons cell of an `HList`, containing an element type and the element */
final class HCons[@specialized +H, +T <: HList](val head: H, val tail: T) extends HList {
  type Self = HCons[H @uv, T @uv]
  type Head = H @uv
  type Tail = T @uv
  type Fold[U, F[_ <: HList, _ <: U] <: U, Z <: U] = F[Self @uv, (T @uv)#Fold[U, F, Z]]

  def self = this
  def fold[U, F[_ <: HList, _ <: U] <: U, Z <: U](f: TypedFunction2[HList, U, U, F], z: Z): Fold[U, F, Z] @uv =
    f.apply[Self, T#Fold[U, F, Z]](self, tail.fold[U, F, Z](f, z))
  def toList: List[Any] = head :: tail.toList
  def nonEmpty = true
}

object HCons {
  def unapply[H, T <: HList](l: HCons[H, T]) = Some((l.head, l.tail))
}

/** The empty `HList` */
final object HNil extends HList {
  type Self = HNil.type
  type Head = Nothing
  type Tail = Nothing
  type Fold[U, F[_ <: HList, _ <: U] <: U, Z <: U] = Z

  def self = HNil
  def head = throw new NoSuchElementException("HNil.head")
  def tail = throw new NoSuchElementException("HNil.tail")
  def fold[U, F[_ <: HList, _ <: U] <: U, Z <: U](f: TypedFunction2[HList, U, U, F], z: Z) = z
  def toList = Nil
  def nonEmpty = false
}
