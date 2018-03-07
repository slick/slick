package slick.collection.heterogeneous

import scala.language.higherKinds
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

/** Natural numbers for indexing in HLists.
  *
  * All type-level computations are done with Church Numerals.
  * Value-level computations at run-time are done directly on the underlying
  * `Int` values, similar to `java.lang.Integer`. */
abstract class Nat {
  /** The type of this Nat object. */
  type Self <: Nat
  /** The type of a folding operation on this Nat. */
  type Fold[U, F[_ <: U] <: U, Z <: U] <: U
  /** Add another Nat to this one. */
  type + [_ <: Nat] <: Nat
  /** Multiply another Nat with this one. */
  type * [_ <: Nat] <: Nat
  type Flip_^ [_ <: Nat] <: Nat
  /** Raise this Nat to the exponent given by another Nat. */
  type ^ [T <: Nat] = T # Flip_^[Self]
  /** Increment this Nat. */
  type ++ = Succ[Self]

  /** Increment this Nat. */
  def ++ = Nat._unsafe[++](value+1)
  /** Add another Nat to this one. */
  def + [T <: Nat](n: T): +[T] = Nat._unsafe[+[T]](value + n.value)
  /** Multiply another Nat with this one. */
  def * [T <: Nat](n: T): *[T] = Nat._unsafe[*[T]](value * n.value)
  /** Raise this Nat to the exponent given by another Nat. */
  def ^ [T <: Nat](n: T): ^[T] = Nat._unsafe[^[T]](scala.math.pow(value, n.value).asInstanceOf[Int])
  /** The equivalent Int value for this Nat. */
  def value: Int
  /** This Nat, typed as Self. */
  def self: Self

  override def toString = value.toString
  override def equals(o: Any) = o match {
    case n: Nat => value == n.value
    case _ => false
  }
  override def hashCode = value

  /** Multiply this Nat by 10. */
  type _0 = Self # * [Nat._10]
  /** Multiply this Nat by 10, then add 1. */
  type _1 = _0 # + [Nat._1]
  /** Multiply this Nat by 10, then add 2. */
  type _2 = _0 # + [Nat._2]
  /** Multiply this Nat by 10, then add 3. */
  type _3 = _0 # + [Nat._3]
  /** Multiply this Nat by 10, then add 4. */
  type _4 = _0 # + [Nat._4]
  /** Multiply this Nat by 10, then add 5. */
  type _5 = _0 # + [Nat._5]
  /** Multiply this Nat by 10, then add 6. */
  type _6 = _0 # + [Nat._6]
  /** Multiply this Nat by 10, then add 7. */
  type _7 = _0 # + [Nat._7]
  /** Multiply this Nat by 10, then add 8. */
  type _8 = _0 # + [Nat._8]
  /** Multiply this Nat by 10, then add 9. */
  type _9 = _0 # + [Nat._9]
  /** Multiply this Nat by 10. */
  def _0 = (self * Nat._10): _0
  /** Multiply this Nat by 10, then add 1. */
  def _1 = (_0 + Nat._1): _1
  /** Multiply this Nat by 10, then add 2. */
  def _2 = (_0 + Nat._2): _2
  /** Multiply this Nat by 10, then add 3. */
  def _3 = (_0 + Nat._3): _3
  /** Multiply this Nat by 10, then add 4. */
  def _4 = (_0 + Nat._4): _4
  /** Multiply this Nat by 10, then add 5. */
  def _5 = (_0 + Nat._5): _5
  /** Multiply this Nat by 10, then add 6. */
  def _6 = (_0 + Nat._6): _6
  /** Multiply this Nat by 10, then add 7. */
  def _7 = (_0 + Nat._7): _7
  /** Multiply this Nat by 10, then add 8. */
  def _8 = (_0 + Nat._8): _8
  /** Multiply this Nat by 10, then add 9. */
  def _9 = (_0 + Nat._9): _9
}

object Nat {
  def _unsafe[T <: Nat](value: Int) =
    (if(value < cached.length) cached(value) else new Succ(value)).asInstanceOf[T]
  /** The cached Nat type for 0. */
  type _0 = Zero.type
  /** The cached Nat type for 1. */
  type _1 = _0 # ++
  /** The cached Nat type for 2. */
  type _2 = _1 # ++
  /** The cached Nat type for 3. */
  type _3 = _2 # ++
  /** The cached Nat type for 4. */
  type _4 = _3 # ++
  /** The cached Nat type for 5. */
  type _5 = _4 # ++
  /** The cached Nat type for 6. */
  type _6 = _5 # ++
  /** The cached Nat type for 7. */
  type _7 = _6 # ++
  /** The cached Nat type for 8. */
  type _8 = _7 # ++
  /** The cached Nat type for 9. */
  type _9 = _8 # ++
  /** The cached Nat type for 10. */
  type _10 = _9 # ++
  /** The cached Nat value for 0. */
  val _0: _0 = Zero
  /** The cached Nat value for 1. */
  val _1 = new Succ(1).asInstanceOf[_1]
  /** The cached Nat value for 2. */
  val _2 = new Succ(2).asInstanceOf[_2]
  /** The cached Nat value for 3. */
  val _3 = new Succ(3).asInstanceOf[_3]
  /** The cached Nat value for 4. */
  val _4 = new Succ(4).asInstanceOf[_4]
  /** The cached Nat value for 5. */
  val _5 = new Succ(5).asInstanceOf[_5]
  /** The cached Nat value for 6. */
  val _6 = new Succ(6).asInstanceOf[_6]
  /** The cached Nat value for 7. */
  val _7 = new Succ(7).asInstanceOf[_7]
  /** The cached Nat value for 8. */
  val _8 = new Succ(8).asInstanceOf[_8]
  /** The cached Nat value for 9. */
  val _9 = new Succ(9).asInstanceOf[_9]
  /** The cached Nat value for 10. */
  val _10 = new Succ(10).asInstanceOf[_10]
  private[this] val cached = Array(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10)

  /** Get a `Nat` for an `Int`. If the Int is a literal, the Nat will have
    * the proper type, otherwise only the supertype `Nat`. */
  def apply(i: Int): Nat = macro Nat.applyImpl
  def applyImpl(ctx: Context)(i: ctx.Expr[Int]): ctx.Expr[Nat] = {
    import ctx.universe._
    val _Nat = typeOf[Nat].typeSymbol.companion
    val _Succ = typeOf[Succ[_]].typeSymbol
    val _Zero = reify(Zero).tree

    i.tree match {
      case Literal(Constant(v: Int)) =>
        val tt = (1 to v).foldLeft[Tree](SingletonTypeTree(_Zero)) { case (z, _) =>
          AppliedTypeTree(Ident(_Succ), List(z))
        }
        ctx.Expr(
          Apply(
            TypeApply(
              Select(Ident(_Nat), TermName("_unsafe")),
              List(tt)),
            List(Literal(Constant(v)))))
      case _ => reify(Nat._unsafe[Nat](i.splice))
    }
  }
}

/** The zero value and type for `Nat` */
final object Zero extends Nat {
  type Self = Zero.type
  type Fold[U, F[_ <: U] <: U, Z <: U] = Z
  type + [X <: Nat] = X
  type * [_ <: Nat] = Nat._0
  type Flip_^ [_ <: Nat] = Nat._1
  def value = 0
  def self = this
}

/** A successor of another `Nat` */
final class Succ[N <: Nat] private[heterogeneous] (val value: Int) extends Nat {
  type Self = Succ[N]
  type -- = N
  /** Implements (F^(N+1))(Z) or in other words replaces nested Succs by nested Fs */
  type Fold[U, F[_ <: U] <: U, Z <: U] = F[N#Fold[U, F, Z]]
  type + [X <: Nat] = Succ[N # + [X]]
  type * [X <: Nat] = (N # * [X]) # + [X]
  type Flip_^ [X <: Nat] = (N # Flip_^ [X]) # * [X]
  def -- : -- = Nat._unsafe[--](value-1)
  def self = this
}
