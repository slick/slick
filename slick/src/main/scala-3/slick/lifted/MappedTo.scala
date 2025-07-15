package slick.lifted

import scala.quoted._
import scala.util.control.NonFatal

/** An isomorphism between two types that can be used for mapped column types. */
class Isomorphism[A, B](val map: A => B, val comap: B => A)

trait MappedToBase extends Any {
  type Underlying
  def value: Underlying
}

object MappedToBase {
  inline implicit def mappedToIsomorphism[T, E <: MappedTo[T]]: Isomorphism[E, T] =
    ${mappedToIsomorphismMacroImpl[T, E]}

  def mappedToIsomorphismMacroImpl[T: Type, E <: MappedTo[T]: Type](using Quotes): Expr[Isomorphism[E, T]] = {
    import quotes.reflect._    
    def makeApply(v: Expr[T])(using Quotes): Expr[E] = {
      import quotes.reflect._
      val sym = TypeRepr.of[E].typeSymbol
      Apply(Select(New(TypeTree.of[E]), sym.primaryConstructor), List(v.asTerm)).asExprOf[E]
    }
    val cons = '{(v: T) => ${ makeApply('v)}}

    val res = '{ new Isomorphism[E, T](_.value, $cons) }
    res
  }
}

/** The base type for automatically mapped column types.
  * Extending this type (with a type parameter ``T`` which is already a
  * supported column type) lets you use your custom type as a column
  * type in the Lifted Embedding. You must provide a constructor that
  * takes a single value of the underlying type (same restriction as
  * for value classes). */
trait MappedTo[T] extends Any with MappedToBase {
  type Underlying = T
  def value: T
}
