package scala.slick.lifted

import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.util.control.NonFatal

/** An isomorphism between two types that can be used for mapped column types. */
class Isomorphism[A, B](val map: A => B, val comap: B => A)

trait MappedToBase extends Any {
  type Underlying
  def value: Underlying
}

object MappedToBase {
  implicit def mappedToIsomorphism[E <: MappedToBase]: Isomorphism[E, E#Underlying] =
    macro mappedToIsomorphismMacroImpl[E]

  def mappedToIsomorphismMacroImpl[E <: MappedToBase](c: Context)(implicit e: c.WeakTypeTag[E]): c.Expr[Isomorphism[E, E#Underlying]] = {
    import c.universe._
    if(!(e.tpe <:< c.typeOf[MappedToBase]))
      c.abort(c.enclosingPosition, "Work-around for SI-8351 leading to illegal macro-invocation -- You should not see this message")
    implicit val eutag = c.TypeTag[E#Underlying](e.tpe.member(newTypeName("Underlying")).typeSignatureIn(e.tpe))
    val cons = c.Expr[E#Underlying => E](Function(
      List(ValDef(Modifiers(Flag.PARAM), newTermName("v"), /*Ident(eu.tpe.typeSymbol)*/TypeTree(), EmptyTree)),
      Apply(
        Select(New(TypeTree(e.tpe)), nme.CONSTRUCTOR),
        List(Ident(newTermName("v")))
      )
    ))
    val res = reify { new Isomorphism[E, E#Underlying](_.value, cons.splice) }
    try c.typeCheck(res.tree) catch { case NonFatal(ex) =>
      val p = c.enclosingPosition
      val msg = "Error typechecking MappedTo expansion: " + ex.getMessage
      println(p.source.path + ":" + p.line + ": " + msg)
      c.error(c.enclosingPosition, msg)
    }
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
