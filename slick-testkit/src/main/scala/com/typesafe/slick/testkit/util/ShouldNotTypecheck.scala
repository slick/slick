package com.typesafe.slick.testkit.util

import scala.language.experimental.macros
import scala.reflect.macros.{Context, TypecheckException}
import scala.util.control.NonFatal
import java.util.regex.Pattern

/**
 * A macro that ensures that a code snippet does not typecheck.
 */
object ShouldNotTypecheck {
  def apply(code: String): Unit = macro applyImplNoExp
  def apply(code: String, expected: String): Unit = macro applyImpl

  def applyImplNoExp(ctx: Context)(code: ctx.Expr[String]) = applyImpl(ctx)(code, null)

  def applyImpl(ctx: Context)(code: ctx.Expr[String], expected: ctx.Expr[String]): ctx.Expr[Unit] = {
    import ctx.universe._

    val Expr(Literal(Constant(codeStr: String))) = code
    val (expPat, expMsg) = expected match {
      case null => (null, "Expected some error.")
      case Expr(Literal(Constant(s: String))) =>
        (Pattern.compile(s, Pattern.CASE_INSENSITIVE), "Expected error matching: "+s)
    }

    try ctx.typeCheck(ctx.parse("{ "+codeStr+" }")) catch { case e: TypecheckException =>
      val msg = e.getMessage
      if((expected ne null) && !(expPat.matcher(msg)).matches)
        ctx.abort(ctx.enclosingPosition, "Type-checking failed in an unexpected way.\n"+
          expMsg+"\nActual error: "+msg)
      else return reify(())
    }

    ctx.abort(ctx.enclosingPosition, "Type-checking succeeded unexpectedly.\n"+expMsg)
  }
}
