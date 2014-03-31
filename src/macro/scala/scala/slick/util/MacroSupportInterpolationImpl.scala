package scala.slick.util

import scala.reflect.macros.Context
import scala.reflect.NameTransformer
import scala.collection.mutable.ListBuffer

object MacroSupportInterpolationImpl {
  def b(ctx: Context)(args: ctx.Expr[Any]*): ctx.Expr[Unit] = {
    import ctx.universe._

    val nodeSymbol = ctx.mirror.staticClass("scala.slick.ast.Node")
    val nodeType = nodeSymbol.toType
    val stringType = definitions.StringClass.toType
    val symbolType = ctx.mirror.staticClass("scala.slick.ast.Symbol").toType

    val skipParens = Ident(newTermName("skipParens"))
    val sqlBuilder = Ident(newTermName("sqlBuilder"))
    def quoteIdentifier(t: Tree) = Apply(Ident(newTermName("quoteIdentifier")), List(t))
    def symbolName(t: Tree) = Apply(Ident(newTermName("symbolName")), List(t))
    def toStr(t: Tree) = Apply(Select(Ident(definitions.StringClass.companionSymbol), newTermName("valueOf")), List(t))
    def append(t: Tree) = Apply(Select(sqlBuilder, newTermName("+=").encodedName), List(t))

    def appendString(str: String): List[Tree] = {
      val exprs = new ListBuffer[Tree]
      val sb = new StringBuilder
      val len = str.length
      var pos = 0
      while(pos < len) {
        val c = str.charAt(pos)
        if(c == '\\') {
          pos += 1
          if(pos < len) {
            val c2 = str.charAt(pos)
            if(c2 == '(' || c2 == ')') {
              if(!sb.isEmpty) {
                exprs += append(Literal(Constant(sb.toString)))
                sb.clear()
              }
              exprs += If(
                Select(skipParens, newTermName(NameTransformer.encode("unary_!"))),
                append(Literal(Constant(c2))),
                ctx.universe.EmptyTree
              )
            } else sb append c
            //else ctx.abort(ctx.enclosingPosition, "Invalid escaped character '"+c2+"' in literal \""+str+"\"")
          }
        } else sb append c
        pos += 1
      }
      if(!sb.isEmpty)
        exprs += append(Literal(Constant(sb.toString)))
      exprs.toList
    }

    val Expr(Apply(_, List(Apply(_, parts)))) = ctx.prefix
    val pit = parts.map { case Literal(Constant(s: String)) => s }.iterator
    val ait = args.iterator
    val exprs = new ListBuffer[Tree]

    while(ait.hasNext) {
      val s = pit.next()
      val ae @ Expr(a) = ait.next()
      val len = s.length
      val marker = if(len == 0) '\u0000' else s.charAt(len-1)
      marker match {
        case '`' =>
          exprs ++= appendString(s.substring(0, len-1))
          if(ae.actualType <:< stringType)
            exprs += append(quoteIdentifier(a))
          else if(ae.actualType <:< symbolType)
            exprs += append(symbolName(a))
          else
            ctx.abort(ae.tree.pos, "Unknown type. Must be Node or Symbol.")
        case '!' =>
          exprs ++= appendString(s.substring(0, len-1))
          exprs += Apply(Ident(newTermName("expr")), List(a, Literal(Constant(true))))
        case _ =>
          exprs ++= appendString(s)
          //println("### tpe: "+ae.actualType)
          //println("### is String: "+(ae.actualType <:< stringType))
          //println("### is Node: "+(ae.actualType <:< nodeType))
          exprs += (
            if(ae.actualType <:< stringType)
              append(a)
            else if(ae.actualType <:< definitions.AnyValTpe)
              append(toStr(a))
            else if(ae.actualType <:< nodeType)
              Apply(Ident(newTermName("expr")), List(a, Literal(Constant(false))))
            else
              ctx.abort(ae.tree.pos, "Unknown type. Must be Node, String or AnyVal.")
          )
      }
    }
    exprs ++= appendString(pit.next())

    //println("### exprs: "+exprs)
    ctx.Expr[Unit](Block(exprs.toList,Literal(Constant(()))))
  }
}
