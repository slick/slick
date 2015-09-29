package slick.util

import scala.reflect.macros.blackbox.Context
import scala.reflect.NameTransformer
import scala.collection.mutable.ListBuffer

object MacroSupportInterpolationImpl {
  def b(ctx: Context)(args: ctx.Expr[Any]*): ctx.Expr[Unit] = {
    import ctx.universe._

    val nodeSymbol = ctx.mirror.staticClass("slick.ast.Node")
    val nodeType = nodeSymbol.toType
    val stringType = definitions.StringClass.toType
    val symbolType = ctx.mirror.staticClass("slick.ast.Symbol").toType

    val skipParens = Ident(TermName("skipParens"))
    val sqlBuilder = Ident(TermName("sqlBuilder"))
    def quoteIdentifier(t: Tree) = Apply(Ident(TermName("quoteIdentifier")), List(t))
    def symbolName(t: Tree) = Apply(Ident(TermName("symbolName")), List(t))
    def toStr(t: Tree) = Apply(Select(Ident(definitions.StringClass.companion), TermName("valueOf")), List(t))
    def append(t: Tree) = Apply(Select(sqlBuilder, TermName("+=").encodedName), List(t))

    def appendString(str: String): List[Tree] = {
      val exprs = new ListBuffer[Tree]
      val sb = new StringBuilder
      val len = str.length
      var pos = 0
      def flushSB: Unit = if(!sb.isEmpty) {
        exprs += append(Literal(Constant(sb.toString)))
        sb.clear()
      }
      while(pos < len) {
        str.charAt(pos) match {
          case '\\' =>
            pos += 1
            if(pos < len) {
              str.charAt(pos) match {
                case c2 @ ('(' | ')') => // optional parentheses
                  flushSB
                  exprs += If(
                    Select(skipParens, TermName(NameTransformer.encode("unary_!"))),
                    append(Literal(Constant(c2))),
                    ctx.universe.EmptyTree
                  )
                case '{' => // optional open parentheses with indent
                  flushSB
                  exprs += If(
                    Select(skipParens, TermName(NameTransformer.encode("unary_!"))),
                    Block(List(
                      append(Literal(Constant('('))),
                      Select(sqlBuilder, TermName("newLineIndent"))
                    ), Literal(Constant(()))),
                    ctx.universe.EmptyTree
                  )
                case '}' => // optional close parentheses with dedent
                  flushSB
                  exprs += If(
                    Select(skipParens, TermName(NameTransformer.encode("unary_!"))),
                    Block(List(
                      Select(sqlBuilder, TermName("newLineDedent")),
                      append(Literal(Constant(')')))
                    ), Literal(Constant(()))),
                    ctx.universe.EmptyTree
                  )
                case '[' => // open parenthesis with indent
                  sb append '('
                  flushSB
                  exprs += Select(sqlBuilder, TermName("newLineIndent"))
                case ']' => // close parenthesis with dedent
                  flushSB
                  exprs += Select(sqlBuilder, TermName("newLineDedent"))
                  sb append ')'
                case 'n' =>
                  flushSB
                  exprs += Select(sqlBuilder, TermName("newLineOrSpace"))
                case c2 =>
                  ctx.abort(ctx.enclosingPosition, "Invalid escaped character '"+c2+"' in literal \""+str+"\"")
              }
            }
          case c => sb append c
        }
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
          exprs += Apply(Ident(TermName("expr")), List(a, Literal(Constant(true))))
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
              Apply(Ident(TermName("expr")), List(a, Literal(Constant(false))))
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
