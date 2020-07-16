package slick.util

import scala.language.implicitConversions

import slick.ast.{Node, Symbol, SymbolNamer}

object QueryInterpolator {
  implicit def queryInterpolator(s: StringContext): QueryInterpolator = new QueryInterpolator(s)
}

trait InterpolationContext {
  def sqlBuilder: SQLBuilder
  def quoteIdentifier(s: String): String
  def symbolName: SymbolNamer
  def expr(n: Node): Unit
  def skipParens: Boolean
  def withSkipParens[U](b: Boolean)(f: => U): U
}

class QueryInterpolator(context: StringContext) {
  /**
   * String interpolation using the "b" prefix to build SQL statements. In
   * general, the generated string is passed to "sqlBuilder.+=". When preceded
   * by '\', the following characters have special meaning:
   *
   * <ul>
   *   <li>'(': if(!skipParens) sqlBuilder += '('</li>
   *   <li>')': if(!skipParens) sqlBuilder += ')'</li>
   *   <li>'[': Add a '(' plus newline and increase indentation</li>
   *   <li>'[': Decrease indentation and add a newline plus ')'</li>
   *   <li>'{': Like '[' but only if(!skipParens)</li>
   *   <li>'}': Like ']' but only if(!skipParens)</li>
   *   <li>'n': Add a newline plus indentation</li>
   * </ul>
   *
   * If 'slick.sqlIndent' is not set in application.conf, no newlines are inserted and indentation
   * is replaced by a single space character.
   *
   * Variables inside the string can be prefixed by another symbol before the
   * standard '$' escape character to modify their meaning:
   *
   * <ul>
   *   <li>'&#96;' (before a String variable s): sqlBuilder += quoteIdentifier(s)</li>
   *   <li>'&#96;' (before a Symbol variable s): sqlBuilder += symbolName(s)</li>
   *   <li>'!' (before a Node variable n): expr(n, true)</li>
   *   <li>no prefix (before a Node variable n): expr(n)</li>
   *   <li>no prefix (before a String variable s): sqlBuilder += s</li>
   *   <li>no prefix (before an AnyVal variable v): sqlBuilder += String.valueOf(v)</li>
   * </ul>
   *
   * The identifiers sqlBuilder, skipParens, expr, quoteIdentifier and
   * symbolName are resolved *dynamically* at the call site.
   */
  def b(args: Any*)(implicit ic: InterpolationContext): Unit = {

    def appendString(str: String): Unit = {
      val len = str.length
      var pos = 0
      while(pos < len) {
        str.charAt(pos) match {
          case '\\' =>
            pos += 1
            if(pos < len) {
              str.charAt(pos) match {
                case c2 @ ('(' | ')') => // optional parentheses
                  if(!ic.skipParens) ic.sqlBuilder += c2
                case '{' => // optional open parentheses with indent
                  if(!ic.skipParens) {
                    ic.sqlBuilder += '('
                    ic.sqlBuilder.newLineIndent()
                  }
                case '}' => // optional close parentheses with dedent
                  if(!ic.skipParens) {
                    ic.sqlBuilder.newLineDedent()
                    ic.sqlBuilder += ')'
                  }
                case '[' => // open parenthesis with indent
                  ic.sqlBuilder += '('
                  ic.sqlBuilder.newLineIndent()
                case ']' => // close parenthesis with dedent
                  ic.sqlBuilder.newLineDedent()
                  ic.sqlBuilder += ')'
                case 'n' =>
                  ic.sqlBuilder.newLineOrSpace()
                case c2 =>
                  throw new IllegalArgumentException("Invalid escaped character '"+c2+"' in literal \""+str+"\"")
              }
            }
          case c => ic.sqlBuilder += c
        }
        pos += 1
      }
    }

    val pit = context.parts.iterator
    val ait = args.iterator

    while(ait.hasNext) {
      val s = pit.next()
      val av = ait.next()
      val len = s.length
      val marker = if(len == 0) '\u0000' else s.charAt(len-1)
      marker match {
        case '`' =>
          appendString(s.substring(0, len-1))
          av match {
            case av: String => ic.sqlBuilder += ic.quoteIdentifier(av)
            case av: Symbol => ic.sqlBuilder += ic.symbolName(av)
            case av =>
              throw new IllegalArgumentException(s"Unsupported type ${av.getClass.getName}. Must be Node or Symbol.")
          }
        case '!' =>
          appendString(s.substring(0, len-1))
          ic.withSkipParens(true)(ic.expr(av.asInstanceOf[Node]))
        case _ =>
          appendString(s)
          av match {
            case av: String => ic.sqlBuilder += av
            case av: Node => ic.withSkipParens(false)(ic.expr(av))
            case av => ic.sqlBuilder += String.valueOf(av)
          }
      }
    }
    appendString(pit.next())
  }
}
