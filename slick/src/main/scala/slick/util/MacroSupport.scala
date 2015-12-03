package slick.util

import scala.language.experimental.macros
import scala.language.implicitConversions

object MacroSupport {
  implicit def macroSupportInterpolation(s: StringContext): MacroSupportInterpolation = new MacroSupportInterpolation(s)
}

class MacroSupportInterpolation(context: StringContext) {
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
   *   <li>'`' (before a String variable s): sqlBuilder += quoteIdentifier(s)</li>
   *   <li>'`' (before a Symbol variable s): sqlBuilder += symbolName(s)</li>
   *   <li>'!' (before a Node variable n): expr(n, true)</li>
   *   <li>no prefix (before a Node variable n): expr(n)</li>
   *   <li>no prefix (before a String variable s): sqlBuilder += s</li>
   *   <li>no prefix (before an AnyVal variable v): sqlBuilder += String.valueOf(v)</li>
   * </ul>
   *
   * The identifiers sqlBuilder, skipParens, expr, quoteIdentifier and
   * symbolName are resolved *dynamically* at the call site.
   */
  def b(args: Any*): Unit = macro MacroSupportInterpolationImpl.b
}
