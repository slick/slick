package scala.slick.typeproviders.codegenerator

import scala.slick.typeproviders.CodeGenerator
import scala.reflect.runtime.universe._

trait ExpressionComponent { self: CodeGenerator =>
  def generateCodeForTree(tree: Tree): String = {
    @inline def rec(t: Tree): String = generateCodeForTree(t)
    tree match {
      case Apply(Select(qualifier, name), args) => {
        val operand = rec(qualifier)
        val op = generateCodeForName(name)
        val arg = args match {
          case List(elem) => generateCodeForTree(elem)
          case list => list.map(generateCodeForTree).mkString("(", ", ", ")")
        }
        s"$operand $op $arg"
      }
      case Apply(fun, args) =>
        // TODO consider the case of (a op b)
        s"${rec(fun)}(${args.map(rec).mkString(", ")})"
      case Select(qualifier, name) =>
        s"${rec(qualifier)}.${generateCodeForName(name)}"
      case Ident(ident) =>
        generateCodeForName(ident)
      case Function(_, Apply(fun, _)) =>
        // not complete. doesn't the case of having more than input
        s"${rec(fun)} _"
      case _ =>
        tree.toString
    }
  }

  def generateCodeForName(name: String): String = {
    name match {
      case "$times" => "*"
      case "$less$greater" => "<>"
      case "$tilde" => "~"
      case _ => name
    }
  }

  def generateCodeForName(name: Name): String = {
    generateCodeForName(name.toString)
  }
}