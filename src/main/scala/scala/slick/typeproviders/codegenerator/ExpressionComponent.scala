package scala.slick.typeproviders.codegenerator

import scala.slick.typeproviders.CodeGenerator
import scala.reflect.runtime.universe._

trait ExpressionComponent { self: CodeGenerator =>
  def generateCodeForTree(tree: Tree): String =
    generateCodeForTree(tree, None)

  def generateCodeForTree(tree: Tree, parent: Option[Tree]): String = {
    @inline def rec(t: Tree): String = generateCodeForTree(t, Some(tree))
    tree match {
      case Apply(Select(qualifier, name), args) if parent.collect { case a @ Apply(_, _) => a }.isEmpty => {
        val operand = generateCodeForTree(qualifier)
        val op = generateCodeForName(name)
        val arg = args match {
          case List(elem) => rec(elem)
          case list => list.map(rec).mkString("(", ", ", ")")
        }
        s"$operand $op $arg"
      }
      case Apply(fun, args) =>
        s"${rec(fun)}(${args.map(rec).mkString(", ")})"
      case Select(qualifier, name) =>
        s"${rec(qualifier)}.${generateCodeForName(name)}"
      case Ident(ident) =>
        generateCodeForName(ident)
      case Function(List(ValDef(_, TermName(variable), _, _)), body) =>
        // not complete. doesn't the case of having more than input
        s"$variable => (${rec(body)})"
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
    if (name equals nme.CONSTRUCTOR)
      ""
    else
      generateCodeForName(name.toString)
  }
}