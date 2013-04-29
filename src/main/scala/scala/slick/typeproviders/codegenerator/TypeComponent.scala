package scala.slick.typeproviders.codegenerator

import scala.slick.typeproviders.CodeGenerator
import scala.reflect.runtime.universe._

trait TypeComponent { self: CodeGenerator =>
  def generateCodeForTypeTree(tpt: Tree): String =
    tpt.toString

  def generateCodeForTypeDef(typeDef: TypeDef): String = {
    val TypeDef(_, TypeName(typeName), List(), typeType) = typeDef
    s"${genIndent}type $typeName = ${generateCodeForTypeTree(typeType)}"
  }
}