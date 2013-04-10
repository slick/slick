package scala.slick.typeproviders.codegenerator

import scala.slick.typeproviders.CodeGenerator
import scala.reflect.runtime.universe._
import Flag._
import scala.slick.SlickException

trait TemplateComponent { self: CodeGenerator =>
  def generateCodeForClass(classDef: ClassDef): String = {
    val isCaseClass = classDef.mods.equals(Modifiers(CASE))
    if (!isCaseClass) {
      throw new SlickException("CodeGeneration not supported for non-case classes")
    }
    val ClassDef(_, TypeName(name), _, Template(_, _, fieldsCtor)) = classDef
    // We don't need constructor of case class, we're only interested in
    // fields. Also we don't consider the methods which are implemented for
    // that case class
    val fields = fieldsCtor collect {
      case ValDef(_, TermName(fieldName), tpe, _) => s"$fieldName: ${generateCodeForTypeTree(tpe)}"
    }
    s"${genIndent}case class $name(${fields.mkString(", ")})"
  }

  def generateCodeForModule(moduleDef: ModuleDef): String = {
    val ModuleDef(_, TermName(name), Template(List(tableSuper), _, methods)) = moduleDef
    val tableSuperCode = generateCodeForTypeTree(tableSuper)
    // we don't need constructor of case class
    val constructor = methods collectFirst {
      case DefDef(_, methodName, _, _, _, Block(List(Apply(_, ctorArgs)), _)) if methodName == nme.CONSTRUCTOR => {
        ctorArgs.map(generateCodeForTree).mkString("(", ", ", ")")
      }
    } getOrElse ""
    incIndent
    val methodsCode = methods collect {
      case method @ DefDef(_, methodName, _, _, _, rhs) if methodName != nme.CONSTRUCTOR => {
        generateCodeForDefDef(method)
      }
    }
    decIndent
    s"""${genIndent}object $name extends $tableSuperCode $constructor{
${methodsCode.mkString("\n")}
${genIndent}}"""
  }

}