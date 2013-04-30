package scala.slick.typeproviders

import scala.slick.typeproviders.codegenerator.TemplateComponent
import scala.slick.typeproviders.codegenerator.DefComponent
import scala.slick.typeproviders.codegenerator.ExpressionComponent
import scala.slick.typeproviders.codegenerator.TypeComponent

class CodeGenerator(val configFileName: String) extends TemplateComponent
  with DefComponent with ExpressionComponent with TypeComponent {
  import scala.reflect.runtime.{ universe => runtimeUniverse }
  import runtimeUniverse._

  object TermName {
    def apply(s: String): runtimeUniverse.TermName = runtimeUniverse.newTermName(s)
    def unapply(t: runtimeUniverse.TermName): Option[String] = Some(t.toString)
  }
  object TypeName {
    def apply(s: String): runtimeUniverse.TypeName = runtimeUniverse.newTypeName(s)
    def unapply(t: runtimeUniverse.TypeName): Option[String] = Some(t.toString)
  }

  val macroHelper = new {
    val universe: runtimeUniverse.type = runtimeUniverse
  } with MacroHelpers(DefaultContextUtils, configFileName)

  private var indentLevel = 0

  def incIndent {
    indentLevel += 1
  }

  def decIndent {
    indentLevel -= 1
  }

  def genIndent: String = {
    val result = new StringBuffer
    for (i <- 1 to indentLevel) {
      result append "  "
    }
    result.toString
  }

  val tableTrees = macroHelper.generateTreeForTables
  val imports = macroHelper.getImports
  val connectionString = macroHelper.urlForConnection
  import macroHelper.{ userForConnection, passForConnection, slickDriverObject, jdbcClass }

  def generateCode(className: String = "GeneratedDb"): String = {
    incIndent
    val tableCode = tableTrees map generateCodeForTable
    val importsCode = imports map generateCodeForImport
    decIndent
    s"""
object $className {
${importsCode.mkString("\n")}

${tableCode.mkString("\n")}
}   
"""
  }

  def generateCodeForTable(tree: Tree): String = {
    tree match {
      case classDef: ClassDef =>
        // we're sure that it would be the generated case class for it
        generateCodeForClass(classDef)
      case moduleDef: ModuleDef =>
        generateCodeForModule(moduleDef)
      case typeDef: TypeDef =>
        generateCodeForTypeDef(typeDef)
      case valDef: ValDef =>
        generateCodeForValDef(valDef)
      case _ => tree.toString
    }
  }

  def generateCodeForImport(imp: Import): String = {
    s"$genIndent$imp"
  }
}

object DefaultContextUtils {
  var map = new collection.mutable.HashMap[String, Int]
  def freshName(name: String): String = {
    val index = map.get(name) match {
      case Some(i) => {
        map(name) = i + 1
        i
      }
      case None => {
        map(name) = 1
        0
      }
    }
    name + "$" + index
  }
}