package scala.slick.typeproviders.codegenerator

import scala.slick.typeproviders.CodeGenerator
import scala.reflect.runtime.universe._

trait DefComponent { self: CodeGenerator =>
  def generateCodeForValDef(valDef: ValDef): String = {
    val ValDef(_, TermName(fieldName), tpe, rhs) = valDef
    val typeCode = generateCodeForTypeTree(tpe)
    val typePart = if (tpe.isEmpty) "" else ": $typeCode"
    val fieldNameCode = generateCodeForName(fieldName)
    s"${genIndent}val $fieldNameCode$typePart" + {
      if (rhs.isEmpty) ""
      else " = " + generateCodeForTree(rhs)
    }
  }

  def generateCodeForDefDef(defDef: DefDef): String = {
    val DefDef(_, TermName(methodName), _, _, _, rhs) = defDef
    val methodNameCode = generateCodeForName(methodName)
    s"${genIndent}def $methodNameCode" + {
      if (rhs.isEmpty) ""
      else " = " + generateCodeForTree(rhs)
    }
  }

}