package scala.slick.typeproviders

import language.experimental.macros

object TypeProvider {
  type Db(configurationFileName: String = "configuration") = macro Macros.DbImpl
  
  def generateCode(configurationFileName: String) = new CodeGenerator(configurationFileName).generateCode()
}