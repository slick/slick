package scala.slick.typeproviders

import language.experimental.macros

object TypeProvider {
  def generateCode(configurationFileName: String) = new CodeGenerator(configurationFileName).generateCode()
}