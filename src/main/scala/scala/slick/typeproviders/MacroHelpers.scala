package scala.slick.typeproviders

import scala.slick.schema.naming.Naming
import scala.reflect.api.Universe

abstract class MacroHelpers(val contextUtils: ContextUtils, val configFileName: String) extends ConfigHandler with TreeGeneratorUtils with TreeGeneratorCore {
  val universe: Universe
}
