package scala.slick.typeproviders

import scala.slick.schema.naming.Naming
import scala.reflect.api.Universe

abstract class MacroHelpers(val contextUtils: ContextUtils, val configFileName: String) extends ConfigHandler with TreeGeneratorUtils with TreeGeneratorCore {
  val universe: Universe

  // workaround for compatibility of 2.10 and macro paradise
  object TermName {
    def apply(s: String): universe.TermName = universe.newTermName(s)
    def unapply(t: universe.TermName): Option[String] = Some(t.toString)
  }
  object TypeName {
    def apply(s: String): universe.TypeName = universe.newTypeName(s)
    def unapply(t: universe.TypeName): Option[String] = Some(t.toString)
  }
}