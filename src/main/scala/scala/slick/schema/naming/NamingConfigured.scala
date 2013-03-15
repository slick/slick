package scala.slick.schema.naming

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigException
import scala.slick.SlickException

class NamingConfigured(config: Config) extends Naming {
  import NamingConfigured._
  val mapping = {
    val namingConf = try {
      config.getConfig("naming")
    } catch {
      case e: ConfigException.Missing => ConfigFactory.empty
      case _: ConfigException => throw new SlickException("Invalid naming configuration")
    }
    new MappingConfiguration(namingConf.withFallback(defaultConfiguration))

  }
  override def tableSQLToModule(table: String): String = mapping.getRuleForTableModule(table)(table)
  override def tableSQLToCase(table: String): String = mapping.getRuleForCaseClass(table)(table)
  override def columnSQLToCaseField(table: String)(column: String): String = mapping.getRuleForCaseField(table)(column)(column)
  override def columnSQLToModuleField(table: String)(column: String): String = mapping.getRuleForModuleField(table)(column)(column)
}

object NamingConfigured {
  val defaultConfiguration = ConfigFactory.parseString("""
table-module = [lowercase, capitalize, camelize]
case-class = [lowercase, capitalize, camelize, singularize]
case-field = [lowercase, camelize]
module-field = [lowercase, camelize]
      """)
}

object NamingDefault extends NamingConfigured(ConfigFactory.empty)
