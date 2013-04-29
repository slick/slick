package scala.slick.schema.naming

import com.typesafe.config._
import scala.slick.SlickException

/**
 * A class for dealing with naming part of configuration files
 */
class MappingConfiguration(val namingConf: Config) {
  def mergeWithFallback(config: Config, path: String): Config = {
    try {
      val mergedConfig = config.getConfig(path)
      mergedConfig.withFallback(config)
    } catch {
      case e: ConfigException.Missing => config
    }
  }

  def getMapping(config: Config)(kind: String)(key: String): String = {
    import scala.collection.JavaConversions.collectionAsScalaIterable
    val v = config.getValue(kind)
    import ConfigValueType._
    v.valueType() match {
      case STRING => config.getString(kind)
      case LIST => CompositeRule(collectionAsScalaIterable(config.getStringList(kind)).toList)(key)
      case t @ _ => throw new SlickException(s"Type for '$kind' is ${t.toString} but String or List was expected!")
    }
  }

  def getMappingForTable(key: String)(table: String): String = {
    val tableConfig = mergeWithFallback(namingConf, s"custom.$table")
    getMapping(tableConfig)(key)(table)
  }

  def getMappingForTableModule(table: String): String = {
    getMappingForTable("table-module")(table)
  }

  def getMappingForCaseClass(table: String): String = {
    getMappingForTable("case-class")(table)
  }

  def getMappingForField(key: String)(table: String)(column: String): String = {
    val tableConfig = mergeWithFallback(namingConf, s"custom.$table")
    val columnConfig = mergeWithFallback(tableConfig, s"custom.$table.custom.$column")
    getMapping(columnConfig)(key)(column)
  }

  def getMappingForCaseField(table: String)(column: String): String = {
    getMappingForField("case-field")(table)(column)
  }

  def getMappingForModuleField(table: String)(column: String): String = {
    getMappingForField("module-field")(table)(column)
  }
}

object MappingConfiguration {

  def apply(config: Config): MappingConfiguration = {
    import NamingConfigured._
    val namingConf = try {
      config.getConfig("naming")
    } catch {
      case e: ConfigException.Missing => ConfigFactory.empty
      case e: ConfigException => throw new SlickException("Invalid naming configuration", e)
    }
    new MappingConfiguration(namingConf.withFallback(defaultConfiguration))
  }

}