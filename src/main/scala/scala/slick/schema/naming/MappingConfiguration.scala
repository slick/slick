package scala.slick.schema.naming

import com.typesafe.config._
import scala.slick.SlickException

class MappingConfiguration(val namingConf: Config) {
  def mergeWithFallback(config: Config, path: String): Config = {
    try {
      val mergedConfig = config.getConfig(path)
      mergedConfig.withFallback(config)
    } catch {
      case e: ConfigException.Missing => config
    }
  }

  def getRule(config: Config)(key: String): Rule = {
    import scala.collection.JavaConversions.collectionAsScalaIterable
    val v = config.getValue(key)
    import ConfigValueType._
    v.valueType() match {
      case STRING => ConstantRule(config.getString(key))
      case LIST => CompositeRule(collectionAsScalaIterable(config.getStringList(key)).toList)
      case t @ _ => throw new SlickException(s"Type for '$key' is ${t.toString} but String or List was expected!")
    }
  }

  def getRuleForTable(key: String)(table: String): Rule = {
    val tableConfig = mergeWithFallback(namingConf, s"custom.$table")
    getRule(tableConfig)(key)
  }

  def getRuleForTableModule(table: String): Rule = {
    getRuleForTable("table-module")(table)
  }

  def getRuleForCaseClass(table: String): Rule = {
    getRuleForTable("case-class")(table)
  }

  def getRuleForField(key: String)(table: String)(column: String): Rule = {
    val tableConfig = mergeWithFallback(namingConf, s"custom.$table")
    val columnConfig = mergeWithFallback(tableConfig, s"custom.$table.custom.$column")
    getRule(columnConfig)(key)
  }

  def getRuleForCaseField(table: String)(column: String): Rule = {
    getRuleForField("case-field")(table)(column)
  }

  def getRuleForModuleField(table: String)(column: String): Rule = {
    getRuleForField("module-field")(table)(column)
  }
}