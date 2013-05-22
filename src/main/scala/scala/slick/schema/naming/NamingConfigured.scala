package scala.slick.schema.naming

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigException
import scala.slick.SlickException
import scala.slick.schema.QualifiedName
import scala.slick.schema.NamePart
import scala.slick.schema.ColumnName
import scala.slick.schema.TableName

/**
 * A class for retrieving the names for meta-models out of a naming configuration
 */
class NamingConfigured(val mapping: MappingConfiguration) extends Naming {
  @inline def getColumnName(name: QualifiedName): String = {
    name.getPartName(ColumnName)
  }

  @inline def getTableName(name: QualifiedName): String = {
    name.getPartName(TableName)
  }

  override def tableSQLToModule(name: QualifiedName): String = {
    val table = getTableName(name)
    mapping.getMappingForTableModule(table)
  }
  override def tableSQLToEntity(name: QualifiedName): String = {
    val table = getTableName(name)
    mapping.getMappingForCaseClass(table)
  }
  override def columnSQLToModuleField(name: QualifiedName): String = {
    val table = getTableName(name)
    val column = getColumnName(name)
    mapping.getMappingForModuleField(table)(column)
  }
  override def columnSQLToEntityField(name: QualifiedName): String = {
    val table = getTableName(name)
    val column = getColumnName(name)
    mapping.getMappingForCaseField(table)(column)
  }
}

object NamingConfigured {
  val defaultConfiguration = ConfigFactory.parseString("""table-module = [lowercase, capitalize, camelize]
case-class = [lowercase, capitalize, camelize, singularize]
case-field = [lowercase, camelize]
module-field = [lowercase, camelize]
      """)
}

object NamingDefault extends NamingConfigured(MappingConfiguration(ConfigFactory.empty))
