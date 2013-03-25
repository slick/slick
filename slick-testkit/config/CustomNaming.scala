import scala.slick.schema.QualifiedName
import scala.slick.schema.naming._
import com.typesafe.config.ConfigFactory

class CustomNaming(mapping: MappingConfiguration) extends NamingConfigured(mapping) {
  override def tableSQLToModule(name: QualifiedName): String =
    name.lastPart match {
      case "SUPPLIERS" => "Supps"
      case "COFFEES" => "Coffs"
      case _ => super.tableSQLToModule(name)
    }

  override def tableSQLToCase(name: QualifiedName): String =
    name.lastPart match {
      case "COFFEES" => "Coff"
      case _ => super.tableSQLToCase(name)
    }

  override def columnSQLToCaseField(name: QualifiedName): String =
    super.columnSQLToCaseField(name)

  override def columnSQLToModuleField(name: QualifiedName): String =
    super.columnSQLToModuleField(name)

}
