package scala.slick.schema

import scala.slick.jdbc.meta.CodeGen

object Naming {
  def tableSQLToModule(tableSQL: String): String = {
    tableSQL.toLowerCase.capitalize
  }

  def moduleToCaseClass(module: String): String = {
    if (module.endsWith("s"))
      module.dropRight(1)
    else
      "C" + module
  }

  def columnSQLToField(columnSQL: String): String = {
    CodeGen.mkScalaName(columnSQL, false)
  }

  def indexName(idx: Index): String = {
    indexName(idx.fields.map(_.scalaName))
  }

  def indexName(columnNames: List[String]): String = {
    "idx_" + columnNames.mkString("_")
  }
}
