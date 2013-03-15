package scala.slick.schema.naming

import scala.slick.jdbc.meta.CodeGen
import scala.slick.SlickException
import scala.slick.schema.Index
import com.typesafe.config.Config

trait Naming {
  def tableSQLToModule(table: String): String
  def tableSQLToCase(table: String): String
  def columnSQLToCaseField(table: String)(column: String): String
  def columnSQLToModuleField(table: String)(column: String): String
  
  def indexName(idx: Index): String = {
    indexName(idx.fields.map(_.scalaName))
  }

  def indexName(columnNames: List[String]): String = {
    "idx_" + columnNames.mkString("_")
  }
}
