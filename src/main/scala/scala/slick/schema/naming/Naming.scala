package scala.slick.schema.naming

import scala.slick.jdbc.meta.CodeGen
import scala.slick.SlickException
import scala.slick.schema.Index
import com.typesafe.config.Config
import scala.slick.schema.ForeignKey
import scala.slick.schema.PrimaryKey

trait Naming {
  def tableSQLToModule(table: String): String
  def tableSQLToCase(table: String): String
  def columnSQLToCaseField(table: String)(column: String): String
  def columnSQLToModuleField(table: String)(column: String): String

  def indexName(idx: Index): String = {
    indexName(idx.fields.map(_.moduleFieldName))
  }

  def indexName(columnNames: List[String]): String = {
    "idx_" + columnNames.mkString("_")
  }

  def foreignKeyName(fk: ForeignKey): String = {
    "fk" + fk.pkTableName
  }

  def primaryKeyName(pk: PrimaryKey): String = {
    "pk" + pk.fields.map(_.moduleFieldName).mkString
  }
}
