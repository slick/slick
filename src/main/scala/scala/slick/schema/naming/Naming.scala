package scala.slick.schema.naming

import scala.slick.schema.ForeignKey
import scala.slick.schema.Index
import scala.slick.schema.PrimaryKey
import scala.slick.schema.QualifiedName

trait Naming {
  def tableSQLToModule(name: QualifiedName): String
  def tableSQLToCase(name: QualifiedName): String
  def columnSQLToCaseField(name: QualifiedName): String
  def columnSQLToModuleField(name: QualifiedName): String

  def indexName(idx: Index): String = {
    indexName(idx.fields.map(_.moduleFieldName))
  }

  def indexName(columnNames: List[String]): String = {
    "idx_" + columnNames.mkString("_")
  }

  def foreignKeyName(fk: ForeignKey): String = {
    "fk" + fk.pkTableName.lastPart
  }

  def primaryKeyName(pk: PrimaryKey): String = {
    "pk" + pk.fields.map(_.moduleFieldName).mkString
  }
}
