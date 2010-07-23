package com.novocode.squery.meta

import java.sql._
import com.novocode.squery.{ResultSetInvoker, UnitInvoker}
import com.novocode.squery.session._
import com.novocode.squery.simple.Implicit._

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getTables().
 */
case class MTable(
  cat: Option[String], schema: Option[String], name: String, typ: String, remarks: String, typeCat: Option[String],
  typeSchema: Option[String], typeName: Option[String], selfRefColName: Option[String], refGen: Option[String]) {
  def getColumns = MColumn.getColumns(cat, schema, name, "%")
  def getPrimaryKeys = MPrimaryKey.getPrimaryKeys(cat, schema, name)
  def getImportedKeys = MForeignKey.getImportedKeys(cat, schema, name)
  def getExportedKeys = MForeignKey.getExportedKeys(cat, schema, name)
}

object MTable {
  def getTables(cat: Option[String], schemaPattern: Option[String], namePattern: Option[String],
    types: Option[Seq[String]]): UnitInvoker[MTable] =
    ResultSetInvoker[MTable](
      _.conn.getMetaData().getTables(cat.getOrElse(null), schemaPattern.getOrElse(null),
                                     namePattern.getOrElse(null), types.map(_.toArray).getOrElse(null)) ) { r =>
      MTable(r.nextStringOption, r.nextStringOption, r.nextString, r.nextString, r.nextString, r.nextStringOption,
        r.nextStringOption, r.nextStringOption, r.nextStringOption, r.nextStringOption)
  }
  def getTables(namePattern: String): UnitInvoker[MTable] = getTables(Some(""), Some(""), Some(namePattern), None)
  def getTables: UnitInvoker[MTable] = getTables(Some(""), Some(""), None, None)
}
