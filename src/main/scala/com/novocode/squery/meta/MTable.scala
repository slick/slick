package com.novocode.squery.meta

import java.sql._
import com.novocode.squery.{ResultSetInvoker, UnitInvoker}
import com.novocode.squery.session._
import com.novocode.squery.simple.Implicit._

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getTables().
 */
case class MTable(
  name: MQName, tableType: String, remarks: String, typeName: Option[MQName],
  selfRefColName: Option[String], refGen: Option[String]) {
  def getColumns = MColumn.getColumns(name, "%")
  def getPrimaryKeys = MPrimaryKey.getPrimaryKeys(name)
  def getImportedKeys = MForeignKey.getImportedKeys(name)
  def getExportedKeys = MForeignKey.getExportedKeys(name)
  def getVersionColumns = MVersionColumn.getVersionColumns(name)
  def getTablePrivileges = MTablePrivilege.getTablePrivileges(name)
  def getBestRowIdentifier(scope: MBestRowIdentifierColumn.Scope, nullable: Boolean = false) =
    MBestRowIdentifierColumn.getBestRowIdentifier(name, scope, nullable)
  def getIndexInfo(unique: Boolean = false, approximate: Boolean = false) =
    MIndexInfo.getIndexInfo(name, unique, approximate)
}

object MTable {
  def getTables(cat: Option[String], schemaPattern: Option[String], namePattern: Option[String],
    types: Option[Seq[String]]): UnitInvoker[MTable] =
    ResultSetInvoker[MTable](
      _.conn.getMetaData().getTables(cat.getOrElse(null), schemaPattern.getOrElse(null),
                                     namePattern.getOrElse(null), types.map(_.toArray).getOrElse(null)) ) { r =>
      MTable(MQName.from(r), r.nextString, r.nextString, MQName.optionalFrom(r), r.nextStringOption, r.nextStringOption)
  }
  def getTables(namePattern: String): UnitInvoker[MTable] = getTables(Some(""), Some(""), Some(namePattern), None)
  def getTables: UnitInvoker[MTable] = getTables(Some(""), Some(""), None, None)
}
