package scala.slick.jdbc.meta

import scala.slick.jdbc.{ResultSetInvoker, UnitInvoker}

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
  /** @param unique when true, return only indices for unique values; when false, return indices regardless of whether unique or not */
  def getIndexInfo(unique: Boolean = false, approximate: Boolean = false) =
    MIndexInfo.getIndexInfo(name, unique, approximate)
}

object MTable {
  def getTables(cat: Option[String], schemaPattern: Option[String], namePattern: Option[String],
    types: Option[Seq[String]]) = ResultSetInvoker[MTable](
      _.metaData.getTables(cat.orNull, schemaPattern.orNull, namePattern.orNull, types.map(_.toArray).orNull) ) { r =>
      if(r.numColumns > 5) MTable(MQName.from(r), r.<<, r.<<, MQName.optionalFrom(r), r.<<, r.<<)
      else MTable(MQName.from(r), r.<<, r.<<, None, None, None)
  }
  def getTables(namePattern: String): UnitInvoker[MTable] = getTables(Some(""), Some(""), Some(namePattern), None)
  def getTables: UnitInvoker[MTable] = getTables(Some(""), Some(""), None, None)
}
