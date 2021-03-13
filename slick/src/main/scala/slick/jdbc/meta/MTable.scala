package slick.jdbc.meta

import slick.dbio.Effect
import slick.jdbc.ResultSetAction
import slick.basic.BasicStreamingAction

/** A wrapper for a row in the ResultSet returned by DatabaseMetaData.getTables(). */
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
  /**
    * Some DatabaseMetaData methods take arguments that are String patterns. These arguments all have names such as fooPattern.
    * Within a pattern String, "%" means match any substring of 0 or more characters, and "_" means match any one character.
    * Only metadata entries matching the search pattern are returned.
    * If a search pattern argument is set to null, that argument's criterion will be dropped from the search.
    */
  def getTables(cat: Option[String], schemaPattern: Option[String], namePattern: Option[String],
    types: Option[Seq[String]]) = ResultSetAction[MTable](
      _.metaData.getTables(cat.orNull, schemaPattern.orNull, namePattern.orNull, types.map(_.toArray).orNull) ) { r =>
      if(r.numColumns > 5) MTable(MQName.from(r), r.<<, r.<<, MQName.optionalFrom(r), r.<<, r.<<)
      else MTable(MQName.from(r), r.<<, r.<<, None, None, None)
  }
  def getTables(namePattern: String): BasicStreamingAction[Vector[MTable], MTable, Effect.Read] = getTables(Some(""), Some(""), Some(namePattern), None)
  def getTables: BasicStreamingAction[Vector[MTable], MTable, Effect.Read] = getTables(Some(""), Some(""), None, None)
}
