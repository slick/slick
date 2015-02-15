package scala.slick.jdbc.meta

import scala.util._
import scala.slick.dbio._
import scala.slick.jdbc.{ResultSetAction, Invoker}
import scala.slick.profile.BasicStreamingAction
import scala.concurrent.ExecutionContext
import scala.slick.util.Logging

/** A wrapper for a row in the ResultSet returned by DatabaseMetaData.getTables(). */
case class MTable(
  name: MQName,
  tableType: String,
  remarks: String,
  typeName: Option[MQName],
  selfRefColName: Option[String],
  refGen: Option[String]
) extends Logging{
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
  def getExtended(implicit ec: ExecutionContext)
   : DBIOAction[MTableExtended,NoStream,Effect.Read]
   = {
    for{
      cols <- getColumns
      pks <- getPrimaryKeys
      ifk <- getImportedKeys
      _idx <- getIndexInfo().asTry
    } yield {
      val idx = _idx match {
        case Success(i) => i
        case Failure(e: java.sql.SQLException) => // TODO: this needs a test!
          logger.debug(s"Skipping indices of table ${name.name} due to exception during getIndexInfo: "+e.getMessage.trim)
          Seq()
        case Failure(e) => throw e
      }
      MTableExtended(
        name, tableType, remarks, typeName, selfRefColName, refGen,
        cols, pks, idx, ifk
      )
    }
  }
}

/**
Like MTable but with additional meta data already preloaded.
*/
case class MTableExtended(
  name: MQName,
  tableType: String,
  remarks: String,
  typeName: Option[MQName],
  selfRefColName: Option[String],
  refGen: Option[String],
  columns: Seq[MColumn],
  primaryKeys: Seq[MPrimaryKey],
  indexInfo: Seq[MIndexInfo],
  importedKeys: Seq[MForeignKey]
)

object MTable {
  def getTables(cat: Option[String], schemaPattern: Option[String], namePattern: Option[String],
    types: Option[Seq[String]]) = ResultSetAction[MTable](
      _.metaData.getTables(cat.orNull, schemaPattern.orNull, namePattern.orNull, types.map(_.toArray).orNull) ) { r =>
      if(r.numColumns > 5) MTable(MQName.from(r), r.<<, r.<<, MQName.optionalFrom(r), r.<<, r.<<)
      else MTable(MQName.from(r), r.<<, r.<<, None, None, None)
  }
  def getTables(namePattern: String): BasicStreamingAction[Effect.Read, Vector[MTable], MTable] = getTables(Some(""), Some(""), Some(namePattern), None)
  def getTables: BasicStreamingAction[Effect.Read, Vector[MTable], MTable] = getTables(Some(""), Some(""), None, None)

  def getExtendedTables(
    cat: Option[String], schemaPattern: Option[String], namePattern: Option[String], types: Option[Seq[String]]
  )(implicit ec: ExecutionContext) = getTables(
      cat, schemaPattern, namePattern, types
    ).flatMap(t => DBIO.sequence(t.map(_.getExtended)))

  def getExtendedTables(namePattern: String)(implicit ec: ExecutionContext)
    = getTables(namePattern).flatMap(t => DBIO.sequence(t.map(_.getExtended)))

  def getExtendedTables(implicit ec: ExecutionContext)
    = getTables.flatMap(t => DBIO.sequence(t.map(_.getExtended)))
}
