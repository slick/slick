package com.novocode.squery.meta

import java.sql._
import com.novocode.squery.{ResultSetInvoker, UnitInvoker}
import com.novocode.squery.combinator.ForeignKeyAction
import com.novocode.squery.session._
import com.novocode.squery.simple.Implicit._

/**
 * A wrapper for a row in the ResultSet returned by
 * DatabaseMetaData.getImportedKeys/getExportedKeys/getCrossReference().
 */
case class MForeignKey(
  pktableCat: Option[String], pktableSchema: Option[String], pktable: String, pkcolumn: String,
  fktableCat: Option[String], fktableSchema: Option[String], fktable: String, fkcolumn: String,
  keySeq: Short, updateRule: ForeignKeyAction, deleteRule: ForeignKeyAction,
  fkName: String, pkName: String, deferrability: Short)

object MForeignKey {

  def getImportedKeys(cat: Option[String], schema: Option[String], table: String): UnitInvoker[MForeignKey] =
    createInvoker(_.conn.getMetaData().getImportedKeys(cat.getOrElse(null), schema.getOrElse(null), table))

  def getImportedKeys(table: String): UnitInvoker[MForeignKey] = getImportedKeys(Some(""), Some(""), table)

  def getExportedKeys(cat: Option[String], schema: Option[String], table: String): UnitInvoker[MForeignKey] =
    createInvoker(_.conn.getMetaData().getExportedKeys(cat.getOrElse(null), schema.getOrElse(null), table))

  def getExportedKeys(table: String): UnitInvoker[MForeignKey] = getExportedKeys(Some(""), Some(""), table)

  def getCrossReference(parentCat: Option[String], parentSchema: Option[String], parentTable: String,
    foreignCat: Option[String], foreignSchema: Option[String], foreignTable: String): UnitInvoker[MForeignKey] =
    createInvoker(_.conn.getMetaData().getCrossReference(
      parentCat.getOrElse(null), parentSchema.getOrElse(null), parentTable,
      foreignCat.getOrElse(null), foreignSchema.getOrElse(null), foreignTable))

  def getCrossReference(parentTable: String, foreignTable: String): UnitInvoker[MForeignKey] =
    getCrossReference(Some(""), Some(""), parentTable, Some(""), Some(""), foreignTable)

  private[this] def createInvoker(f: Session => ResultSet) =
    ResultSetInvoker[MForeignKey](f) { r =>
      MForeignKey(r.nextStringOption, r.nextStringOption, r.nextString, r.nextString,
        r.nextStringOption, r.nextStringOption, r.nextString, r.nextString,
        r.nextShort, fkActionFor(r.nextShort), fkActionFor(r.nextShort),
        r.nextString, r.nextString, r.nextShort)
  }

  private[this] def fkActionFor(v: Short) = v match {
    case DatabaseMetaData.importedKeyNoAction => ForeignKeyAction.NoAction
    case DatabaseMetaData.importedKeyCascade => ForeignKeyAction.Cascade
    case DatabaseMetaData.importedKeySetNull => ForeignKeyAction.SetNull
    case DatabaseMetaData.importedKeySetDefault => ForeignKeyAction.SetDefault
    case DatabaseMetaData.importedKeyRestrict => ForeignKeyAction.Restrict
  }
}
