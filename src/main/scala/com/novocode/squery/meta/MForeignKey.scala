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
  pkTable: MQName, pkColumn: String, fkTable: MQName, fkColumn: String,
  keySeq: Short, updateRule: ForeignKeyAction, deleteRule: ForeignKeyAction,
  fkName: String, pkName: String, deferrability: Short)

object MForeignKey {

  def getImportedKeys(table: MQName): UnitInvoker[MForeignKey] =
    createInvoker(_.conn.getMetaData().getImportedKeys(table.catalog.getOrElse(null), table.schema.getOrElse(null), table.name))

  def getImportedKeys(table: String): UnitInvoker[MForeignKey] = getImportedKeys(MQName.local(table))

  def getExportedKeys(table: MQName): UnitInvoker[MForeignKey] =
    createInvoker(_.conn.getMetaData().getExportedKeys(table.catalog.getOrElse(null), table.schema.getOrElse(null), table.name))

  def getExportedKeys(table: String): UnitInvoker[MForeignKey] = getExportedKeys(MQName.local(table))

  def getCrossReference(parentTable: MQName, foreignTable: MQName): UnitInvoker[MForeignKey] =
    createInvoker(_.conn.getMetaData().getCrossReference(
      parentTable.catalog.getOrElse(null), parentTable.schema.getOrElse(null), parentTable.name,
      foreignTable.catalog.getOrElse(null), foreignTable.schema.getOrElse(null), foreignTable.name))

  def getCrossReference(parentTable: String, foreignTable: String): UnitInvoker[MForeignKey] =
    getCrossReference(MQName.local(parentTable), MQName.local(foreignTable))

  private[this] def createInvoker(f: Session => ResultSet) =
    ResultSetInvoker[MForeignKey](f) { r =>
      MForeignKey(MQName.from(r), r.nextString, MQName.from(r), r.nextString,
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
