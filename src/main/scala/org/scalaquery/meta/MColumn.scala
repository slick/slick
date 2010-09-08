package org.scalaquery.meta

import java.sql._
import org.scalaquery.{ResultSetInvoker, UnitInvoker}
import org.scalaquery.ql.TypeMapperDelegate

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getColumns().
 */
case class MColumn(
  table: MQName, column: String, sqlType: Int, typeName: String,
  columnSize: Option[Int], decimalDigits: Option[Int], numPrecRadix: Int, nullable: Option[Boolean], remarks: Option[String],
  columnDef: Option[String], charOctetLength: Int, ordinalPos: Int, isNullable: Option[Boolean], scope: Option[MQName],
  sourceDataType: Option[Int], isAutoInc: Option[Boolean]) {

  def sqlTypeName = TypeMapperDelegate.typeNames.get(sqlType)
  def getColumnPrivileges = MColumnPrivilege.getColumnPrivileges(table, column)
}

object MColumn {
  def getColumns(tablePattern: MQName, columnPattern: String) = ResultSetInvoker[MColumn](
      _.metaData.getColumns(tablePattern.catalog_?, tablePattern.schema_?, tablePattern.name, columnPattern)) { r =>
      MColumn(MQName.from(r), r<<, r<<, r<<, r<<, r.skip<<, r<<, r.nextInt match {
          case DatabaseMetaData.columnNoNulls => Some(false)
          case DatabaseMetaData.columnNullable => Some(true)
          case _ => None
        }, r<<, r<<, r.skip.skip<<, r<<, DatabaseMeta.yesNoOpt(r), MQName.optionalFrom(r), r<<, DatabaseMeta.yesNoOpt(r))
  }
}
